# Install the R packages the apps need on a server, without renv.
#
# Only packages that are missing, or older than a version some package
# requires, are installed. Packages that are already installed and recent
# enough are left alone. Then ofarena and arenaveg are installed from this
# repository.
#
# Run from the repository root, as the owner of the target library (the first
# entry of .libPaths(), e.g. /usr/local/lib/R/site-library as root):
#
#   Rscript deploy/install_packages.R --dry-run   # only list what would be installed
#   sudo Rscript deploy/install_packages.R
#
# Packages come from getOption("repos"). For precompiled Linux binaries, point
# it to Posit Package Manager, e.g. for Ubuntu 22.04:
#   options(repos = c(CRAN = "https://packagemanager.posit.co/cran/__linux__/jammy/latest"))
# in /usr/lib/R/etc/Rprofile.site, or pass --repos=<url>.

args <- commandArgs(trailingOnly = TRUE)
dry_run <- "--dry-run" %in% args
repos <- sub("^--repos=", "", grep("^--repos=", args, value = TRUE))
if (length(repos) == 1) options(repos = c(CRAN = repos))
if (is.null(getOption("repos")) || identical(getOption("repos")[["CRAN"]], "@CRAN@")) {
  options(repos = c(CRAN = "https://cloud.r-project.org"))
}
if (!file.exists("ofarena/DESCRIPTION") || !file.exists("arenaveg/DESCRIPTION")) {
  stop("Run this from the repository root (the folder containing ofarena/ and arenaveg/).")
}

# "pkg (>= 1.0)" entries of Depends/Imports/LinkingTo -> data frame of name, minimum version.
parse_deps <- function(fields) {
  fields <- as.vector(fields)
  deps <- trimws(unlist(strsplit(paste(fields[!is.na(fields)], collapse = ","), ",")))
  deps <- deps[nzchar(deps)]
  data.frame(
    pkg = trimws(sub("\\(.*", "", deps)),
    min = ifelse(grepl(">=", deps), trimws(sub(".*>=\\s*([^)]+)\\).*", "\\1", deps)), NA),
    stringsAsFactors = FALSE
  )
}
dep_fields <- c("Depends", "Imports", "LinkingTo")

base <- rownames(utils::installed.packages(priority = "base"))
installed <- utils::installed.packages()
installed <- installed[!duplicated(installed[, "Package"]), , drop = FALSE]
rownames(installed) <- installed[, "Package"]
available <- utils::available.packages()

own <- c("ofarena", "arenaveg")
queue <- do.call(rbind, lapply(own, function(p) parse_deps(read.dcf(file.path(p, "DESCRIPTION"), dep_fields))))
required <- list()
to_install <- character()
seen <- character()

# Walk the dependency tree. An installed package that is kept contributes its own
# requirements; one that gets installed contributes those of the repository version.
while (nrow(queue) > 0) {
  d <- queue[1, ]
  queue <- queue[-1, ]
  if (d$pkg %in% c("R", base, own)) {
    if (d$pkg == "R" && !is.na(d$min) && getRversion() < d$min) {
      stop(sprintf("R >= %s is required, this is R %s.", d$min, getRversion()))
    }
    next
  }
  # Highest minimum version any package asks for, as text.
  if (!is.na(d$min) && (is.null(required[[d$pkg]]) || package_version(d$min) > package_version(required[[d$pkg]]))) {
    required[[d$pkg]] <- d$min
  }

  have <- if (d$pkg %in% rownames(installed)) package_version(installed[d$pkg, "Version"])
  need <- required[[d$pkg]]
  outdated <- is.null(have) || (!is.null(need) && have < package_version(need))
  if (outdated && !d$pkg %in% to_install) {
    if (!d$pkg %in% rownames(available)) stop("Package not found in the repositories: ", d$pkg)
    to_install <- c(to_install, d$pkg)
    seen <- setdiff(seen, d$pkg)
  }
  if (d$pkg %in% seen) next
  seen <- c(seen, d$pkg)
  fields <- if (d$pkg %in% to_install) available[d$pkg, dep_fields] else installed[d$pkg, dep_fields]
  queue <- rbind(queue, parse_deps(fields))
}

# A repository frozen at an old date (e.g. rocker images) may not offer new enough versions.
too_old <- Filter(function(p) !is.null(required[[p]]) && package_version(available[p, "Version"]) < package_version(required[[p]]), to_install)
if (length(too_old) > 0) {
  stop("The repository ", getOption("repos")[["CRAN"]], " only offers older versions than required: ",
       paste(sprintf("%s %s (needs >= %s)", too_old, available[too_old, "Version"],
                     unlist(required[too_old])), collapse = ", "),
       ". Use a current repository, e.g. --repos=https://packagemanager.posit.co/cran/__linux__/jammy/latest")
}

cat("R", as.character(getRversion()), "- library:", .libPaths()[1], "\n")
if (length(to_install) == 0) {
  cat("All required packages are installed.\n")
} else {
  cat("To install or update (", length(to_install), "): ",
      paste(sort(to_install), collapse = ", "), "\n", sep = "")
}
if (dry_run) quit(save = "no")

if (length(to_install) > 0) {
  utils::install.packages(to_install, dependencies = FALSE)
  failed <- to_install[!vapply(to_install, requireNamespace, TRUE, quietly = TRUE)]
  if (length(failed) > 0) {
    stop("Not installed: ", paste(failed, collapse = ", "),
         ". Usually a system library is missing; see the messages above and README.md.")
  }
}
utils::install.packages(file.path(".", own), repos = NULL, type = "source")
suppressPackageStartupMessages(library(arenaveg))
cat("Done: ofarena", as.character(utils::packageVersion("ofarena")),
    "and arenaveg", as.character(utils::packageVersion("arenaveg")), "installed.\n")

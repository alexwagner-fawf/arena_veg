#' Access files in the current app
#'
#' @param ... Path components below `inst/`.
#' @noRd
app_sys <- function(...) {
  system.file(..., package = "arenaveg")
}

#' Read the app configuration
#'
#' The active section comes from `GOLEM_CONFIG_ACTIVE` or `R_CONFIG_ACTIVE`
#' (`default` if unset; `production` on the server).
#'
#' @param value Value to retrieve.
#' @noRd
get_golem_config <- function(value,
                             config = Sys.getenv("GOLEM_CONFIG_ACTIVE", Sys.getenv("R_CONFIG_ACTIVE", "default")),
                             use_parent = TRUE,
                             file = app_sys("golem-config.yml")) {
  config::get(value = value, config = config, file = file, use_parent = use_parent)
}

#' The application user interface
#'
#' @param request Internal parameter for `{shiny}`.
#' @noRd
app_ui <- function(request) {
  bslib::page_navbar(
    id = "nav",
    title = "Verj\u00fcngungsaufnahme",
    window_title = "Verj\u00fcngungsaufnahme \u2013 FAWF",
    lang = "de",
    theme = bslib::bs_theme(version = 5, primary = "#2e6b3a"),
    navbar_options = bslib::navbar_options(bg = "#2e6b3a", theme = "dark"),
    header = golem_add_external_resources(),
    sidebar = bslib::sidebar(title = "Auswahl", width = 290, mod_filters_ui("filters")),
    fillable = "karte",
    bslib::nav_panel("Karte", value = "karte", icon = shiny::icon("map"), mod_karte_ui("karte", rasters = length(app_opt("raster_roots")) > 0)),
    bslib::nav_panel("Fotos", value = "fotos", icon = shiny::icon("images"), mod_fotos_ui("fotos")),
    bslib::nav_panel("Datenqualit\u00e4t", value = "qa", icon = shiny::icon("list-check"), mod_qa_ui("qa")),
    bslib::nav_panel("Info", value = "info", icon = shiny::icon("circle-info"), mod_info_ui("info"))
  )
}

#' Add external resources (CSS and JS in inst/app/www)
#'
#' Not golem::bundle_resources(): it links the CSS without a version, so
#' browsers kept an old custom.css next to new HTML and JS (photo viewer
#' cut off, dropdown covered).
#' @noRd
golem_add_external_resources <- function(path = app_sys("app/www")) {
  golem::add_resource_path("www", path)
  url <- function(f) paste0("www/", f, "?v=", resources_version(path))
  htmltools::tags$head(
    lapply(list.files(path, "\\.css$"), function(f) htmltools::tags$link(rel = "stylesheet", href = url(f))),
    lapply(list.files(path, "\\.js$"), function(f) htmltools::tags$script(src = url(f)))
  )
}

#' Version of the CSS/JS files, derived from their content
#'
#' The files are served without cache headers (only Last-Modified), so
#' browsers may keep an old copy. A new version after every change gives
#' them a new URL.
#' @noRd
resources_version <- function(path = app_sys("app/www")) {
  hashes <- tools::md5sum(sort(list.files(path, full.names = TRUE)))
  paste0(utils::packageVersion("arenaveg"), ".", sum(strtoi(substr(hashes, 1, 6), 16L)) %% 1e8)
}

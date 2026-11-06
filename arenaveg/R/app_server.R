#' The application server-side
#'
#' @param input,output,session Internal parameters for `{shiny}`.
#' @noRd
app_server <- function(input, output, session) {
  survey_ids <- as.integer(app_opt("veg_surveys"))
  ttl <- as.numeric(app_opt("data_ttl_min"))
  geometry <- list(
    crs_metric = as.integer(app_opt("metric_crs")),
    pk_radius_m = as.numeric(app_opt("pk_radius_m")),
    pk_centre = app_opt("pk_centre")
  )
  load <- function(force = FALSE) {
    do.call(veg_data, c(list(pool, survey_ids, ttl, force = force), geometry))
  }

  pool <- tryCatch(app_pool(), error = identity)
  if (inherits(pool, "error")) {
    return(show_fatal("Keine Verbindung zur Datenbank", conditionMessage(pool)))
  }
  access <- ofarena::arena_check_access(pool, survey_ids)
  if (!all(access$ok)) {
    return(show_fatal("Fehlende Leserechte", access_problem(access[!access$ok, ])))
  }
  initial <- tryCatch(load(), error = identity)
  if (inherits(initial, "error")) {
    return(show_fatal("Daten konnten nicht gelesen werden", conditionMessage(initial)))
  }
  dataset <- shiny::reactiveVal(initial)

  positions <- tryCatch(app_positions(app_opt("positions_gpkg"), geometry$crs_metric), error = identity)
  if (inherits(positions, "error")) {
    return(show_fatal("Korrigierte Lagen konnten nicht gelesen werden", conditionMessage(positions)))
  }

  filters <- mod_filters_server("filters", dataset)
  shiny::observeEvent(filters$reload(), ignoreInit = TRUE, {
    dataset(load(force = TRUE))
    shiny::showNotification("Daten neu geladen.", type = "message")
  })
  filtered <- shiny::reactive(veg_filter(dataset(), filters$filter()))

  cache_dir <- app_opt("cache_dir")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  url_fun <- ofarena::arena_file_server(pool, cache_dir, session)

  selection <- shiny::reactiveValues(pk = NULL, verjk = NULL, nonce = NULL)
  goto <- function(pk, verjk = NULL) {
    selection$pk <- pk
    selection$verjk <- verjk
    selection$nonce <- stats::runif(1)
    bslib::nav_select("nav", "karte")
  }

  raster_roots <- unlist(app_opt("raster_roots"))
  raster_cache_dir <- app_opt("raster_cache_dir")
  if (length(raster_roots) > 0) dir.create(raster_cache_dir, recursive = TRUE, showWarnings = FALSE)
  mod_karte_server("karte", filtered, filters$species, selection, url_fun, positions, raster_roots, raster_cache_dir)
  fotos_goto <- mod_fotos_server("fotos", filtered, url_fun)
  shiny::observeEvent(fotos_goto(), goto(fotos_goto()$pk, fotos_goto()$verjk))
  qa_goto <- mod_qa_server("qa", filtered, as.numeric(app_opt("max_gps_offset_m")))
  shiny::observeEvent(qa_goto(), goto(qa_goto()$pk))
  mod_info_server("info", dataset, access)
}

show_fatal <- function(title, body) {
  shiny::showModal(shiny::modalDialog(title = title, body, footer = NULL, easyClose = FALSE))
  invisible()
}

# One pool per R process, shared by all sessions; closed when the process ends.
app_state <- new.env(parent = emptyenv())

app_pool <- function() {
  if (is.null(app_state$pool) || !pool::dbIsValid(app_state$pool)) {
    app_state$pool <- ofarena::arena_pool()
    reg.finalizer(app_state, function(e) if (!is.null(e$pool)) pool::poolClose(e$pool), onexit = TRUE)
  }
  app_state$pool
}

# Read once per R process and shared by all sessions, so everyone sees a new
# position at once. A relative path starts in the app's working directory.
app_positions <- function(path, crs) {
  if (is.null(app_state$positions)) {
    app_state$positions <- position_store(file.path(normalizePath(dirname(path)), basename(path)), crs)
  }
  app_state$positions
}

# golem options given to run_app() override golem-config.yml.
app_opt <- function(name) {
  golem::get_golem_options(name) %||% get_golem_config(name)
}

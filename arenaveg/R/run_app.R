#' Run the vegetation app
#'
#' Database settings come from the `ARENA_DB_*` environment variables (see
#' [ofarena::arena_pool()]).
#'
#' @param ... Values overriding `golem-config.yml`, e.g. `veg_surveys = 33`.
#' @param onStart,options,enableBookmarking,uiPattern See [shiny::shinyApp()].
#' @export
run_app <- function(onStart = NULL, options = list(), enableBookmarking = NULL,
                    uiPattern = "/", ...) {
  golem::with_golem_options(
    app = shiny::shinyApp(
      ui = app_ui,
      server = app_server,
      onStart = onStart,
      options = options,
      enableBookmarking = enableBookmarking,
      uiPattern = uiPattern
    ),
    golem_opts = list(...)
  )
}

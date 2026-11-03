#' Info page: data state, surveys, access check
#' @noRd
mod_info_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(6, 6),
    bslib::card(bslib::card_header("Daten"), bslib::card_body(shiny::uiOutput(ns("data")))),
    bslib::card(bslib::card_header("Datenbankzugriff"), bslib::card_body(shiny::uiOutput(ns("access"))))
  )
}

#' @param data Reactive with the unfiltered data.
#' @param access Result of [ofarena::arena_check_access()].
#' @noRd
mod_info_server <- function(id, data, access) {
  shiny::moduleServer(id, function(input, output, session) {
    output$data <- shiny::renderUI({
      d <- data()
      s <- d$surveys
      cycles <- vapply(s$cycles, function(c) paste(sprintf("%d: %s", as.integer(c$cycle) + 1, format(c$date_start, "%Y")), collapse = ", "), "")
      n <- c(
        "Probekreis-Jahre" = nrow(d$pk), "Verj\u00fcngungskreise" = nrow(d$verjk),
        "Bodendecker erw." = nrow(d$bd_ext), "Bodendecker" = nrow(d$bd), "Verj\u00fcngung" = nrow(d$verj),
        "Losung" = nrow(d$losung), "Fotos" = nrow(d$photos), "Horste mit Umriss" = nrow(d$horst_sf)
      )
      htmltools::tagList(
        htmltools::p("Geladen: ", format(d$loaded_at, "%d.%m.%Y %H:%M")),
        htmltools::tags$table(
          class = "table table-sm",
          htmltools::tags$thead(htmltools::tags$tr(lapply(c("Survey", "Name", "Titel", "Zyklen (Start)"), htmltools::tags$th))),
          htmltools::tags$tbody(lapply(seq_len(nrow(s)), function(i) htmltools::tags$tr(
            htmltools::tags$td(s$id[i]), htmltools::tags$td(htmltools::tags$code(s$name[i])),
            htmltools::tags$td(s$label[i]), htmltools::tags$td(cycles[i])
          )))
        ),
        definition_list(n)
      )
    })

    output$access <- shiny::renderUI({
      missing <- access[!access$ok, ]
      htmltools::tagList(
        htmltools::p(sprintf("Benutzer %s auf %s:%s/%s", Sys.getenv("ARENA_DB_USER", "geouser"),
                             Sys.getenv("ARENA_DB_HOST", "localhost"), Sys.getenv("ARENA_DB_PORT", "5432"),
                             Sys.getenv("ARENA_DB_NAME", "arena"))),
        if (nrow(missing) == 0) {
          htmltools::p(class = "text-success", sprintf("Alle %d ben\u00f6tigten Objekte lesbar.", nrow(access)))
        } else {
          access_problem(missing)
        }
      )
    })
  })
}

access_problem <- function(missing) {
  htmltools::tagList(
    htmltools::p(class = "text-danger", sprintf(
      "Dem Datenbank-Benutzer fehlen Leserechte auf %d Objekte. Bitte sql/geouser_grants.sql auf dem Server ausf\u00fchren lassen (z. B. nach dem erneuten Ver\u00f6ffentlichen eines Surveys in Arena).",
      nrow(missing)
    )),
    htmltools::tags$ul(lapply(utils::head(paste(missing$privilege, "auf", missing$object), 15), htmltools::tags$li))
  )
}

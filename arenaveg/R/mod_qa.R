qa_colours <- c(ok = "success", warnung = "warning", fehler = "danger", fehlt = "secondary")

#' Data quality page: completeness grid (Versuchsfläche × Probekreis × year)
#' and the list of issues
#' @noRd
mod_qa_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    shiny::uiOutput(ns("summary")),
    bslib::card(
      bslib::card_header("Vollst\u00e4ndigkeit je Probekreis und Jahr",
                         htmltools::span(class = "small text-muted ms-2", "Zelle anklicken: Probekreis auf der Karte")),
      bslib::card_body(reactable::reactableOutput(ns("grid")))
    ),
    bslib::card(
      bslib::card_header("Hinweise und Fehler"),
      bslib::card_body(reactable::reactableOutput(ns("issues")))
    )
  )
}

#' @return Reactive `list(pk, n)` when a cell was clicked.
#' @noRd
mod_qa_server <- function(id, filtered, max_gps_offset_m = 20) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    qa <- shiny::reactive(veg_qa(filtered(), max_gps_offset_m))

    output$summary <- shiny::renderUI({
      counts <- table(qa()$status)
      boxes <- lapply(qa_levels, function(s) {
        bslib::value_box(
          title = qa_labels[[s]], value = as.integer(counts[[s]]),
          theme = qa_colours[[s]], showcase = NULL
        )
      })
      do.call(bslib::layout_columns, c(list(fill = FALSE), boxes))
    })

    output$grid <- reactable::renderReactable({
      q <- qa()
      q <- q[q$in_grid, ]
      shiny::validate(shiny::need(nrow(q) > 0, "Keine Probekreise in der Auswahl."))
      q$cell <- seq_len(nrow(q))
      # Duplicates share a cell; the worst status is shown.
      q <- q[order(q$status, decreasing = TRUE), ]
      wide <- q |>
        dplyr::distinct(vf, pk_nr, jahr, .keep_all = TRUE) |>
        dplyr::select(vf, pk_nr, jahr, cell) |>
        tidyr::pivot_wider(names_from = jahr, values_from = cell, names_sort = TRUE) |>
        dplyr::arrange(vf, natural_key(pk_nr))
      q <- q[order(q$cell), ]
      years <- setdiff(names(wide), c("vf", "pk_nr"))

      year_cols <- lapply(years, function(y) reactable::colDef(
        name = y, align = "center", minWidth = 120,
        # HTML strings rather than tags: reactable turns tags into React
        # elements, which cannot carry inline onclick handlers.
        cell = function(value) if (is.na(value)) "" else qa_cell(q[value, ], ns("open")),
        html = TRUE
      ))
      names(year_cols) <- years
      reactable::reactable(
        wide,
        columns = c(list(
          vf = reactable::colDef(name = "Versuchsfl\u00e4che", minWidth = 160),
          pk_nr = reactable::colDef(name = "Probekreis", minWidth = 90)
        ), year_cols),
        groupBy = "vf", defaultExpanded = TRUE, pagination = FALSE, compact = TRUE,
        bordered = TRUE, sortable = FALSE, language = reactable::reactableLang(noData = "Keine Daten")
      )
    })

    output$issues <- reactable::renderReactable({
      q <- qa()
      q <- q[q$status != "ok", c("vf", "pk_nr", "jahr", "status", "issues")]
      q$status <- unname(qa_labels[as.character(q$status)])
      reactable::reactable(
        q, filterable = TRUE, compact = TRUE, striped = TRUE, defaultPageSize = 20,
        columns = list(
          vf = reactable::colDef(name = "Versuchsfl\u00e4che"),
          pk_nr = reactable::colDef(name = "Probekreis", maxWidth = 110),
          jahr = reactable::colDef(name = "Jahr", maxWidth = 80),
          status = reactable::colDef(name = "Status", maxWidth = 150),
          issues = reactable::colDef(name = "Befund", minWidth = 320)
        ),
        language = reactable::reactableLang(noData = "Keine Befunde", filterPlaceholder = "Filter")
      )
    })

    goto <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$open, goto(list(pk = input$open$pk, n = input$open$n)))
    goto
  })
}

qa_cell <- function(r, input_id) {
  status <- as.character(r$status)
  if (status == "fehlt") {
    return('<span class="badge w-100 qa-cell text-bg-secondary" title="nicht aufgenommen">fehlt</span>')
  }
  text <- sprintf("%d VK \u00b7 %d BD \u00b7 %d Fotos", r$n_verjk, r$n_bd_ext + r$n_bd, r$n_photos)
  sprintf(
    '<button type="button" class="btn btn-sm w-100 qa-cell btn-%s" title="%s" onclick="Shiny.setInputValue(\'%s\', {pk: \'%s\', n: Math.random()})">%s</button>',
    qa_colours[[status]], htmltools::htmlEscape(r$issues, attribute = TRUE), input_id, r$pk_uuid, text
  )
}

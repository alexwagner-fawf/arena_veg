#' Photo gallery module
#'
#' A paged thumbnail grid. Clicking a thumbnail opens the photo in a modal
#' with its caption and details, and a button to show it on the map.
#'
#' @param id Module id.
#' @export
mod_gallery_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::tagList(
    htmltools::tags$style(htmltools::HTML("
      .arena-gallery { display: grid; grid-template-columns: repeat(auto-fill, minmax(180px, 1fr)); gap: .75rem; }
      .arena-gallery figure { margin: 0; cursor: pointer; }
      .arena-gallery img { width: 100%; aspect-ratio: 4 / 3; object-fit: cover; border-radius: .375rem;
                           background: var(--bs-tertiary-bg); }
      .arena-gallery figcaption { font-size: .8rem; line-height: 1.2; margin-top: .25rem; }
      .arena-gallery img:hover { outline: 3px solid var(--bs-primary); }
    ")),
    shiny::uiOutput(ns("pager")),
    shiny::uiOutput(ns("grid"))
  )
}

#' @rdname mod_gallery_ui
#' @param photos Reactive tibble with columns `survey_id`, `file_uuid`,
#'   `caption` and optionally `details` (character, shown in the modal) and
#'   `key` (passed back by the map button).
#' @param url_fun URL builder from [arena_file_server()].
#' @param page_size Photos per page.
#' @return A reactive `list(key, n)`: `key` of the photo whose "Auf Karte
#'   zeigen" button was pressed last, `n` counts presses (so pressing it twice
#'   for the same photo still triggers observers). `NULL` before the first press.
#' @export
mod_gallery_server <- function(id, photos, url_fun, page_size = 24) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns
    page <- shiny::reactiveVal(1)
    n_pages <- shiny::reactive(max(1, ceiling(nrow(photos()) / page_size)))
    shiny::observeEvent(photos(), page(1))
    shiny::observeEvent(input$prev, page(max(1, page() - 1)))
    shiny::observeEvent(input$`next`, page(min(n_pages(), page() + 1)))

    current <- shiny::reactive({
      rows <- seq((page() - 1) * page_size + 1, length.out = page_size)
      photos()[rows[rows <= nrow(photos())], ]
    })

    output$pager <- shiny::renderUI({
      htmltools::div(
        class = "d-flex align-items-center gap-2 mb-2",
        shiny::actionButton(ns("prev"), "\u2039 Zur\u00fcck", class = "btn-sm", disabled = page() <= 1),
        htmltools::span(sprintf("Seite %d von %d \u00b7 %d Fotos", page(), n_pages(), nrow(photos()))),
        shiny::actionButton(ns("next"), "Weiter \u203a", class = "btn-sm", disabled = page() >= n_pages())
      )
    })

    output$grid <- shiny::renderUI({
      p <- current()
      if (nrow(p) == 0) return(htmltools::p(class = "text-muted", "Keine Fotos f\u00fcr die aktuelle Auswahl."))
      htmltools::div(
        class = "arena-gallery",
        lapply(seq_len(nrow(p)), function(i) {
          htmltools::tags$figure(
            onclick = sprintf("Shiny.setInputValue('%s', %d, {priority: 'event'})", ns("open"), i),
            htmltools::tags$img(src = url_fun(p$survey_id[i], p$file_uuid[i], thumb = TRUE),
                                loading = "lazy", alt = p$caption[i]),
            htmltools::tags$figcaption(p$caption[i])
          )
        })
      )
    })

    shown <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$open, {
      photo <- current()[input$open, ]
      shown(photo)
      full <- url_fun(photo$survey_id, photo$file_uuid)
      shiny::showModal(shiny::modalDialog(
        title = photo$caption,
        size = "xl",
        easyClose = TRUE,
        htmltools::tags$a(href = full, target = "_blank",
                          htmltools::tags$img(src = full, style = "width:100%;max-height:75vh;object-fit:contain;border-radius:.375rem;")),
        if (!is.null(photo$details)) htmltools::p(class = "mt-2 small", photo$details),
        footer = htmltools::tagList(
          if (!is.null(photo$key)) shiny::actionButton(ns("goto"), "Auf Karte zeigen", class = "btn-primary"),
          shiny::modalButton("Schlie\u00dfen")
        )
      ))
    })

    goto <- shiny::reactiveVal(NULL)
    shiny::observeEvent(input$goto, {
      shiny::removeModal()
      goto(list(key = shown()$key, n = input$goto))
    })
    goto
  })
}

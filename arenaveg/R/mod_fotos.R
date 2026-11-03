#' Photo page: gallery of all photos of the filtered plots
#' @noRd
mod_fotos_ui <- function(id) {
  ns <- shiny::NS(id)
  bslib::card(
    bslib::card_header(
      shiny::checkboxGroupInput(ns("types"), NULL, choices = unname(photo_types),
                                selected = unname(photo_types), inline = TRUE)
    ),
    bslib::card_body(ofarena::mod_gallery_ui(ns("gallery")))
  )
}

#' @return Reactive `list(pk, verjk, n)` when a photo's "Auf Karte zeigen" was
#'   pressed.
#' @noRd
mod_fotos_server <- function(id, filtered, url_fun) {
  shiny::moduleServer(id, function(input, output, session) {
    photos <- shiny::reactive({
      p <- filtered()$photos
      p <- p[p$type %in% input$types, ]
      p <- p[order(p$vf, p$pk_label, match(p$type, photo_types)), ]
      tibble::tibble(
        survey_id = p$source_survey,
        file_uuid = p$file_uuid,
        caption = sprintf("%s \u00b7 %s", p$type, p$pk_label),
        details = ifelse(is.na(p$detail), p$file_name, paste0(p$detail, " \u00b7 ", p$file_name)),
        key = paste(p$pk_uuid, p$verjk_uuid, sep = "|")
      )
    })
    goto <- ofarena::mod_gallery_server("gallery", photos, url_fun)
    shiny::reactive({
      g <- goto()
      if (is.null(g)) return(NULL)
      parts <- strsplit(g$key, "|", fixed = TRUE)[[1]]
      list(pk = parts[1], verjk = if (length(parts) > 1 && parts[2] != "NA") parts[2], n = g$n)
    })
  })
}

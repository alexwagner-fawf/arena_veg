#' Large photo with rotation in 90 degree steps
#'
#' Shows a photo as large as the visible panel allows (the large copy from
#' [ofarena::arena_file_fetch()], not the original), always in full, with
#' buttons to rotate it and a link to the original. With several photos, a
#' dropdown switches between them. Switching and rotating happen in the
#' browser (`www/viewer.js`), which remembers each photo's rotation while the
#' page is open.
#'
#' @param photos Data frame with `source_survey`, `file_uuid`, `type` and
#'   `detail`, the photo to show first in the first row.
#' @param url_fun File URL builder ([ofarena::arena_file_server()]).
#' @param context Text to drop from the start of `detail`, e.g. the circle's
#'   label when all photos belong to that circle.
#' @noRd
photo_viewer <- function(photos, url_fun, context = NULL) {
  detail <- photos$detail
  if (!is.null(context) && !is.na(context)) {
    detail <- substring(detail, ifelse(startsWith(detail, context), nchar(context) + 1, 1))
    detail <- sub("^ · ", "", detail)
    detail[detail == ""] <- NA
  }
  caption <- ifelse(is.na(detail), photos$type, paste(photos$type, detail, sep = " · "))
  large <- url_fun(photos$source_survey, photos$file_uuid, large = TRUE)
  original <- url_fun(photos$source_survey, photos$file_uuid)
  button <- function(dir, icon, title) {
    htmltools::tags$button(
      type = "button", class = "btn btn-sm btn-outline-secondary", title = title,
      onclick = sprintf("vegViewer.rotate(this, %d)", dir), shiny::icon(icon)
    )
  }
  chooser <- if (nrow(photos) > 1) {
    htmltools::tags$select(
      class = "form-select form-select-sm veg-viewer-select", title = "Foto wählen",
      `aria-label` = "Foto wählen", onchange = "vegViewer.show(this)",
      lapply(seq_len(nrow(photos)), function(i) htmltools::tags$option(
        value = i, `data-key` = photos$file_uuid[i], `data-src` = large[i], `data-href` = original[i],
        sprintf("%d/%d · %s", i, nrow(photos), caption[i])
      ))
    )
  } else {
    htmltools::span(class = "small text-muted me-auto", caption[1])
  }

  htmltools::div(
    class = "veg-viewer",
    htmltools::div(
      class = "d-flex align-items-center gap-1 mb-2",
      chooser,
      button(-1, "rotate-left", "90° nach links drehen"),
      button(1, "rotate-right", "90° nach rechts drehen"),
      htmltools::tags$a(class = "veg-viewer-original btn btn-sm btn-outline-secondary", href = original[1],
                        target = "_blank", title = "Original in neuem Tab öffnen",
                        shiny::icon("up-right-from-square"))
    ),
    htmltools::div(
      class = "veg-viewer-stage",
      htmltools::tags$img(class = "veg-viewer-img", src = large[1], alt = caption[1],
                          `data-key` = photos$file_uuid[1], onload = "vegViewer.fit(this)")
    )
  )
}

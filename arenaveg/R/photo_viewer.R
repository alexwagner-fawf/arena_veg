#' Large photo with rotation in 90 degree steps
#'
#' Shows the first photo large (the large copy from [ofarena::arena_file_fetch()],
#' not the original) with buttons to rotate it and a link to the original.
#' Further photos are shown as thumbnails below; clicking one shows it large.
#' Switching and rotating happen in the browser (`www/viewer.js`), which
#' remembers each photo's rotation while the page is open.
#'
#' @param photos Data frame with `source_survey`, `file_uuid`, `type` and
#'   `detail`, the photo to show first in the first row.
#' @param url_fun File URL builder ([ofarena::arena_file_server()]).
#' @noRd
photo_viewer <- function(photos, url_fun) {
  caption <- ifelse(is.na(photos$detail), photos$type, paste(photos$type, photos$detail, sep = " · "))
  large <- url_fun(photos$source_survey, photos$file_uuid, large = TRUE)
  original <- url_fun(photos$source_survey, photos$file_uuid)
  button <- function(dir, icon, title) {
    htmltools::tags$button(
      type = "button", class = "btn btn-sm btn-outline-secondary", title = title,
      onclick = sprintf("vegViewer.rotate(this, %d)", dir), shiny::icon(icon)
    )
  }

  htmltools::div(
    class = "veg-viewer",
    htmltools::div(
      class = "d-flex align-items-center gap-1 mb-2",
      htmltools::span(class = "veg-viewer-caption small text-muted me-auto", caption[1]),
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
    ),
    if (nrow(photos) > 1) {
      htmltools::div(class = "veg-viewer-thumbs", lapply(seq_len(nrow(photos)), function(i) {
        htmltools::tags$img(
          src = url_fun(photos$source_survey[i], photos$file_uuid[i], thumb = TRUE), loading = "lazy",
          class = if (i == 1) "active", title = caption[i], alt = caption[i],
          `data-key` = photos$file_uuid[i], `data-src` = large[i], `data-href` = original[i],
          `data-caption` = caption[i], onclick = "vegViewer.show(this)"
        )
      }))
    }
  )
}

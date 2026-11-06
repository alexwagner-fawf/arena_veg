#' Proposed true positions of the regeneration circles
#'
#' Set on the map by comparing the circle's photo with a GeoTIFF, and kept in
#' a local GeoPackage because the Arena database is read-only for the app.
#' Layer `verjk_position` has one row per circle: the point is the proposed
#' centre, `orig_x`/`orig_y` the centre in Arena at that time, `offset_m` the
#' distance between them, `photo_uuid` and `rotation_deg` the photo shown when
#' saving and how far it was turned (clockwise, 0/90/180/270).
#'
#' @name verjk_position
#' @noRd
NULL

position_layer <- "verjk_position"

position_empty <- function(crs) {
  sf::st_sf(
    verjk_uuid = NA_character_, pk_uuid = NA_character_, source_survey = NA_integer_,
    pk_label = NA_character_, verjk_label = NA_character_, orig_x = NA_real_, orig_y = NA_real_,
    offset_m = NA_real_, photo_uuid = NA_character_, rotation_deg = NA_integer_,
    edited_at = as.POSIXct(NA, tz = "UTC"), geometry = sf::st_sfc(sf::st_point(), crs = crs)
  )[0, ]
}

position_read <- function(path, crs) {
  if (!file.exists(path)) return(position_empty(crs))
  x <- sf::st_read(path, layer = position_layer, quiet = TRUE, stringsAsFactors = FALSE)
  # GeoPackages call the geometry column "geom".
  sf::st_transform(sf::st_sf(sf::st_drop_geometry(x), geometry = sf::st_geometry(x)), crs)
}

# Replaces the file in one step, so a crash while writing cannot damage the
# saved positions and a reader never sees half a file.
position_write <- function(x, path) {
  folder <- dirname(path)
  if (file.access(folder, 2) != 0) stop("Ordner nicht beschreibbar: ", folder, call. = FALSE)
  tmp <- tempfile("verjk_position", tmpdir = folder, fileext = ".gpkg")
  on.exit(unlink(tmp), add = TRUE)
  sf::st_write(x, tmp, layer = position_layer, quiet = TRUE)
  if (!file.rename(tmp, path)) stop("Datei nicht ersetzbar: ", path, call. = FALSE)
  invisible(x)
}

#' Positions on disk, with a reactive copy for the sessions
#' @return A list with `data` (reactive: all positions), `crs`, `save(row)`
#'   (adds or replaces the circle's row) and `remove(verjk_uuid)`.
#' @noRd
position_store <- function(path, crs) {
  data <- shiny::reactiveVal(position_read(path, crs))
  update <- function(change) {
    # Starts from the file, not `data`: it may have changed since startup.
    x <- change(position_read(path, crs))
    position_write(x, path)
    data(x)
  }
  list(
    data = data, crs = crs,
    save = function(row) update(function(x) rbind(x[x$verjk_uuid != row$verjk_uuid, ], row)),
    remove = function(verjk_uuid) update(function(x) x[x$verjk_uuid != verjk_uuid, ])
  )
}

circle_centres <- function(verjk_sf, crs) {
  sf::st_centroid(sf::st_transform(sf::st_geometry(verjk_sf), crs))
}

#' Row for `position_store()$save()`
#' @param verjk One row of `verjk_sf`.
#' @param point New centre (`sfc`, any CRS).
#' @noRd
position_row <- function(verjk, point, crs) {
  point <- sf::st_transform(point, crs)
  from <- sf::st_coordinates(circle_centres(verjk, crs))
  to <- sf::st_coordinates(point)
  sf::st_sf(
    verjk_uuid = verjk$verjk_uuid, pk_uuid = verjk$pk_uuid, source_survey = as.integer(verjk$source_survey),
    pk_label = verjk$pk_label, verjk_label = verjk$verjk_label, orig_x = from[[1, "X"]], orig_y = from[[1, "Y"]],
    offset_m = sqrt(sum((to - from)^2)), photo_uuid = NA_character_, rotation_deg = NA_integer_,
    edited_at = as.POSIXct(NA, tz = "UTC"), geometry = point
  )
}

#' Geometries for the map
#'
#' A new circle of the original's size around each position, and a line from
#' the original centre to the new one. Positions of circles not in `verjk_sf`
#' (filtered out) are left out.
#'
#' @param draft Unsaved `position_row()`, shown instead of the circle's saved
#'   position; `is_draft` marks it.
#' @return A list with the `sf` tables `circles` and `lines`.
#' @noRd
position_features <- function(positions, verjk_sf, draft = NULL) {
  crs <- sf::st_crs(positions)
  positions$is_draft <- rep(FALSE, nrow(positions))
  if (!is.null(draft)) {
    draft$is_draft <- TRUE
    positions <- rbind(positions[positions$verjk_uuid != draft$verjk_uuid, ], draft)
  }
  verjk_sf <- verjk_sf[!sf::st_is_empty(verjk_sf), ]
  positions <- positions[positions$verjk_uuid %in% verjk_sf$verjk_uuid, ]
  v <- verjk_sf[match(positions$verjk_uuid, verjk_sf$verjk_uuid), ]

  to <- sf::st_geometry(positions)
  from <- circle_centres(v, crs)
  lines <- lapply(seq_along(to), function(i) {
    sf::st_linestring(rbind(sf::st_coordinates(from[i]), sf::st_coordinates(to[i])))
  })
  attrs <- sf::st_drop_geometry(positions)
  list(
    circles = sf::st_sf(attrs, geometry = ofarena::buffer_m(to, sqrt(as.numeric(v$verjk_groesse) / pi), crs_metric = crs)),
    lines = sf::st_sf(attrs, geometry = sf::st_sfc(lines, crs = crs))
  )
}

position_summary <- function(saved) {
  turned <- !is.na(saved$rotation_deg) && saved$rotation_deg != 0
  paste(c(
    sprintf("Korrigiert: %.2f m vom Original", saved$offset_m),
    if (turned) sprintf("Foto %d° gedreht", saved$rotation_deg),
    format(saved$edited_at, "%d.%m.%Y", tz = Sys.timezone())
  ), collapse = " · ")
}

#' Buttons above the photo for correcting the selected circle's position
#' @param ns Namespace function of the module.
#' @param saved The circle's saved position (0 or 1 rows).
#' @param placing Is the map waiting for clicks?
#' @param draft Unsaved `position_row()`, or `NULL`.
#' @param rotation_changed The photo is turned differently than saved.
#' @noRd
position_controls <- function(ns, saved, placing, draft, rotation_changed) {
  bar <- function(text, ...) {
    htmltools::div(class = "veg-position", htmltools::span(class = "small me-auto", text), ...)
  }
  button <- function(id, label, icon, class = "btn-outline-secondary") {
    shiny::actionButton(ns(id), label, icon = shiny::icon(icon), class = paste("btn-sm", class))
  }
  if (placing) {
    text <- if (is.null(draft)) {
      "Neuen Mittelpunkt auf der Karte anklicken."
    } else {
      sprintf("%.2f m vom Original. Erneut klicken zum Verschieben.", draft$offset_m)
    }
    return(bar(
      text,
      if (!is.null(draft)) button("position_save", "Speichern", "check", "btn-primary"),
      button("position_cancel", "Abbrechen", "xmark")
    ))
  }
  if (nrow(saved) == 0) {
    return(bar("Lage nicht korrigiert.", button("position_start", "Lage korrigieren", "location-crosshairs", "btn-outline-primary")))
  }
  bar(
    position_summary(saved),
    if (rotation_changed) button("position_rotation", "Drehung speichern", "rotate-right", "btn-primary"),
    button("position_start", "Ändern", "location-crosshairs"),
    button("position_delete", "Löschen", "trash-can")
  )
}

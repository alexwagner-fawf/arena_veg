# Entity tables of the vegetation survey (surveys 31 and 33), as named in the
# Arena data schema, with the columns the app relies on. Columns missing in
# older survey versions are added as NA, so both versions stack cleanly.
veg_tables <- list(
  rec = list(entity = "verjuengungsaufnahme", cols = c(
    "record_uuid", "record_cycle", "record_step", "note", "versuchsflaeche",
    "versuchsflaeche_label", "probekreisnummer", "probekreisnummer_label"
  )),
  pk = list(entity = "probekreis", cols = c(
    "panoramafoto_file_uuid", "panoramafoto_file_name", "hauptaufnahmejahr", "curr_position_x",
    "curr_position_y", "curr_position_srs", "dateneintragung_durch"
  )),
  pers = list(entity = "aufnehmende_personen", cols = "aufnehmende_personen"),
  verjk = list(entity = "tab_verjk", cols = c(
    "gon", "gab", "verjk_groesse", "verjuengungshorst", "gezauent", "gezauent_label",
    "bodendecker_extended", "verjk_bild_file_uuid", "verjk_bild_file_name", "gon_horst",
    "gab_horst", "verjh_bild_file_uuid", "verjh_bild_file_name", "verjk_bild_horizontal_file_uuid",
    "verjk_bild_horizontal_file_name", paste0("gab_horst_", seq(50, 400, 50))
  )),
  bd_ext = list(entity = "tab_bodendecker_extended", cols = c(
    "manual_id", "bodendecker_ext_group", "bodendecker_ext_group_label", "bodendecker_ext_art",
    "bodendecker_ext_art_label", "cover_bodendecker_ext", "cover_funct_group_perc",
    "identification_certainty", "identification_certainty_label", "kommentar",
    "foto_bodendecker_ext_file_uuid", "foto_bodendecker_ext_file_name", "aktiv_filter"
  )),
  bd = list(entity = "tab_bodendecker", cols = c(
    "bodendecker", "bodendecker_label", "bodendecker_art", "bodendecker_art_label",
    "bodendecker_deckungsgrad"
  )),
  verj = list(entity = "tab_verj", cols = c(
    "art", "art_label", "wuchsstufe", "wuchsstufe_label", "anzahl", "schaden", "schaden_label",
    "verjuengung_photo_file_uuid", "verjuengung_photo_file_name"
  )),
  losung = list(entity = "tab_losung", cols = c("tierart", "tierart_label", "anzahl_losung", "bemerkung_losung")),
  losung_img = list(entity = "losung_img", cols = c("losung_img_file_uuid", "losung_img_file_name"))
)

# Photo attributes, in display order, with their German type names.
photo_types <- c(
  panoramafoto = "Panorama",
  verjk_bild = "Verj\u00fcngungskreis",
  verjk_bild_horizontal = "Verjk horizontal",
  verjh_bild = "Horst",
  foto_bodendecker_ext = "Bodendecker erw.",
  verjuengung_photo = "Verj\u00fcngung",
  losung_img = "Losung"
)

base_cols <- c("source_survey", "id", "uuid", "parent_uuid")

# Stack the tables of several survey versions. Columns whose type differs
# between versions (e.g. an attribute changed from text to code) become text.
bind_surveys <- function(dfs) {
  dfs <- Filter(Negate(is.null), dfs)
  cols <- unique(unlist(lapply(dfs, names)))
  for (col in cols) {
    classes <- unique(unlist(lapply(dfs, function(df) if (col %in% names(df)) class(df[[col]])[1])))
    if (length(classes) > 1 && !all(classes %in% c("integer", "numeric"))) {
      dfs <- lapply(dfs, function(df) {
        if (col %in% names(df)) df[[col]] <- as.character(df[[col]])
        df
      })
    }
  }
  dplyr::bind_rows(dfs)
}

ensure_cols <- function(df, cols) {
  if (is.null(df) || ncol(df) == 0) {
    df <- tibble::tibble(source_survey = integer(), id = numeric(), uuid = character(), parent_uuid = character())
  }
  for (col in setdiff(cols, names(df))) df[[col]] <- rep(NA, nrow(df))
  df
}

#' Read the vegetation survey from the database
#'
#' @param con A DBI connection or pool.
#' @param survey_ids Arena survey ids, oldest first.
#' @param ... Geometry options passed on to `veg_assemble()`.
#' @return See `veg_assemble()`.
#' @noRd
veg_load <- function(con, survey_ids, ...) {
  raw <- lapply(veg_tables, function(t) {
    bind_surveys(lapply(survey_ids, function(id) ofarena::arena_read_entity(con, id, t$entity)))
  })
  surveys <- ofarena::arena_surveys(con)
  surveys <- surveys[surveys$id %in% survey_ids, ]
  # Newest survey first, so its labels win for attributes present in both.
  defs <- lapply(rev(survey_ids), function(id) tibble::add_column(ofarena::arena_node_defs(con, id), source_survey = id))
  locations <- dplyr::bind_rows(lapply(defs, function(d) veg_locations(con, d)))
  veg_assemble(raw, surveys, dplyr::bind_rows(defs), locations, ...)
}

#' Plot locations
#'
#' The plot number is a code attribute whose category items carry the true
#' plot centre as a point property (in survey 33: category
#' `sampling_point_data`, property `location`). Arena's data tables do not
#' include it, so it is read from the category.
#'
#' @param con A DBI connection or pool.
#' @param defs Node definitions of one survey, with `source_survey`.
#' @return A tibble with `source_survey`, `versuchsflaeche`, `probekreisnummer`,
#'   `location_x`, `location_y`, `location_srs`, or `NULL` if there is none.
#' @noRd
veg_locations <- function(con, defs) {
  category <- defs$category_uuid[defs$name == "probekreisnummer"][1]
  if (is.na(category)) return(NULL)
  items <- ofarena::arena_category_items(con, defs$source_survey[1], category)
  point <- sub("_srs$", "", grep("_srs$", names(items), value = TRUE))[1]
  if (is.na(point)) return(NULL)
  plots <- items[!is.na(items$parent_code), ]
  tibble::tibble(
    source_survey = defs$source_survey[1],
    versuchsflaeche = plots$parent_code,
    probekreisnummer = plots$code,
    location_x = plots[[paste0(point, "_x")]],
    location_y = plots[[paste0(point, "_y")]],
    location_srs = plots[[paste0(point, "_srs")]]
  )
}

#' Join the entity tables into the app's data model
#'
#' Every child table gets the keys of its plot (`pk_uuid`, Versuchsfläche,
#' Probekreis, year, record step and cycle) and, below the regeneration
#' circle, `verjk_uuid` and `verjk_label`.
#'
#' @param raw Named list of entity tables (names of `veg_tables`).
#' @param surveys Rows of [ofarena::arena_surveys()].
#' @param node_defs Node definitions of the surveys.
#' @param locations Plot locations (`veg_locations()`), or `NULL`.
#' @param crs_metric Metric CRS for derived geometries.
#' @param pk_radius_m Radius of the sample plot in metres.
#' @param pk_centre Plot centre for placing the circles: `"category"` (the
#'   true location from the plot category, else GPS) or `"gps"` (the position
#'   measured in the field, else the category location).
#' @return A list with the tables `pk`, `verjk`, `bd_ext`, `bd`, `verj`,
#'   `losung`, `photos`, the sf layers `pk_sf` (plot centres), `pk_area_sf`,
#'   `gps_sf`, `verjk_sf`, `horst_sf`, plus `surveys`, `labels` and
#'   `loaded_at`.
#' @noRd
veg_assemble <- function(raw, surveys, node_defs, locations = NULL, crs_metric = 25832,
                         pk_radius_m = 17.84, pk_centre = c("category", "gps")) {
  pk_centre <- match.arg(pk_centre)
  t <- lapply(names(veg_tables), function(n) ensure_cols(raw[[n]], c(base_cols, veg_tables[[n]]$cols)))
  names(t) <- names(veg_tables)

  rec <- t$rec |>
    dplyr::mutate(
      vf = dplyr::coalesce(versuchsflaeche_label, versuchsflaeche, "(ohne Fl\u00e4che)"),
      pk_nr = dplyr::coalesce(probekreisnummer_label, probekreisnummer, "(ohne Nr.)")
    )
  rec_keys <- c("record_uuid", "record_cycle", "record_step", "note", "versuchsflaeche",
                "versuchsflaeche_label", "probekreisnummer", "probekreisnummer_label", "vf", "pk_nr")

  personen <- t$pers |>
    dplyr::filter(!is.na(aufnehmende_personen)) |>
    dplyr::group_by(source_survey, uuid = parent_uuid) |>
    dplyr::summarise(aufnehmende_personen = paste(aufnehmende_personen, collapse = ", "), .groups = "drop")

  if (is.null(locations) || nrow(locations) == 0) {
    locations <- tibble::tibble(source_survey = integer(), versuchsflaeche = character(), probekreisnummer = character(),
                              location_x = numeric(), location_y = numeric(), location_srs = character())
  }
  locations <- locations[!duplicated(locations[c("source_survey", "versuchsflaeche", "probekreisnummer")]), ]

  pk <- ofarena::arena_join_parent(t$pk, rec, keep = rec_keys, parent_id = "rec_uuid") |>
    dplyr::left_join(personen, by = c("source_survey", "uuid")) |>
    dplyr::left_join(locations, by = c("source_survey", "versuchsflaeche", "probekreisnummer")) |>
    dplyr::mutate(
      pk_uuid = uuid,
      jahr = as.integer(hauptaufnahmejahr),
      pk_label = sprintf("%s \u00b7 PK %s \u00b7 %s", vf, pk_nr, ifelse(is.na(jahr), "?", jahr))
    ) |>
    dplyr::arrange(vf, natural_key(pk_nr), jahr)
  pk_keys <- c("pk_uuid", "vf", "pk_nr", "jahr", "pk_label", "record_step", "record_cycle")

  verjk <- ofarena::arena_join_parent(t$verjk, pk, keep = pk_keys) |>
    dplyr::group_by(source_survey, pk_uuid) |>
    dplyr::mutate(verjk_nr = dplyr::row_number(id)) |>
    dplyr::ungroup() |>
    dplyr::mutate(
      verjk_uuid = uuid,
      verjk_label = sprintf("VK %d (%s gon, %s m)", verjk_nr, format_num(gon), format_num(gab))
    )
  verjk_keys <- c(pk_keys, "verjk_uuid", "verjk_label")
  below_verjk <- function(df) ofarena::arena_join_parent(df, verjk, keep = verjk_keys)

  losung <- below_verjk(t$losung)
  losung_img <- ofarena::arena_join_parent(t$losung_img, losung, keep = c(verjk_keys, "tierart_label"))

  out <- list(
    pk = pk,
    verjk = verjk,
    bd_ext = below_verjk(t$bd_ext),
    bd = below_verjk(t$bd),
    verj = below_verjk(t$verj),
    losung = losung,
    surveys = surveys,
    loaded_at = Sys.time()
  )
  out$photos <- veg_photos(pk, verjk, out$bd_ext, out$verj, losung_img)
  geo <- veg_geometries(pk, verjk, crs_metric, pk_radius_m, pk_centre)
  out$pk <- geo$pk
  geo$pk <- NULL
  c(out, geo, list(labels = veg_labels(out, node_defs)))
}

format_num <- function(x) ifelse(is.na(x), "?", format(x, trim = TRUE, drop0trailing = TRUE))

# Sort key that orders embedded numbers by value: "Knopf_2" before "Knopf_105".
natural_key <- function(x) {
  x <- ifelse(is.na(x), "", x)
  m <- gregexpr("[0-9]+", x)
  regmatches(x, m) <- lapply(regmatches(x, m), function(d) formatC(as.numeric(d), width = 12, flag = "0", format = "f", digits = 0))
  x
}

# All photos in one table, with the plot/circle they belong to.
veg_photos <- function(pk, verjk, bd_ext, verj, losung_img) {
  collect <- function(df, detail) {
    files <- ofarena::arena_file_cols(df)
    if (nrow(files) == 0) return(NULL)
    ctx <- df[match(files$entity_uuid, df$uuid), ]
    files$pk_uuid <- ctx$pk_uuid
    files$verjk_uuid <- if ("verjk_uuid" %in% names(ctx)) ctx$verjk_uuid else NA_character_
    files$pk_label <- ctx$pk_label
    files$vf <- ctx$vf
    files$jahr <- ctx$jahr
    files$detail <- detail(ctx)
    files
  }
  photos <- dplyr::bind_rows(
    collect(pk, function(ctx) NA_character_),
    collect(verjk, function(ctx) ctx$verjk_label),
    collect(bd_ext, function(ctx) paste0(ctx$verjk_label, " \u00b7 ", ctx$bodendecker_ext_art_label)),
    collect(verj, function(ctx) paste0(ctx$verjk_label, " \u00b7 ", ctx$art_label, ", ", ctx$wuchsstufe_label)),
    collect(losung_img, function(ctx) paste0(ctx$verjk_label, " \u00b7 ", ctx$tierart_label))
  )
  if (is.null(photos) || nrow(photos) == 0) {
    return(tibble::tibble(source_survey = integer(), entity_uuid = character(), field = character(),
                          file_uuid = character(), file_name = character(), pk_uuid = character(),
                          verjk_uuid = character(), pk_label = character(), vf = character(),
                          jahr = integer(), detail = character(), type = character()))
  }
  photos$type <- unname(photo_types[photos$field])
  photos$type[is.na(photos$type)] <- photos$field[is.na(photos$type)]
  photos
}

# Plot centres and areas, GPS positions, regeneration circles (placed by
# bearing/distance from the plot centre; `verjk_groesse` is their area in m2)
# and patch polygons (8 rays around the patch centre, which is itself placed
# by bearing/distance from the plot centre). Adds `centre_source` and
# `gps_offset_m` to the plots.
veg_geometries <- function(pk, verjk, crs_metric, pk_radius_m = 17.84, pk_centre = "category") {
  location <- sf::st_geometry(ofarena::arena_points_sf(pk, "location", keep_empty = TRUE))
  gps <- sf::st_geometry(ofarena::arena_points_sf(pk, "curr_position", keep_empty = TRUE))
  has_location <- !sf::st_is_empty(location)
  has_gps <- !sf::st_is_empty(gps)
  use_location <- if (pk_centre == "category") has_location else has_location & !has_gps
  pk_geom <- gps
  pk_geom[use_location] <- location[use_location]
  pk$centre_source <- ifelse(use_location, "Kategorie", ifelse(has_gps, "GPS", NA_character_))
  pk$gps_offset_m <- NA_real_
  both <- has_location & has_gps
  if (any(both)) {
    pk$gps_offset_m[both] <- as.numeric(sf::st_distance(
      sf::st_transform(location[both], crs_metric), sf::st_transform(gps[both], crs_metric), by_element = TRUE
    ))
  }
  pk_sf <- sf::st_sf(pk, geometry = pk_geom)
  pk_area_sf <- sf::st_sf(pk, geometry = ofarena::buffer_m(pk_geom, pk_radius_m, crs_metric = crs_metric))
  gps_sf <- sf::st_sf(pk, geometry = gps)[has_gps, ]

  idx <- match(paste(verjk$source_survey, verjk$pk_uuid), paste(pk$source_survey, pk$pk_uuid))
  centre <- sf::st_sfc(lapply(idx, function(i) if (is.na(i)) sf::st_point() else pk_geom[[i]]), crs = 4326)

  verjk_centre <- ofarena::polar_offset(centre, verjk$gon, verjk$gab, crs_metric = crs_metric)
  radius <- sqrt(as.numeric(verjk$verjk_groesse) / pi)
  verjk_sf <- sf::st_sf(verjk, geometry = ofarena::buffer_m(verjk_centre, radius, crs_metric = crs_metric))

  horst_centre <- ofarena::polar_offset(centre, verjk$gon_horst, verjk$gab_horst, crs_metric = crs_metric)
  rays <- as.matrix(sf::st_drop_geometry(verjk)[paste0("gab_horst_", seq(50, 400, 50))])
  storage.mode(rays) <- "double"
  horst_sf <- sf::st_sf(verjk, geometry = ofarena::polygon_from_rays(horst_centre, seq(50, 400, 50), rays,
                                                                     crs_metric = crs_metric))
  list(pk = pk, pk_sf = pk_sf, pk_area_sf = pk_area_sf, gps_sf = gps_sf, verjk_sf = verjk_sf,
       horst_sf = horst_sf[!sf::st_is_empty(horst_sf), ])
}

veg_labels <- function(data, node_defs) {
  cols <- unique(unlist(lapply(data[c("pk", "verjk", "bd_ext", "bd", "verj", "losung")], names)))
  labels <- ofarena::arena_column_labels(cols, node_defs)
  extra <- c(
    source_survey = "Survey", vf = "Versuchsfl\u00e4che", pk_nr = "Probekreis", jahr = "Jahr",
    pk_label = "Probekreis", verjk_label = "Verj\u00fcngungskreis", verjk_nr = "Nr.",
    record_step = "Schritt", record_cycle = "Zyklus", aufnehmende_personen = "Aufnehmende Personen",
    verjk_groesse = "Fl\u00e4che (m\u00b2)"
  )
  labels[names(extra)[names(extra) %in% cols]] <- extra[names(extra) %in% cols]
  labels
}

# Process-wide cache shared by all sessions of the app.
veg_cache <- new.env(parent = emptyenv())

#' Vegetation data, cached for `ttl_min` minutes
#'
#' @param pool A DBI pool.
#' @param survey_ids Arena survey ids.
#' @param ttl_min Minutes after which the data is read again.
#' @param force Read again regardless of age.
#' @param ... Geometry options passed on to `veg_assemble()`.
#' @noRd
veg_data <- function(pool, survey_ids, ttl_min = 10, force = FALSE, ...) {
  key <- paste(c(survey_ids, unlist(list(...))), collapse = ",")
  hit <- veg_cache[[key]]
  fresh <- !is.null(hit) && difftime(Sys.time(), hit$loaded_at, units = "mins") < ttl_min
  if (force || !fresh) {
    veg_cache[[key]] <- veg_load(pool, survey_ids, ...)
  }
  veg_cache[[key]]
}

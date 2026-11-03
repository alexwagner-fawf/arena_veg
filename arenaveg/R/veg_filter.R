#' Apply the sidebar filters
#'
#' Plot-level filters (survey, Versuchsfläche, Probekreis, year, step, cycle)
#' select plots; the fence filter selects regeneration circles and keeps only
#' plots with at least one matching circle. `NULL` for Versuchsfläche,
#' Probekreis, year or fence means no restriction.
#'
#' @param d Data from `veg_assemble()`.
#' @param f Filter list with `surveys`, `vf`, `pk_nr`, `jahr`, `step`, `cycle`
#'   and `gezaeunt`.
#' @return `d` with all tables and layers restricted to the selection.
#' @noRd
veg_filter <- function(d, f) {
  any_of <- function(x, sel) if (length(sel) == 0) rep(TRUE, length(x)) else x %in% sel
  pk <- d$pk
  keep_pk <- pk$source_survey %in% f$surveys &
    any_of(pk$vf, f$vf) & any_of(pk$pk_nr, f$pk_nr) & any_of(as.character(pk$jahr), f$jahr) &
    pk$record_step %in% f$step & pk$record_cycle %in% f$cycle
  pk_ids <- pk$pk_uuid[keep_pk]

  verjk <- d$verjk
  keep_verjk <- verjk$pk_uuid %in% pk_ids & any_of(fence_label(verjk$gezauent_label), f$gezaeunt)
  verjk_ids <- verjk$verjk_uuid[keep_verjk]
  if (length(f$gezaeunt) > 0) pk_ids <- intersect(pk_ids, verjk$pk_uuid[keep_verjk])

  below <- function(df) df[df$verjk_uuid %in% verjk_ids, ]
  photos <- d$photos
  d$pk <- pk[pk$pk_uuid %in% pk_ids, ]
  d$verjk <- verjk[keep_verjk, ]
  d$bd_ext <- below(d$bd_ext)
  d$bd <- below(d$bd)
  d$verj <- below(d$verj)
  d$losung <- below(d$losung)
  d$photos <- photos[photos$pk_uuid %in% pk_ids & (is.na(photos$verjk_uuid) | photos$verjk_uuid %in% verjk_ids), ]
  d$pk_sf <- d$pk_sf[d$pk_sf$pk_uuid %in% pk_ids, ]
  d$pk_area_sf <- d$pk_area_sf[d$pk_area_sf$pk_uuid %in% pk_ids, ]
  d$gps_sf <- d$gps_sf[d$gps_sf$pk_uuid %in% pk_ids, ]
  d$verjk_sf <- d$verjk_sf[d$verjk_sf$verjk_uuid %in% verjk_ids, ]
  d$horst_sf <- d$horst_sf[d$horst_sf$verjk_uuid %in% verjk_ids, ]
  d
}

fence_label <- function(x) ifelse(is.na(x), "(keine Angabe)", x)

# Fenced circles: code 1 / label "ja" in survey 33.
is_fenced <- function(code, label) {
  code %in% "1" | tolower(label) %in% c("ja", "gez\u00e4unt")
}

#' Regeneration circles containing any of the given species
#'
#' Looks in the extended and the simple ground cover.
#'
#' @param d Data from `veg_assemble()`.
#' @param species Species labels.
#' @return Circle uuids.
#' @noRd
veg_species_circles <- function(d, species) {
  if (length(species) == 0) return(character())
  unique(c(
    d$bd_ext$verjk_uuid[species_name(d$bd_ext$bodendecker_ext_art_label, d$bd_ext$bodendecker_ext_art) %in% species],
    d$bd$verjk_uuid[species_name(d$bd$bodendecker_art_label, d$bd$bodendecker_art) %in% species]
  ))
}

species_name <- function(label, code) dplyr::coalesce(label, code)

#' All species of the ground cover, for the highlight selector
#' @noRd
veg_species_choices <- function(d) {
  df <- dplyr::bind_rows(
    tibble::tibble(group = d$bd_ext$bodendecker_ext_group_label,
                   art = species_name(d$bd_ext$bodendecker_ext_art_label, d$bd_ext$bodendecker_ext_art)),
    tibble::tibble(group = d$bd$bodendecker_label,
                   art = species_name(d$bd$bodendecker_art_label, d$bd$bodendecker_art))
  )
  df <- df[!is.na(df$art), ]
  df$group[is.na(df$group)] <- "Ohne Gruppe"
  df <- unique(df[order(df$group, df$art), ])
  lapply(split(df$art, df$group), function(x) as.list(unique(x)))
}

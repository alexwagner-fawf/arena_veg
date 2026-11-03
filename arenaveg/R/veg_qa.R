# Status levels of a plot-year in the completeness grid, worst last.
qa_levels <- c("ok", "warnung", "fehler", "fehlt")
qa_labels <- c(ok = "vollst\u00e4ndig", warnung = "Hinweise", fehler = "Fehler", fehlt = "nicht aufgenommen")

#' Completeness of each plot-year
#'
#' The expected plot-years are all Probekreise of a Versuchsfläche times all
#' years in which that Versuchsfläche was surveyed; plot-years without data
#' are `fehlt`. `fehler`: no position at all, no plot number, no regeneration
#' circle, or a survey year outside the years of the record's cycle.
#' `warnung`: circles without photo or without ground cover rows, GPS far
#' from the plot centre, duplicates. (Panoramas are not checked: they are
#' not taken every year.)
#'
#' @param d Data from `veg_assemble()` or `veg_filter()`.
#' @param max_gps_offset_m GPS positions further from the plot centre are flagged.
#' @return A tibble with one row per plot-year (more for duplicates): keys,
#'   `pk_uuid` (NA when missing), counts, `issues` (text), `status` and
#'   `in_grid` (FALSE for plot-years with an implausible year, which only
#'   appear in the issue list).
#' @noRd
veg_qa <- function(d, max_gps_offset_m = 20) {
  count_by <- function(df, col) {
    dplyr::count(df, key = .data[[col]], name = "n")
  }
  lookup <- function(counts, keys) {
    n <- counts$n[match(keys, counts$key)]
    ifelse(is.na(n), 0L, n)
  }

  verjk <- d$verjk
  verjk$n_ext <- lookup(count_by(d$bd_ext, "verjk_uuid"), verjk$verjk_uuid)
  verjk$n_verj <- lookup(count_by(d$verj, "verjk_uuid"), verjk$verjk_uuid)
  verjk$n_bd <- lookup(count_by(d$bd, "verjk_uuid"), verjk$verjk_uuid)
  verjk$n_losung <- lookup(count_by(d$losung, "verjk_uuid"), verjk$verjk_uuid)
  # Extended circles need extended ground cover rows, the others simple ones.
  extended <- tolower(verjk$bodendecker_extended) %in% "true"
  verjk$no_ground_cover <- ifelse(extended, verjk$n_ext == 0, verjk$n_bd == 0)
  per_pk <- verjk |>
    dplyr::group_by(pk_uuid) |>
    dplyr::summarise(
      n_verjk = dplyr::n(),
      n_no_ground_cover = sum(no_ground_cover),
      n_no_photo = sum(is.na(verjk_bild_file_uuid)),
      n_bd_ext = sum(n_ext),
      n_bd = sum(n_bd),
      n_verj = sum(n_verj),
      n_losung = sum(n_losung),
      .groups = "drop"
    )

  pk <- d$pk |>
    dplyr::transmute(
      vf, pk_nr, jahr, pk_uuid, source_survey, record_step, record_cycle, gps_offset_m,
      has_position = !is.na(centre_source),
      has_number = !is.na(probekreisnummer)
    ) |>
    dplyr::left_join(per_pk, by = "pk_uuid") |>
    dplyr::mutate(dplyr::across(dplyr::starts_with("n_"), ~ ifelse(is.na(.x), 0L, as.integer(.x))))
  pk$n_photos <- lookup(count_by(d$photos, "pk_uuid"), pk$pk_uuid)
  pk$n_duplicates <- stats::ave(seq_len(nrow(pk)), pk$vf, pk$pk_nr, pk$jahr, FUN = length)
  years <- cycle_years(d$surveys, pk$source_survey, pk$record_cycle)
  pk$year_ok <- is.na(years$from) | (!is.na(pk$jahr) & pk$jahr >= years$from & pk$jahr <= years$to)

  pk$issues <- vapply(seq_len(nrow(pk)), function(i) {
    r <- pk[i, ]
    paste(c(
      if (!r$year_ok) sprintf("Aufnahmejahr %s passt nicht zu Zyklus %d (%s)", ifelse(is.na(r$jahr), "fehlt", r$jahr),
                              as.integer(r$record_cycle) + 1, years$text[i]),
      if (!r$has_number) "keine Probekreisnummer",
      if (!r$has_position) "keine Position (weder Mittelpunkt noch GPS)",
      if (r$n_verjk == 0) "kein Verj\u00fcngungskreis",
      if (!is.na(r$gps_offset_m) && r$gps_offset_m > max_gps_offset_m) sprintf("GPS %.0f m vom Probekreismittelpunkt", r$gps_offset_m),
      if (r$n_no_photo > 0) sprintf("%d Verjk ohne Foto", r$n_no_photo),
      if (r$n_no_ground_cover > 0) sprintf("%d Verjk ohne Bodendecker-Eintr\u00e4ge", r$n_no_ground_cover),
      if (r$n_duplicates > 1) sprintf("%d-fach erfasst", r$n_duplicates)
    ), collapse = "; ")
  }, character(1))
  pk$status <- ifelse(!pk$has_position | !pk$has_number | pk$n_verjk == 0 | !pk$year_ok, "fehler",
                      ifelse(nzchar(pk$issues), "warnung", "ok"))
  pk$in_grid <- pk$year_ok

  expected <- pk[pk$in_grid, ] |>
    dplyr::distinct(vf, pk_nr, jahr) |>
    dplyr::group_by(vf) |>
    dplyr::reframe(tidyr::expand_grid(pk_nr = unique(pk_nr), jahr = unique(jahr)))
  grid <- dplyr::left_join(expected, pk[pk$in_grid, ], by = c("vf", "pk_nr", "jahr"))
  missing <- is.na(grid$pk_uuid)
  grid$status[missing] <- "fehlt"
  grid$issues[missing] <- "nicht aufgenommen"
  grid$in_grid[missing] <- TRUE
  out <- dplyr::bind_rows(grid, pk[!pk$in_grid, ])
  out$status <- factor(out$status, levels = qa_levels)
  dplyr::arrange(out, vf, natural_key(pk_nr), jahr)
}

# Years covered by the cycle of each record, from the survey's cycle dates
# (open-ended cycles run to the current year). NA where unknown.
cycle_years <- function(surveys, survey_id, cycle) {
  n <- length(survey_id)
  from <- to <- rep(NA_integer_, n)
  if ("cycles" %in% names(surveys)) {
    for (i in seq_len(nrow(surveys))) {
      c <- surveys$cycles[[i]]
      if (is.null(c) || nrow(c) == 0) next
      rows <- which(survey_id == surveys$id[i])
      k <- match(cycle[rows], c$cycle)
      from[rows] <- as.integer(format(c$date_start[k], "%Y"))
      to[rows] <- as.integer(format(dplyr::coalesce(c$date_end[k], Sys.Date()), "%Y"))
    }
  }
  list(from = from, to = to, text = ifelse(is.na(from), "", ifelse(from == to, from, paste0(from, "\u2013", to))))
}

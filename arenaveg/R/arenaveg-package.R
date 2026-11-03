#' @keywords internal
"_PACKAGE"

#' @importFrom rlang .data %||%
NULL

# Columns used in dplyr pipelines.
utils::globalVariables(c(
  "versuchsflaeche_label", "versuchsflaeche", "probekreisnummer_label", "probekreisnummer",
  "aufnehmende_personen", "source_survey", "parent_uuid", "uuid", "hauptaufnahmejahr", "vf", "pk_nr",
  "jahr", "pk_uuid", "id", "gon", "gab", "bodendecker_extended", "n_ext", "n_verj", "n_bd",
  "n_losung", "verjk_bild_file_uuid", "record_step", "curr_position_x", "curr_position_y",
  "panoramafoto_file_uuid", "cell", "verjk_nr", "no_ground_cover", "record_cycle", "gps_offset_m",
  "centre_source", "probekreisnummer"
))

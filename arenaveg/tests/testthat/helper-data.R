# Small in-memory version of both survey versions: survey 31 lacks the
# extended ground cover and some columns, survey 33 has them.
fake_raw <- function() {
  u <- function(prefix, n) sprintf("%s-%04d-0000-4000-8000-000000000000", prefix, seq_len(n))
  list(
    rec = tibble::tibble(
      source_survey = c(31L, 33L), id = c(1, 1), uuid = u("aaaaaaaa", 2), parent_uuid = NA_character_,
      record_uuid = u("rrrrrrrr", 2), record_cycle = c("0", "1"), record_step = c("3", "1"), note = NA,
      versuchsflaeche = "1", versuchsflaeche_label = "NWR Test", probekreisnummer = "101",
      probekreisnummer_label = "101"
    ),
    pk = tibble::tibble(
      source_survey = c(31L, 33L), id = c(1, 1), uuid = u("bbbbbbbb", 2), parent_uuid = u("aaaaaaaa", 2),
      panoramafoto_file_uuid = c(u("ffffffff", 1), NA), panoramafoto_file_name = c("p.jpg", NA),
      hauptaufnahmejahr = c(2024, 2025),
      curr_position_x = c(7.78, 7.78), curr_position_y = c(49.35, 49.35), curr_position_srs = "4326"
    ),
    pers = tibble::tibble(
      source_survey = 33L, id = 1:2, uuid = u("cccccccc", 2), parent_uuid = u("bbbbbbbb", 2)[2],
      aufnehmende_personen = c("A", "B")
    ),
    verjk = tibble::tibble(
      source_survey = c(31L, 33L, 33L), id = c(1, 1, 2), uuid = u("dddddddd", 3),
      parent_uuid = u("bbbbbbbb", 2)[c(1, 2, 2)],
      gon = c(0, 100, 200), gab = c(10, 5, 5), verjk_groesse = c(1, 2, 1.5),
      verjuengungshorst = c("false", "true", "false"), gezauent = "1", gezauent_label = "gezäunt",
      verjk_bild_file_uuid = NA_character_, verjk_bild_file_name = NA_character_,
      gon_horst = c(NA, 100, NA), gab_horst = c(NA, 5, NA),
      gab_horst_50 = c(NA, 2, NA), gab_horst_100 = c(NA, 2, NA), gab_horst_150 = c(NA, 2, NA),
      gab_horst_200 = c(NA, 2, NA), gab_horst_250 = c(NA, 2, NA), gab_horst_300 = c(NA, 2, NA),
      gab_horst_350 = c(NA, 2, NA), gab_horst_400 = c(NA, 2, NA),
      bodendecker_extended = c(NA, "true", "true")
    ),
    bd_ext = tibble::tibble(
      source_survey = 33L, id = 1:2, uuid = u("eeeeeeee", 2), parent_uuid = u("dddddddd", 3)[2],
      bodendecker_ext_art = c("vacmyr", "desfle"), bodendecker_ext_art_label = c("Vaccinium myrtillus", "Deschampsia flexuosa"),
      bodendecker_ext_group_label = c("Zwergsträucher", "Gräser"), cover_bodendecker_ext = c(10, 5)
    ),
    bd = tibble::tibble(
      source_survey = 31L, id = 1, uuid = u("gggggggg", 1), parent_uuid = u("dddddddd", 1),
      bodendecker_label = "Gräser", bodendecker_art = "desfle", bodendecker_deckungsgrad = 20
    ),
    verj = tibble::tibble(
      source_survey = c(31L, 33L), id = c(1, 1), uuid = u("hhhhhhhh", 2), parent_uuid = u("dddddddd", 3)[1:2],
      art_label = "Rotbuche", anzahl = c(3, 4)
    ),
    losung = NULL,
    losung_img = NULL
  )
}

fake_surveys <- function() {
  tibble::tibble(id = c(31L, 33L), name = c("v31", "v33"))
}

fake_node_defs <- function() {
  tibble::tibble(name = c("gon", "gab"), label = c("Richtung (gon)", "Abstand (m)"))
}

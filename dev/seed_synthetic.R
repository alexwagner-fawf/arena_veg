# Synthetic data for surveys 31 and 33 (vegetation / regeneration survey) in the
# local dev database, for developing the app before a real data dump is available.
#
#   Rscript dev/seed_synthetic.R
#
# Idempotent: removes earlier synthetic rows of surveys 31 and 33 first.
# Follows the conventions of the real survey 33: plot locations in the
# category `sampling_point_data` (extra property `location`), circles at 8 m
# with 10 m2, fence codes 0/1. Contains deliberate gaps for the QA view: a plot
# without GPS, a GPS reading 150 m off, a Verjk flagged for extended ground
# cover without rows, and a plot-year that was not surveyed.

library(DBI)

con <- dbConnect(RPostgres::Postgres(), host = "localhost", port = 55432, dbname = "arena",
                 user = "postgres", password = "postgres")
on.exit(dbDisconnect(con))
set.seed(20240601)

uuid <- function(n = 1) {
  vapply(seq_len(n), function(i) {
    h <- sprintf("%02x", sample(0:255, 16, replace = TRUE))
    h[7] <- sprintf("%02x", bitwOr(bitwAnd(strtoi(h[7], 16L), 0x0f), 0x40))
    h[9] <- sprintf("%02x", bitwOr(bitwAnd(strtoi(h[9], 16L), 0x3f), 0x80))
    paste0(paste(h[1:4], collapse = ""), "-", paste(h[5:6], collapse = ""), "-",
           paste(h[7:8], collapse = ""), "-", paste(h[9:10], collapse = ""), "-",
           paste(h[11:16], collapse = ""))
  }, character(1))
}
pick <- function(x, n = 1, ...) x[sample(length(x), n, ...)]
json <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null"))

# Append the columns of `df` that exist in the table (survey versions differ).
append <- function(schema, table, df) {
  cols <- ofarena::arena_columns(con, schema, table)$name
  if (length(cols) == 0 || nrow(df) == 0) return(invisible(0))
  dbAppendTable(con, Id(schema = schema, table = table), df[intersect(names(df), cols)])
}

# ---- Code lists ---------------------------------------------------------------

versuchsflaechen <- data.frame(
  code = c("1", "2", "3"),
  label = c("NWR Birkenkopf", "NWR Eichelberg", "NWR Weiherwald"),
  lon = c(7.781, 7.702, 7.858),
  lat = c(49.352, 49.281, 49.431)
)
groups <- c(gr = "Gräser", kr = "Kräuter", fa = "Farne", mo = "Moose", zs = "Zwergsträucher", st = "Sträucher")
species <- data.frame(
  group = c("gr", "gr", "gr", "gr", "kr", "kr", "kr", "kr", "fa", "fa", "mo", "mo", "mo", "zs", "zs", "st", "st"),
  code = c("desfle", "luzluz", "calepi", "carpil", "oxaace", "galodo", "digpur", "imppar", "pteaqu",
           "drycar", "polfor", "hypcup", "leugla", "vacmyr", "calvul", "rubfru", "rubida"),
  label = c("Deschampsia flexuosa", "Luzula luzuloides", "Calamagrostis epigejos", "Carex pilulifera",
            "Oxalis acetosella", "Galium odoratum", "Digitalis purpurea", "Impatiens parviflora",
            "Pteridium aquilinum", "Dryopteris carthusiana", "Polytrichum formosum",
            "Hypnum cupressiforme", "Leucobryum glaucum", "Vaccinium myrtillus", "Calluna vulgaris",
            "Rubus fruticosus agg.", "Rubus idaeus")
)
trees <- c(BU = "Rotbuche", TEI = "Traubeneiche", FI = "Fichte", DGL = "Douglasie",
           KI = "Waldkiefer", BAH = "Bergahorn", HBU = "Hainbuche", VB = "Eberesche")
wuchsstufen <- c("1" = "< 20 cm", "2" = "20–50 cm", "3" = "51–130 cm", "4" = "> 130 cm")
schaeden <- c("0" = "kein Schaden", "1" = "Verbiss", "2" = "Fegeschaden", "3" = "Schälschaden")
tierarten <- c(reh = "Reh", rot = "Rothirsch", ws = "Wildschwein", hase = "Feldhase")
certainty <- c("1" = "sicher", "2" = "unsicher", "3" = "nur Gattung")
seitenlicht <- c("0" = "kein", "1" = "gering", "2" = "mittel", "3" = "stark")
mischung <- c("E" = "Einzelmischung", "T" = "Truppmischung", "G" = "Gruppenmischung", "R" = "Reinbestand")

# ---- Photos -------------------------------------------------------------------

files <- list()
photo <- function(survey, record_uuid, text, colour, width = 640, height = 480) {
  img <- magick::image_blank(width, height, colour)
  img <- magick::image_annotate(img, text, size = 28, gravity = "center", color = "white",
                                weight = 700)
  bytes <- magick::image_write(img, format = "jpeg", quality = 70)
  id <- uuid()
  name <- paste0(gsub("[^A-Za-z0-9]+", "_", text), ".jpg")
  files[[length(files) + 1]] <<- data.frame(
    survey = survey, uuid = id,
    props = json(list(name = name, size = length(bytes), recordUuid = record_uuid, nodeUuid = uuid())),
    content = I(list(bytes))
  )
  c(uuid = id, name = name)
}
no_photo <- c(uuid = NA_character_, name = NA_character_)
with_photo <- function(p, survey, record_uuid, text, colour, ...) {
  if (runif(1) < p) photo(survey, record_uuid, text, colour, ...) else no_photo
}

# ---- Records and entities -----------------------------------------------------

owner <- "00000000-0000-4000-8000-000000000001"
rows <- list(rec = list(), record = list(), pk = list(), pers = list(), verjk = list(), bd = list(),
             bd_ext = list(), verj = list(), losung = list(), losung_img = list())
# Rows are tagged with the survey (`sid` of the loop below); append() drops the tag.
add <- function(table, df) {
  df$survey <- sid
  rows[[table]][[length(rows[[table]]) + 1]] <<- df
}

# Survey 31 was used in 2024, survey 33 from 2025 on.
survey_years <- list(`31` = c(`0` = 2024), `33` = c(`1` = 2025, `2` = 2026))
utm <- sf::st_crs(25832)

for (s in names(survey_years)) {
  sid <- as.integer(s)
  for (v in seq_len(nrow(versuchsflaechen))) {
    vf <- versuchsflaechen[v, ]
    centre <- sf::st_coordinates(sf::st_transform(sf::st_sfc(sf::st_point(c(vf$lon, vf$lat)), crs = 4326), utm))
    for (k in 1:6) {
      pk_code <- sprintf("%s%02d", vf$code, k)
      pk_xy <- centre + c(((k - 1) %% 3) * 60, ((k - 1) %/% 3) * 60)
      fenced <- k %% 2 == 0
      for (cycle in names(survey_years[[s]])) {
        year <- survey_years[[s]][[cycle]]
        # Plot-year not surveyed: a gap in the completeness grid.
        if (sid == 33 && vf$code == "2" && k == 5 && year == 2026) next

        record_uuid <- uuid()
        step <- if (year == 2026) pick(c("1", "1", "2")) else pick(c("2", "3", "3"))
        add("record", data.frame(uuid = record_uuid, owner_uuid = owner, step = step, cycle = cycle,
                                 date_created = as.POSIXct(sprintf("%d-06-15 08:00:00", year), tz = "UTC")))
        rec_uuid <- uuid()
        add("rec", data.frame(uuid = rec_uuid, parent_uuid = NA, record_uuid = record_uuid,
                              record_cycle = cycle, record_step = step, record_owner_uuid = owner,
                              note = if (runif(1) < 0.2) "Zaun kontrolliert" else NA,
                              versuchsflaeche = vf$code, versuchsflaeche_label = vf$label,
                              probekreisnummer = pk_code, probekreisnummer_label = pk_code))

        # GPS position with a few metres of noise; one plot in UTM, one without GPS, one far off.
        pos <- pk_xy + rnorm(2, 0, 2.5)
        if (sid == 33 && vf$code == "2" && k == 2 && year == 2026) pos <- pos + c(150, 0)
        missing_pos <- sid == 33 && vf$code == "3" && k == 2 && year == 2025
        in_utm <- vf$code == "1" && k == 3
        ll <- sf::st_coordinates(sf::st_transform(sf::st_sfc(sf::st_point(pos), crs = utm), 4326))
        pk_uuid <- uuid()
        pano <- with_photo(if (vf$code == "2") 0.6 else 0.95, sid, record_uuid,
                           sprintf("Panorama %s PK %s %d", vf$label, pk_code, year), "#4a6b3a",
                           width = 960, height = 320)
        add("pk", data.frame(
          uuid = pk_uuid, parent_uuid = rec_uuid,
          panoramafoto_file_uuid = pano[["uuid"]], panoramafoto_file_name = pano[["name"]],
          hauptaufnahmejahr = year,
          curr_position_x = if (missing_pos) NA else if (in_utm) pos[1] else ll[1],
          curr_position_y = if (missing_pos) NA else if (in_utm) pos[2] else ll[2],
          curr_position_srs = if (missing_pos) NA else if (in_utm) "25832" else "4326",
          dateneintragung_durch = pick(c("Feldtablet", "Nachtrag Büro"))
        ))
        add("pers", data.frame(uuid = uuid(2), parent_uuid = pk_uuid,
                               aufnehmende_personen = pick(c("A. Weber", "K. Schmitt", "M. Becker", "J. Klein"), 2)))

        for (j in seq_len(sample(2:4, 1))) {
          verjk_uuid <- uuid()
          gon <- ((j - 1) * 100 + round(runif(1, -5, 5), 1)) %% 400
          gab <- 8
          horst <- runif(1) < 0.4
          gon_h <- if (horst) round((gon + runif(1, -8, 8)) %% 400, 1) else NA
          gab_h <- if (horst) round(gab + runif(1, -1, 1), 1) else NA
          rays <- if (horst) round(runif(8, 0.8, 3), 1) else rep(NA, 8)
          extended <- sid == 33 && runif(1) < 0.8
          no_ext_rows <- extended && vf$code == "1" && k == 1 && j == 1 && year == 2025
          label <- sprintf("%s PK %s %d VK %d", vf$label, pk_code, year, j)
          bild <- with_photo(0.85, sid, record_uuid, paste("Verjk", label), "#2f5d8a")
          bild_h <- if (sid == 33) with_photo(0.5, sid, record_uuid, paste("Verjk horizontal", label), "#3d7ab8") else no_photo
          horst_bild <- if (horst) with_photo(0.8, sid, record_uuid, paste("Horst", label), "#7a4f2a") else no_photo
          sl <- pick(names(seitenlicht))
          mi <- pick(names(mischung))

          verjk <- data.frame(
            uuid = verjk_uuid, parent_uuid = pk_uuid, gon = gon, gab = gab,
            verjk_groesse = 10,
            verjuengungshorst = tolower(as.character(horst)),
            verjuengungshorst_groesse = if (horst) round(runif(1, 3, 20), 1) else NA,
            verjk_aufnahmedatum = as.Date(sprintf("%d-%02d-%02d", year, sample(5:9, 1), sample(1:28, 1))),
            seitenlicht = sl, seitenlicht_label = seitenlicht[[sl]],
            ueberschirmung = round(runif(1, 20, 95)),
            mischungsform = mi, mischungsform_label = mischung[[mi]],
            besonderheiten = NA, besonderheiten_label = NA,
            hangneigung = round(runif(1, 0, 25)),
            bemerkungen = if (runif(1) < 0.15) "Wildwechsel am Rand" else NA,
            verjk_bild_file_uuid = bild[["uuid"]], verjk_bild_file_name = bild[["name"]],
            verj_deckungsgrad_g = round(runif(1, 5, 80)),
            gezauent = if (fenced) "1" else "0", gezauent_label = if (fenced) "ja" else "nein",
            bodendecker_extended = tolower(as.character(extended)),
            verjk_bild_horizontal_file_uuid = bild_h[["uuid"]], verjk_bild_horizontal_file_name = bild_h[["name"]],
            gon_horst = gon_h, gab_horst = gab_h,
            neigung_horst = if (horst) round(runif(1, 0, 20)) else NA,
            verjh_kunstverjuengung = if (horst) "false" else NA,
            verjh_bild_file_uuid = horst_bild[["uuid"]], verjh_bild_file_name = horst_bild[["name"]],
            x_horstmittelpunkt = if (horst) round(gab_h * sin(gon_h * pi / 200), 2) else NA,
            y_horstmittelpunkt = if (horst) round(gab_h * cos(gon_h * pi / 200), 2) else NA
          )
          verjk[paste0("gab_horst_", seq(50, 400, 50))] <- as.list(rays)
          add("verjk", verjk)

          # Simple ground cover (31 and 33).
          bd_groups <- pick(names(groups), sample(2:4, 1))
          add("bd", data.frame(
            uuid = uuid(length(bd_groups)), parent_uuid = verjk_uuid,
            bodendecker_deckungsgrad = round(runif(length(bd_groups), 1, 60)),
            bodendecker = bd_groups, bodendecker_label = unname(groups[bd_groups]),
            bodendecker_art = vapply(bd_groups, function(g) pick(species$code[species$group == g]), ""),
            bodendecker_art_label = NA
          ))

          # Extended ground cover (33 only).
          if (extended && !no_ext_rows) {
            sp <- species[sample(nrow(species), sample(3:8, 1)), ]
            group_cover <- tapply(runif(nrow(sp), 5, 40), sp$group, sum)
            cert <- pick(names(certainty), nrow(sp), replace = TRUE, prob = c(0.8, 0.15, 0.05))
            ext_photo <- lapply(seq_len(nrow(sp)), function(i) {
              with_photo(0.3, sid, record_uuid, paste(sp$label[i], "-", label), "#5b8a3c")
            })
            add("bd_ext", data.frame(
              uuid = uuid(nrow(sp)), parent_uuid = verjk_uuid,
              foto_bodendecker_ext_file_uuid = vapply(ext_photo, `[[`, "", "uuid"),
              foto_bodendecker_ext_file_name = vapply(ext_photo, `[[`, "", "name"),
              kommentar = ifelse(runif(nrow(sp)) < 0.15, "Beleg gesammelt", NA),
              manual_id = seq_len(nrow(sp)),
              cover_funct_group_perc = round(pmin(group_cover[sp$group], 95)),
              bodendecker_ext_group = sp$group, bodendecker_ext_group_label = unname(groups[sp$group]),
              bodendecker_ext_art = sp$code, bodendecker_ext_art_label = sp$label,
              cover_bodendecker_ext = round(runif(nrow(sp), 0.5, 30), 1),
              identification_certainty = cert,
              identification_certainty_label = unname(certainty[cert]),
              aktiv_filter = sp$group
            ))
          }

          # Regeneration by species and growth stage.
          n_verj <- sample(1:5, 1)
          art <- pick(names(trees), n_verj, replace = TRUE)
          ws <- pick(names(wuchsstufen), n_verj, replace = TRUE)
          sch <- pick(names(schaeden), n_verj, replace = TRUE, prob = if (fenced) c(0.8, 0.1, 0.05, 0.05) else c(0.4, 0.4, 0.1, 0.1))
          anzahl <- sample(1:25, n_verj, replace = TRUE)
          verj_photo <- lapply(seq_len(n_verj), function(i) with_photo(0.15, sid, record_uuid, paste(trees[[art[i]]], "-", label), "#8a6d1f"))
          add("verj", data.frame(
            uuid = uuid(n_verj), parent_uuid = verjk_uuid,
            wuchsstufe = ws, wuchsstufe_label = unname(wuchsstufen[ws]),
            anzahl = anzahl, schaden = sch, schaden_label = unname(schaeden[sch]),
            intensitaet = NA, intensitaet_label = NA,
            anzahl_gver = ifelse(sch == "1", pmin(anzahl, sample(1:5, n_verj, TRUE)), 0L),
            anzahl_tver = ifelse(sch == "1", pmin(anzahl, sample(0:3, n_verj, TRUE)), 0L),
            art = art, art_label = unname(trees[art]),
            verbissgrad_g = NA, verbissgrad_t = NA,
            deckungsgrad = sample(1:30, n_verj, TRUE),
            verjtab_bemerkungen = NA, intensitaet_verb = NA, intensitaet_verb_label = NA,
            kunverjuengung_code = NA, kunverjuengung_code_label = NA,
            verjuengung_photo_file_uuid = vapply(verj_photo, `[[`, "", "uuid"),
            verjuengung_photo_file_name = vapply(verj_photo, `[[`, "", "name"),
            deckungsgrad_dez = NA
          ))

          # Droppings.
          for (l in seq_len(sample(0:2, 1, prob = c(0.5, 0.35, 0.15)))) {
            losung_uuid <- uuid()
            tier <- pick(names(tierarten))
            add("losung", data.frame(uuid = losung_uuid, parent_uuid = verjk_uuid, tierart = tier,
                                     tierart_label = tierarten[[tier]], anzahl_losung = sample(1:6, 1),
                                     bemerkung_losung = NA))
            img <- with_photo(0.6, sid, record_uuid, paste("Losung", tierarten[[tier]], "-", label), "#6b4a6b")
            if (!is.na(img[["uuid"]])) {
              add("losung_img", data.frame(uuid = uuid(), parent_uuid = losung_uuid,
                                           losung_img_file_uuid = img[["uuid"]], losung_img_file_name = img[["name"]]))
            }
          }
        }
      }
    }
  }
}

bind <- function(table, survey) {
  df <- do.call(rbind, lapply(rows[[table]], function(d) d[, sort(names(d)), drop = FALSE]))
  df[df$survey == survey, ]
}

# ---- Metadata: survey rows, node definitions ------------------------------------

node_defs <- function() {
  e <- function(name, label, parent, multiple = TRUE) list(type = "entity", name = name, label = label, parent = parent, multiple = multiple)
  a <- function(name, label, parent, type = "text", key = FALSE) list(type = type, name = name, label = label, parent = parent, key = key)
  list(
    e("verjuengungsaufnahme", "Verjüngungsaufnahme", NA, FALSE),
    a("versuchsflaeche", "Versuchsfläche", "verjuengungsaufnahme", "code", TRUE),
    a("probekreisnummer", "Probekreis", "verjuengungsaufnahme", "code", TRUE),
    a("note", "Notiz", "verjuengungsaufnahme"),
    e("probekreis", "Probekreis", "verjuengungsaufnahme"),
    a("hauptaufnahmejahr", "Hauptaufnahmejahr", "probekreis", "integer", TRUE),
    a("curr_position", "Aktuelle Position", "probekreis", "coordinate"),
    a("panoramafoto", "Panoramafoto", "probekreis", "file"),
    a("dateneintragung_durch", "Dateneintragung durch", "probekreis"),
    a("aufnehmende_personen", "Aufnehmende Personen", "probekreis"),
    e("tab_verjk", "Verjüngungskreis", "probekreis"),
    a("gon", "Richtung (gon)", "tab_verjk", "decimal", TRUE),
    a("gab", "Horizontalabstand (m)", "tab_verjk", "decimal", TRUE),
    a("verjk_groesse", "Größe des Verjüngungskreises [m2]", "tab_verjk", "decimal"),
    a("verjuengungshorst", "Verjüngungshorst", "tab_verjk", "boolean"),
    a("verjuengungshorst_groesse", "Horstgröße (m²)", "tab_verjk", "decimal"),
    a("verjk_aufnahmedatum", "Aufnahmedatum", "tab_verjk", "date"),
    a("seitenlicht", "Seitenlicht", "tab_verjk", "code"),
    a("ueberschirmung", "Überschirmung (%)", "tab_verjk", "decimal"),
    a("mischungsform", "Mischungsform", "tab_verjk", "code"),
    a("besonderheiten", "Besonderheiten", "tab_verjk", "code"),
    a("hangneigung", "Hangneigung (%)", "tab_verjk", "decimal"),
    a("bemerkungen", "Bemerkungen", "tab_verjk"),
    a("verjk_bild", "Foto Verjüngungskreis", "tab_verjk", "file"),
    a("verjk_bild_horizontal", "Foto horizontal", "tab_verjk", "file"),
    a("verj_deckungsgrad_g", "Deckungsgrad Verjüngung (%)", "tab_verjk", "integer"),
    a("gezauent", "Gezäunt", "tab_verjk", "code"),
    a("bodendecker_extended", "Bodendecker erweitert", "tab_verjk", "boolean"),
    a("gon_horst", "Richtung Horstmitte (gon)", "tab_verjk", "decimal", TRUE),
    a("gab_horst", "Abstand Horstmitte (m)", "tab_verjk", "decimal", TRUE),
    a("neigung_horst", "Neigung Horst (%)", "tab_verjk", "decimal"),
    a("verjh_bild", "Foto Horst", "tab_verjk", "file"),
    e("tab_bodendecker", "Bodendecker", "tab_verjk"),
    a("bodendecker", "Gruppe", "tab_bodendecker", "code", TRUE),
    a("bodendecker_art", "Art", "tab_bodendecker", "code", TRUE),
    a("bodendecker_deckungsgrad", "Deckungsgrad (%)", "tab_bodendecker", "decimal"),
    e("tab_bodendecker_extended", "Bodendecker erweitert", "tab_verjk"),
    a("manual_id", "Nr.", "tab_bodendecker_extended", "integer", TRUE),
    a("bodendecker_ext_group", "Funktionsgruppe", "tab_bodendecker_extended", "code", TRUE),
    a("bodendecker_ext_art", "Art", "tab_bodendecker_extended", "code", TRUE),
    a("cover_bodendecker_ext", "Deckung Art (%)", "tab_bodendecker_extended", "decimal"),
    a("cover_funct_group_perc", "Deckung Gruppe (%)", "tab_bodendecker_extended", "decimal"),
    a("identification_certainty", "Bestimmungssicherheit", "tab_bodendecker_extended", "code"),
    a("kommentar", "Kommentar", "tab_bodendecker_extended"),
    a("foto_bodendecker_ext", "Foto", "tab_bodendecker_extended", "file"),
    e("tab_verj", "Verjüngung", "tab_verjk"),
    a("art", "Baumart", "tab_verj", "code"),
    a("wuchsstufe", "Wuchsstufe", "tab_verj", "code"),
    a("anzahl", "Anzahl", "tab_verj", "integer"),
    a("schaden", "Schaden", "tab_verj", "code"),
    a("anzahl_gver", "Anzahl Gipfelverbiss", "tab_verj", "integer"),
    a("anzahl_tver", "Anzahl Triebverbiss", "tab_verj", "integer"),
    a("deckungsgrad", "Deckungsgrad (%)", "tab_verj", "integer"),
    a("verjuengung_photo", "Foto", "tab_verj", "file"),
    e("tab_losung", "Losung", "tab_verjk"),
    a("tierart", "Tierart", "tab_losung", "code"),
    a("anzahl_losung", "Anzahl Losungshaufen", "tab_losung", "integer"),
    a("bemerkung_losung", "Bemerkung", "tab_losung"),
    a("losung_img", "Foto Losung", "tab_losung", "file")
  )
}

survey_props <- function(name, cycles) json(list(
  name = name,
  labels = list(de = "Verjüngungsaufnahme - Naturwaldforschung - FAWF"),
  languages = c("de", "la"),
  srs = list(list(code = "4326", name = "WGS 1984 (EPSG:4326)"),
             list(code = "25832", name = "European Terrestrial Reference System 1989 (EPSG:25832)")),
  cycles = cycles
))

invisible(dbWithTransaction(con, {
  dbExecute(con, "DELETE FROM public.survey WHERE id IN (31, 33)")
  dbExecute(con, "INSERT INTO public.\"user\" (uuid, name, email, status) VALUES ($1, 'Synthetic', 'dev@example.org', 'ACCEPTED')
                  ON CONFLICT DO NOTHING", params = list(owner))
  dbExecute(con, "INSERT INTO public.survey (id, uuid, published, draft, props, owner_uuid)
                  OVERRIDING SYSTEM VALUE VALUES ($1, $2, TRUE, FALSE, $3, $4)",
            params = list(c(31L, 33L), uuid(2), c(
              survey_props("waldstruktur_verjuengung_v2_1", list(`0` = list(dateStart = "2024-01-01", dateEnd = "2024-12-31"), `1` = list(dateStart = "2025-01-01"))),
              survey_props("verjuengungsaufnahme", list(`0` = list(dateStart = "2024-01-01", dateEnd = "2024-12-31"), `1` = list(dateStart = "2025-01-01", dateEnd = "2025-12-31"), `2` = list(dateStart = "2026-01-01")))
            ), rep(owner, 2)))

  files_df <- do.call(rbind, files)
  for (s in c("31", "33")) {
    sid <- as.integer(s)
    meta <- paste0("survey_", s)
    data <- paste0("survey_", s, "_data")
    dbExecute(con, paste0("DELETE FROM ", meta, ".record"))  # cascades into the data tables
    dbExecute(con, paste0("DELETE FROM ", meta, ".file"))
    dbExecute(con, paste0("DELETE FROM ", meta, ".node_def"))

    # Plot category with the plot locations, as in survey 33.
    dbExecute(con, paste0("DELETE FROM ", meta, ".category"))
    category <- uuid()
    levels <- uuid(2)
    append(meta, "category", data.frame(uuid = category, published = TRUE, props = json(list(
      name = "sampling_point_data", itemExtraDef = list(location = list(dataType = "geometryPoint", index = 0))
    ))))
    append(meta, "category_level", data.frame(uuid = levels, category_uuid = category, index = 0:1,
                                              props = c(json(list(name = "level_1")), json(list(name = "level_2")))))
    ewkt <- function(xy) sprintf("SRID=25832;POINT(%.2f %.2f)", xy[1], xy[2])
    vf_items <- uuid(nrow(versuchsflaechen))
    pk_items <- list()
    for (v in seq_len(nrow(versuchsflaechen))) {
      vf <- versuchsflaechen[v, ]
      centre <- sf::st_coordinates(sf::st_transform(sf::st_sfc(sf::st_point(c(vf$lon, vf$lat)), crs = 4326), utm))
      pk_items[[v]] <- data.frame(
        uuid = uuid(6), level_uuid = levels[2], parent_uuid = vf_items[v],
        props = vapply(1:6, function(k) json(list(
          code = sprintf("%s%02d", vf$code, k), labels = list(de = sprintf("%s%02d", vf$code, k)),
          # Same layout as the plots above; the true centre, without GPS noise.
          extra = list(location = ewkt(centre + c(((k - 1) %% 3) * 60, ((k - 1) %/% 3) * 60)))
        )), "")
      )
    }
    append(meta, "category_item", rbind(
      data.frame(uuid = vf_items, level_uuid = levels[1], parent_uuid = NA,
                 props = vapply(seq_len(nrow(versuchsflaechen)), function(v) json(list(
                   code = versuchsflaechen$code[v], labels = list(de = versuchsflaechen$label[v]),
                   extra = list(location = ewkt(sf::st_coordinates(sf::st_transform(sf::st_sfc(
                     sf::st_point(c(versuchsflaechen$lon[v], versuchsflaechen$lat[v])), crs = 4326), utm))))
                 )), "")),
      do.call(rbind, pk_items)
    ))

    defs <- node_defs()
    def_uuid <- setNames(uuid(length(defs)), vapply(defs, `[[`, "", "name"))
    append(meta, "node_def", data.frame(
      uuid = unname(def_uuid),
      parent_uuid = vapply(defs, function(d) if (is.na(d$parent)) NA_character_ else def_uuid[[d$parent]], ""),
      type = vapply(defs, `[[`, "", "type"),
      props = vapply(defs, function(d) json(c(
        list(name = d$name, labels = list(de = d$label), multiple = isTRUE(d$multiple), key = isTRUE(d$key)),
        if (d$name %in% c("versuchsflaeche", "probekreisnummer")) list(categoryUuid = category)
      )), "")
    ))

    f <- files_df[files_df$survey == sid, ]
    append(meta, "file", tibble::tibble(uuid = f$uuid, props = f$props,
                                        content = blob::as_blob(unclass(f$content))))

    append(meta, "record", bind("record", sid))
    append(data, "data_verjuengungsaufnahme", bind("rec", sid))
    append(data, "data_probekreis", bind("pk", sid))
    append(data, "data_aufnehmende_personen", bind("pers", sid))
    append(data, "data_tab_verjk", bind("verjk", sid))
    append(data, "data_tab_bodendecker", bind("bd", sid))
    append(data, "data_tab_bodendecker_extended", bind("bd_ext", sid))
    append(data, "data_tab_verj", bind("verj", sid))
    append(data, "data_tab_losung", bind("losung", sid))
    append(data, "data_losung_img", bind("losung_img", sid))
    dbExecute(con, paste0("UPDATE ", data, ".data_probekreis SET curr_position = ST_MakePoint(curr_position_x, curr_position_y)
                           WHERE curr_position_x IS NOT NULL"))
  }
}))

counts <- dbGetQuery(con, "
  SELECT 'survey_31' AS schema, (SELECT count(*) FROM survey_31_data.data_probekreis) AS probekreis,
         (SELECT count(*) FROM survey_31_data.data_tab_verjk) AS verjk,
         0 AS bd_ext, (SELECT count(*) FROM survey_31.file) AS files
  UNION ALL
  SELECT 'survey_33', (SELECT count(*) FROM survey_33_data.data_probekreis),
         (SELECT count(*) FROM survey_33_data.data_tab_verjk),
         (SELECT count(*) FROM survey_33_data.data_tab_bodendecker_extended),
         (SELECT count(*) FROM survey_33.file)")
print(counts)

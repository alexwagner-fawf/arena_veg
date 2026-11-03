test_that("surveys 31 and 33 stack into one model with NA fill", {
  d <- veg_assemble(fake_raw(), fake_surveys(), fake_node_defs())
  expect_equal(nrow(d$pk), 2)
  expect_equal(d$pk$jahr, c(2024L, 2025L))
  expect_equal(d$pk$aufnehmende_personen, c(NA, "A, B"))
  expect_true(all(is.na(d$pk$dateneintragung_durch)))

  expect_equal(nrow(d$verjk), 3)
  expect_equal(d$verjk$verjk_label, c("VK 1 (0 gon, 10 m)", "VK 1 (100 gon, 5 m)", "VK 2 (200 gon, 5 m)"))
  expect_equal(d$verjk$pk_label[1], "NWR Test · PK 101 · 2024")

  # Children carry the keys of their plot and circle.
  expect_equal(d$bd_ext$pk_uuid, rep(d$pk$pk_uuid[2], 2))
  expect_equal(d$bd_ext$verjk_label, rep("VK 1 (100 gon, 5 m)", 2))
  expect_equal(nrow(d$losung), 0)
  expect_equal(unname(d$labels["gon"]), "Richtung (gon)")
})

test_that("circles and patches are placed around the plot centre", {
  d <- veg_assemble(fake_raw(), fake_surveys(), fake_node_defs())
  centre <- sf::st_transform(d$pk_sf[1, ], 25832)
  circle <- sf::st_transform(d$verjk_sf[1, ], 25832)
  # 0 gon, 10 m: due north; verjk_groesse is the area in m2.
  offset <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(circle))) - sf::st_coordinates(centre)
  expect_equal(unname(offset[1, ]), c(0, 10), tolerance = 1e-3)
  expect_equal(as.numeric(sf::st_area(circle)), 1, tolerance = 0.01)
  # The plot area is a circle of pk_radius_m around the centre.
  expect_equal(as.numeric(sf::st_area(sf::st_transform(d$pk_area_sf[1, ], 25832))), pi * 17.84^2, tolerance = 0.01)

  expect_equal(nrow(d$horst_sf), 1)
  horst <- sf::st_transform(d$horst_sf, 25832)
  centre2 <- sf::st_transform(d$pk_sf[2, ], 25832)
  offset <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(horst))) - sf::st_coordinates(centre2)
  expect_equal(unname(offset[1, ]), c(5, 0), tolerance = 1e-3)
})

test_that("photos are collected with their context", {
  d <- veg_assemble(fake_raw(), fake_surveys(), fake_node_defs())
  expect_equal(nrow(d$photos), 1)
  expect_equal(d$photos$type, "Panorama")
  expect_equal(d$photos$pk_uuid, d$pk$pk_uuid[1])
})

test_that("bind_surveys turns conflicting column types into text", {
  out <- bind_surveys(list(tibble::tibble(a = 1, b = "x"), NULL, tibble::tibble(a = "2", c = TRUE)))
  expect_equal(out$a, c("1", "2"))
  expect_equal(out$b, c("x", NA))
})

test_that("filters restrict plots, circles and children", {
  d <- veg_assemble(fake_raw(), fake_surveys(), fake_node_defs())
  all <- list(surveys = c(31L, 33L), step = c("1", "3"), cycle = c("0", "1"))
  expect_equal(nrow(veg_filter(d, all)$verjk), 3)

  only33 <- veg_filter(d, modifyList(all, list(surveys = 33L)))
  expect_equal(nrow(only33$pk), 1)
  expect_equal(nrow(only33$verjk), 2)
  expect_equal(nrow(only33$bd), 0)
  expect_equal(nrow(only33$photos), 0)

  by_year <- veg_filter(d, modifyList(all, list(jahr = "2024")))
  expect_equal(by_year$pk$jahr, 2024L)

  expect_equal(veg_species_circles(d, "Vaccinium myrtillus"), d$verjk$verjk_uuid[2])
  expect_equal(veg_species_circles(d, "desfle"), d$verjk$verjk_uuid[1])
})

test_that("QA grid flags gaps and missing plot-years", {
  raw <- fake_raw()
  # A second plot surveyed only in 2025, so 2024 is missing for it.
  raw$rec <- dplyr::bind_rows(raw$rec, dplyr::mutate(raw$rec[2, ], uuid = "aaaaaaaa-0003-0000-4000-8000-000000000000",
                                                     probekreisnummer = "102", probekreisnummer_label = "102"))
  raw$pk <- dplyr::bind_rows(raw$pk, dplyr::mutate(raw$pk[2, ], uuid = "bbbbbbbb-0003-0000-4000-8000-000000000000",
                                                   parent_uuid = "aaaaaaaa-0003-0000-4000-8000-000000000000",
                                                   curr_position_x = NA))
  d <- veg_assemble(raw, fake_surveys(), fake_node_defs())
  q <- veg_qa(d)
  expect_equal(nrow(q), 4)
  status <- setNames(as.character(q$status), paste(q$pk_nr, q$jahr))
  expect_equal(status[["102 2024"]], "fehlt")
  expect_equal(status[["102 2025"]], "fehler")   # no position, no circle
  expect_equal(status[["101 2025"]], "warnung")  # circle 2 flagged extended but without rows
  expect_match(q$issues[q$pk_nr == "101" & q$jahr == 2025], "1 Verjk ohne Bodendecker")
  expect_match(q$issues[q$pk_nr == "101" & q$jahr == 2024], "ohne Foto")
})

test_that("the category location is the plot centre, GPS the fallback", {
  locations <- tibble::tibble(
    source_survey = 33L, versuchsflaeche = "1", probekreisnummer = "101",
    location_x = 411000, location_y = 5467000, location_srs = "25832"
  )
  d <- veg_assemble(fake_raw(), fake_surveys(), fake_node_defs(), locations)
  expect_equal(d$pk$centre_source, c("GPS", "Kategorie"))
  centre <- sf::st_coordinates(sf::st_transform(d$pk_sf[2, ], 25832))
  expect_equal(unname(centre[1, ]), c(411000, 5467000), tolerance = 1e-6)
  expect_true(d$pk$gps_offset_m[2] > 100)
  expect_equal(nrow(d$gps_sf), 2)

  gps_first <- veg_assemble(fake_raw(), fake_surveys(), fake_node_defs(), locations, pk_centre = "gps")
  expect_equal(gps_first$pk$centre_source, c("GPS", "GPS"))

  q <- veg_qa(d, max_gps_offset_m = 10)
  expect_match(q$issues[q$jahr == 2025], "GPS [0-9]+ m vom Probekreismittelpunkt")
})

test_that("years outside the record's cycle are errors and stay out of the grid", {
  raw <- fake_raw()
  raw$pk$hauptaufnahmejahr[2] <- 20251
  surveys <- fake_surveys()
  surveys$cycles <- list(
    tibble::tibble(cycle = "0", date_start = as.Date("2024-01-01"), date_end = as.Date("2024-12-31")),
    tibble::tibble(cycle = "1", date_start = as.Date("2025-01-01"), date_end = as.Date(NA))
  )
  q <- veg_qa(veg_assemble(raw, surveys, fake_node_defs()))
  bad <- q[q$jahr %in% 20251, ]
  expect_equal(as.character(bad$status), "fehler")
  expect_false(bad$in_grid)
  expect_match(bad$issues, "Aufnahmejahr 20251 passt nicht zu Zyklus 2")
  expect_true(all(q$in_grid[q$jahr %in% 2024]))
})

test_that("fenced circles are recognised by code or label", {
  expect_equal(is_fenced(c("1", "0", NA, NA), c("ja", "nein", "gez\u00e4unt", NA)), c(TRUE, FALSE, TRUE, FALSE))
})

test_that("natural_key orders embedded numbers by value", {
  x <- c("Knopf_105", "Knopf_2", "Knopf_11", NA, "ALA_401")
  expect_equal(x[order(natural_key(x))], c(NA, "ALA_401", "Knopf_2", "Knopf_11", "Knopf_105"))
})

test_that("positions are shown with a plain EPSG code", {
  p <- list(location_x = 361450.5, location_y = 5510376, location_srs = "25832",
            curr_position_x = 7.077447, curr_position_y = 49.72989, curr_position_srs = "EPSG:4326",
            gps_offset_m = 3.14)
  pos <- format_position(p)
  expect_equal(pos[["Mittelpunkt"]], "361450.5 / 5510376 (EPSG:25832)")
  expect_equal(pos[["GPS"]], "7.077447 / 49.72989 (EPSG:4326), 3.1 m vom Mittelpunkt")
})

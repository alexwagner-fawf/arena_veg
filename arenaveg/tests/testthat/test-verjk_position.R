assembled <- function() {
  d <- veg_assemble(fake_raw(), fake_surveys(), fake_node_defs())
  d$verjk_sf[d$verjk_sf$source_survey == 33L, ]
}

moved <- function(v, dx, dy = 0) {
  xy <- sf::st_coordinates(circle_centres(v, 25832L))
  sf::st_sfc(sf::st_point(c(xy[1, "X"] + dx, xy[1, "Y"] + dy)), crs = 25832L)
}

test_that("positions are kept in the GeoPackage, one row per circle", {
  v <- assembled()
  dir <- withr::local_tempdir()
  path <- file.path(dir, "p.gpkg")
  store <- position_store(path, 25832L)
  expect_equal(nrow(shiny::isolate(store$data())), 0)
  expect_false(file.exists(path))

  row <- position_row(v[1, ], moved(v[1, ], 3, 4), 25832L)
  row$photo_uuid <- "photo"
  row$rotation_deg <- 270L
  row$edited_at <- as.POSIXct("2026-11-06 12:00:00", tz = "UTC")
  store$save(row)
  store$save(position_row(v[2, ], moved(v[2, ], 1), 25832L))

  # A new process reads what was saved.
  again <- shiny::isolate(position_store(path, 25832L)$data())
  expect_equal(nrow(again), 2)
  first <- again[again$verjk_uuid == v$verjk_uuid[1], ]
  expect_equal(first$offset_m, 5)
  expect_equal(first$rotation_deg, 270L)
  expect_equal(first$photo_uuid, "photo")
  expect_equal(first$source_survey, 33L)
  expect_equal(first$edited_at, row$edited_at, ignore_attr = TRUE)
  expect_equal(first$pk_label, v$pk_label[1])
  expect_equal(sf::st_crs(first)$epsg, 25832L)
  expect_equal(unname(sf::st_coordinates(first)), unname(sf::st_coordinates(row)))
  expect_equal(c(first$orig_x, first$orig_y), unname(sf::st_coordinates(circle_centres(v[1, ], 25832L))[1, ]))

  # Saving again replaces the row.
  store$save(position_row(v[1, ], moved(v[1, ], 6, 8), 25832L))
  again <- position_read(path, 25832L)
  expect_equal(nrow(again), 2)
  expect_equal(again$offset_m[again$verjk_uuid == v$verjk_uuid[1]], 10)

  store$remove(v$verjk_uuid[1])
  store$remove(v$verjk_uuid[2])
  expect_equal(nrow(shiny::isolate(store$data())), 0)
  expect_equal(nrow(position_read(path, 25832L)), 0)
  # Adding to the emptied file still works, and no temporary files stay behind.
  store$save(position_row(v[1, ], moved(v[1, ], 1), 25832L))
  expect_equal(nrow(position_read(path, 25832L)), 1)
  expect_equal(list.files(dir, all.files = TRUE, no.. = TRUE), "p.gpkg")
})

test_that("saving starts from the file, so another process's position stays", {
  v <- assembled()
  path <- withr::local_tempfile(fileext = ".gpkg")
  a <- position_store(path, 25832L)
  b <- position_store(path, 25832L)
  a$save(position_row(v[1, ], moved(v[1, ], 1), 25832L))
  b$save(position_row(v[2, ], moved(v[2, ], 1), 25832L))
  expect_setequal(position_read(path, 25832L)$verjk_uuid, v$verjk_uuid[1:2])
})

test_that("a position that cannot be written leaves the saved ones alone", {
  v <- assembled()
  dir <- withr::local_tempdir()
  path <- file.path(dir, "p.gpkg")
  store <- position_store(path, 25832L)
  store$save(position_row(v[1, ], moved(v[1, ], 1), 25832L))
  Sys.chmod(dir, "555")
  withr::defer(Sys.chmod(dir, "755"))
  expect_error(store$save(position_row(v[2, ], moved(v[2, ], 1), 25832L)), "nicht beschreibbar")
  expect_equal(nrow(shiny::isolate(store$data())), 1)
  expect_equal(nrow(position_read(path, 25832L)), 1)
})

test_that("the map shows a circle of the same size at the new position, linked to the original", {
  v <- assembled()
  row <- position_row(v[1, ], moved(v[1, ], 3, 4), 25832L)
  f <- position_features(row, v)

  expect_equal(nrow(f$circles), 1)
  expect_equal(f$circles$is_draft, FALSE)
  new <- sf::st_transform(f$circles, 25832L)
  old <- sf::st_transform(v[1, ], 25832L)
  expect_equal(as.numeric(sf::st_area(new)), as.numeric(sf::st_area(old)), tolerance = 1e-3)
  expect_equal(unname(sf::st_coordinates(sf::st_centroid(sf::st_geometry(new)))), unname(sf::st_coordinates(row)), tolerance = 1e-6)

  ends <- unname(sf::st_coordinates(f$lines)[, 1:2])
  expect_equal(nrow(ends), 2)
  expect_equal(ends[1, ], unname(sf::st_coordinates(circle_centres(v[1, ], 25832L))[1, ]))
  expect_equal(ends[2, ], unname(sf::st_coordinates(row)[1, ]))
})

test_that("a draft replaces the circle's saved position, filtered circles are left out", {
  v <- assembled()
  saved <- rbind(position_row(v[1, ], moved(v[1, ], 3), 25832L), position_row(v[2, ], moved(v[2, ], 1), 25832L))

  draft <- position_row(v[1, ], moved(v[1, ], 9), 25832L)
  f <- position_features(saved, v, draft)
  expect_equal(nrow(f$circles), 2)
  expect_equal(f$circles$offset_m[f$circles$is_draft], 9)
  expect_equal(f$circles$verjk_uuid[f$circles$is_draft], v$verjk_uuid[1])

  expect_equal(position_features(saved, v[2, ])$lines$verjk_uuid, v$verjk_uuid[2])

  none <- position_features(position_empty(25832L), v)
  expect_equal(nrow(none$circles), 0)
  expect_equal(nrow(none$lines), 0)
  expect_equal(nrow(ofarena::map_layer(none$lines, "Verschiebung", none$lines$verjk_uuid)$data), 0)
})

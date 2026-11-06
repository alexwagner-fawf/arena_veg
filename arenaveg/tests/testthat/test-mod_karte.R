test_that("the circle details stay on the open tab when another circle is clicked", {
  d <- veg_assemble(fake_raw(), fake_surveys(), fake_node_defs())
  v <- d$verjk[d$verjk$source_survey == 33L, ]
  selection <- shiny::reactiveValues(pk = NULL, verjk = NULL, nonce = NULL)
  url_fun <- function(survey_id, file_uuid, thumb = FALSE, large = FALSE) paste0("f/", file_uuid)
  # bslib marks the open tab on its <li>.
  active_tab <- function(html) {
    m <- regmatches(html, regexpr("<li class=\"active\">\\s*<a[^>]*data-value=\"[^\"]+\"", html))
    sub(".*data-value=\"([^\"]+)\"$", "\\1", m)
  }
  shiny::testServer(mod_karte_server, args = list(
    filtered = shiny::reactive(d), species = shiny::reactive(character()), selection = selection, url_fun = url_fun,
    positions = position_store(withr::local_tempfile(fileext = ".gpkg"), 25832L)
  ), {
    selection$pk <- v$pk_uuid[1]
    selection$verjk <- v$verjk_uuid[1]
    session$flushReact()
    expect_equal(active_tab(output$verjk_detail$html), "Bodendecker erw.")

    session$setInputs(verjk_tab = "Fotos")
    selection$verjk <- v$verjk_uuid[2]
    session$flushReact()
    expect_equal(active_tab(output$verjk_detail$html), "Fotos")
  })
})

test_that("a corrected position is placed on the map, saved with the photo's rotation, and removed", {
  d <- veg_assemble(fake_raw(), fake_surveys(), fake_node_defs())
  v <- d$verjk_sf[d$verjk_sf$source_survey == 33L, ]
  d$photos <- tibble::tibble(
    source_survey = 33L, file_uuid = c("photo1", "photo2"), type = photo_types[["verjk_bild"]], detail = "VK",
    pk_uuid = v$pk_uuid[1], verjk_uuid = v$verjk_uuid[1], pk_label = v$pk_label[1], vf = v$vf[1]
  )
  selection <- shiny::reactiveValues(pk = NULL, verjk = NULL, nonce = NULL)
  url_fun <- function(survey_id, file_uuid, thumb = FALSE, large = FALSE) paste0("f/", file_uuid)
  path <- withr::local_tempfile(fileext = ".gpkg")
  store <- position_store(path, 25832L)
  centre <- sf::st_coordinates(sf::st_transform(circle_centres(v[1, ], 25832L), 4326))
  click <- function(session, dx) {
    session$setInputs("map-map_click" = list(lng = centre[1, "X"] + dx, lat = centre[1, "Y"]))
  }

  shiny::testServer(mod_karte_server, args = list(
    filtered = shiny::reactive(d), species = shiny::reactive(character()), selection = selection, url_fun = url_fun,
    positions = store
  ), {
    selection$pk <- v$pk_uuid[1]
    selection$verjk <- v$verjk_uuid[1]
    session$flushReact()
    expect_match(output$position_controls$html, "Lage korrigieren", fixed = TRUE)

    # Clicks count only while placing.
    click(session, 1e-4)
    expect_null(draft())

    session$setInputs(photo_state = list(file_uuid = "photo1", rotation = 90))
    session$setInputs(position_start = 1)
    expect_match(output$position_controls$html, "auf der Karte anklicken", fixed = TRUE)
    expect_equal(nrow(position_layers()[[2]]$data), 0)

    click(session, 1e-4)
    expect_match(output$position_controls$html, "Speichern", fixed = TRUE)
    # Shown before saving, dashed; nothing is written yet.
    expect_equal(position_layers()[[2]]$dash_array, "6 6")
    expect_false(file.exists(path))

    # A second click moves it.
    click(session, 2e-5)
    expect_lt(draft()$offset_m, 2)
    session$setInputs(position_save = 1)
    expect_null(placing())
    expect_null(draft())
    expect_match(output$position_controls$html, "Foto 90\u00b0 gedreht", fixed = TRUE)
    expect_equal(position_layers()[[2]]$dash_array, "")

    on_disk <- position_read(path, 25832L)
    expect_equal(nrow(on_disk), 1)
    expect_equal(on_disk$verjk_uuid, v$verjk_uuid[1])
    expect_equal(on_disk$photo_uuid, "photo1")
    expect_equal(on_disk$rotation_deg, 90L)
    expect_false(is.na(on_disk$edited_at))

    # Saving leaves the photo alone; back on the circle it opens at the saved rotation.
    selection$verjk <- v$verjk_uuid[2]
    session$flushReact()
    selection$verjk <- v$verjk_uuid[1]
    session$flushReact()
    expect_match(output$verjk_photos$html, 'data-saved-key="photo1" data-saved-turns="1"', fixed = TRUE)

    # Turning the photo further needs saving, and keeps the position.
    expect_no_match(output$position_controls$html, "Drehung speichern", fixed = TRUE)
    session$setInputs(photo_state = list(file_uuid = "photo1", rotation = 180))
    expect_match(output$position_controls$html, "Drehung speichern", fixed = TRUE)
    session$setInputs(position_rotation = 1)
    expect_no_match(output$position_controls$html, "Drehung speichern", fixed = TRUE)
    on_disk2 <- position_read(path, 25832L)
    expect_equal(on_disk2$rotation_deg, 180L)
    expect_equal(sf::st_coordinates(on_disk2), sf::st_coordinates(on_disk))

    # Another photo at 0 degrees is no change.
    session$setInputs(photo_state = list(file_uuid = "photo2", rotation = 0))
    expect_no_match(output$position_controls$html, "Drehung speichern", fixed = TRUE)

    # Changing starts from the saved position; cancelling keeps it.
    session$setInputs(position_start = 2)
    expect_equal(sf::st_coordinates(draft()), sf::st_coordinates(on_disk))
    session$setInputs(position_cancel = 1)
    expect_null(placing())
    expect_equal(nrow(position_read(path, 25832L)), 1)

    # Selecting another circle ends placing and drops the draft.
    session$setInputs(position_start = 3)
    click(session, 1e-4)
    selection$verjk <- v$verjk_uuid[2]
    session$flushReact()
    expect_null(placing())
    expect_null(draft())

    selection$verjk <- v$verjk_uuid[1]
    session$setInputs(position_delete_ok = 1)
    expect_equal(nrow(position_read(path, 25832L)), 0)
    expect_match(output$position_controls$html, "Lage korrigieren", fixed = TRUE)
  })
})

test_that("a failed save keeps the draft and reports the problem", {
  d <- veg_assemble(fake_raw(), fake_surveys(), fake_node_defs())
  v <- d$verjk_sf[d$verjk_sf$source_survey == 33L, ]
  selection <- shiny::reactiveValues(pk = v$pk_uuid[1], verjk = v$verjk_uuid[1], nonce = NULL)
  url_fun <- function(survey_id, file_uuid, thumb = FALSE, large = FALSE) paste0("f/", file_uuid)
  store <- position_store(file.path(withr::local_tempdir(), "gone", "p.gpkg"), 25832L)
  centre <- sf::st_coordinates(sf::st_transform(circle_centres(v[1, ], 25832L), 4326))

  shiny::testServer(mod_karte_server, args = list(
    filtered = shiny::reactive(d), species = shiny::reactive(character()), selection = selection, url_fun = url_fun,
    positions = store
  ), {
    session$setInputs(position_start = 1)
    session$setInputs("map-map_click" = list(lng = centre[1, "X"], lat = centre[1, "Y"]))
    session$setInputs(position_save = 1)
    expect_false(is.null(placing()))
    expect_false(is.null(draft()))
    expect_equal(nrow(shiny::isolate(store$data())), 0)
  })
})

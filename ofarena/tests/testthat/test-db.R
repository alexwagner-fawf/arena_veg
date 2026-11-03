# Integration tests against a database with Arena surveys 31 and 33, e.g. the
# local dev container after dev/restore.sh and dev/seed_synthetic.R:
#   ARENA_TEST_DB=1 ARENA_DB_PORT=55432 ARENA_DB_PASSWORD=geouser Rscript -e 'devtools::test("ofarena")'
skip_if_not(nzchar(Sys.getenv("ARENA_TEST_DB")), "ARENA_TEST_DB not set")

pool <- arena_pool()
withr::defer(pool::poolClose(pool), teardown_env())

test_that("surveys and access are reported", {
  s <- arena_surveys(pool)
  expect_true(all(c(31L, 33L) %in% s$id))
  expect_true(all(s$data_readable[s$id %in% c(31L, 33L)]))
  expect_true("4326" %in% s$srs[[which(s$id == 33L)]])

  access <- arena_check_access(pool, c(31, 33))
  expect_true(all(access$ok))
  expect_true("survey_33_data.data_tab_bodendecker_extended" %in% access$object)
})

test_that("entities are read without geometry columns", {
  pk <- arena_read_entity(pool, 33, "probekreis")
  expect_equal(names(pk)[1], "source_survey")
  expect_true(all(pk$source_survey == 33L))
  expect_false("curr_position" %in% names(pk))
  expect_true(all(c("curr_position_x", "curr_position_srs") %in% names(pk)))

  expect_null(arena_read_entity(pool, 31, "tab_bodendecker_extended"))
  some <- arena_read_entity(pool, 31, "tab_verjk", cols = c("uuid", "gon", "bodendecker_extended"))
  expect_equal(names(some), c("source_survey", "uuid", "gon"))
})

test_that("node definitions carry labels", {
  defs <- arena_node_defs(pool, 33)
  expect_true(all(c("uuid", "parent_uuid", "type", "name", "label") %in% names(defs)))
  expect_true("probekreis" %in% defs$name[defs$type == "entity"])
})

test_that("files are fetched once into the cache, with thumbnails", {
  pk <- arena_read_entity(pool, 33, "probekreis")
  files <- arena_file_cols(pk)
  cache <- withr::local_tempdir()

  path <- arena_file_fetch(pool, 33, files$file_uuid[1], cache)
  expect_true(file.exists(path))
  expect_match(path, "\\.jpg$")
  if (requireNamespace("magick", quietly = TRUE)) {
    thumb <- arena_file_fetch(pool, 33, files$file_uuid[1], cache, thumb = TRUE)
    expect_equal(magick::image_info(magick::image_read(thumb))$width, 360)
  }
  expect_null(arena_file_fetch(pool, 33, "00000000-0000-4000-8000-000000000000", cache))
  expect_null(arena_file_fetch(pool, 33, "not-a-uuid", cache))

  meta <- arena_file_meta(pool, 33)
  expect_true(all(meta$has_content))
  expect_true(files$file_uuid[1] %in% meta$file_uuid)
})

test_that("category items carry extra properties, points split into x/y/srs", {
  defs <- arena_node_defs(pool, 33)
  category <- defs$category_uuid[defs$name == "probekreisnummer"]
  expect_false(is.na(category))
  items <- arena_category_items(pool, 33, category)
  expect_true(all(c("code", "label", "parent_code", "level", "location_x", "location_y", "location_srs") %in% names(items)))
  plots <- items[items$level == 1, ]
  expect_true(all(!is.na(plots$parent_code)))
  expect_true(all(plots$location_srs == "25832"))
  expect_true(all(plots$location_x > 300000 & plots$location_x < 500000))
})

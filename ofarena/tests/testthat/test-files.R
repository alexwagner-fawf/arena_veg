test_that("arena_file_cols collects all file attributes", {
  df <- tibble::tibble(
    source_survey = 33L,
    uuid = c("a", "b"),
    foto_file_uuid = c("f1", NA),
    foto_file_name = c("x.jpg", NA),
    bild_file_uuid = c("f2", "f3"),
    bild_file_name = c("y.png", "z.jpg")
  )
  files <- arena_file_cols(df)
  expect_equal(nrow(files), 3)
  expect_equal(files$field, c("foto", "bild", "bild"))
  expect_equal(files$entity_uuid, c("a", "a", "b"))
  expect_equal(files$file_name, c("x.jpg", "y.png", "z.jpg"))
  expect_equal(unique(files$source_survey), 33L)
})

test_that("arena_file_cols works without file columns", {
  expect_equal(nrow(arena_file_cols(tibble::tibble(uuid = "a"))), 0)
})

test_that("file_ext falls back for odd names", {
  expect_equal(file_ext(c("IMG_1.JPG", "noext", NA, "a.b c")), c("jpg", "bin", "bin", "bin"))
})

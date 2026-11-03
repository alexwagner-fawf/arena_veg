test_that("arena_column_labels maps flattened columns to attribute labels", {
  defs <- tibble::tibble(
    name = c("bodendecker_art", "curr_position", "foto"),
    label = c("Art", "Position", "Foto")
  )
  labels <- arena_column_labels(
    c("bodendecker_art", "bodendecker_art_label", "curr_position_x", "foto_file_name", "other"),
    defs
  )
  expect_equal(unname(labels), c("Art", "Art", "Position X", "Foto (Datei)", "other"))
  expect_equal(names(labels)[5], "other")
})

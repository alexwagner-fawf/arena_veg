url_fun <- function(survey_id, file_uuid, thumb = FALSE, large = FALSE) {
  paste0("f/", file_uuid, ifelse(thumb, "?thumb", ifelse(large, "?large", "")))
}

test_that("photo_viewer shows the first photo large, with rotation buttons and a dropdown", {
  photos <- data.frame(source_survey = 33, file_uuid = c("a", "b"),
                       type = c("Verjüngungskreis", "Bodendecker erw."),
                       detail = c("VK 1", "VK 1 · Moos"))
  html <- as.character(photo_viewer(photos, url_fun, context = "VK 1"))

  expect_match(html, 'class="veg-viewer-img" src="f/a?large"', fixed = TRUE)
  expect_match(html, 'data-key="a" onload="vegViewer.fit(this)"', fixed = TRUE)
  expect_match(html, "vegViewer.rotate(this, -1)", fixed = TRUE)
  expect_match(html, "vegViewer.rotate(this, 1)", fixed = TRUE)
  expect_match(html, 'href="f/a"', fixed = TRUE)
  # One option per photo; the circle's label is not repeated.
  expect_match(html, 'onchange="vegViewer.show(this)"', fixed = TRUE)
  expect_match(html, 'data-src="f/a?large" data-href="f/a">1/2 · Verjüngungskreis</option>', fixed = TRUE)
  expect_match(html, 'data-src="f/b?large" data-href="f/b">2/2 · Bodendecker erw. · Moos</option>', fixed = TRUE)
  expect_no_match(html, "<img[^>]*thumb")

  # A single photo needs no dropdown.
  single <- as.character(photo_viewer(photos[1, ], url_fun))
  expect_no_match(single, "<select")
  expect_match(single, "Verjüngungskreis · VK 1", fixed = TRUE)
})

test_that("the CSS/JS version changes with the files' content", {
  dir <- withr::local_tempdir()
  writeLines("a { }", file.path(dir, "custom.css"))
  v1 <- resources_version(dir)
  expect_match(v1, "^[0-9.]+$")
  expect_equal(resources_version(dir), v1)
  writeLines("a { color: red; }", file.path(dir, "custom.css"))
  expect_false(identical(resources_version(dir), v1))
})

test_that("CSS and JS are linked with the version, so browsers fetch new copies", {
  dir <- withr::local_tempdir()
  writeLines("a { }", file.path(dir, "custom.css"))
  writeLines("var x;", file.path(dir, "viewer.js"))
  html <- as.character(htmltools::renderTags(golem_add_external_resources(dir))$head)
  v <- resources_version(dir)
  expect_match(html, paste0('href="www/custom.css?v=', v, '"'), fixed = TRUE)
  expect_match(html, paste0('src="www/viewer.js?v=', v, '"'), fixed = TRUE)
  expect_no_match(html, 'custom.css"', fixed = TRUE)
})

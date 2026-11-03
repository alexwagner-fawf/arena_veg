test_that("photo_viewer shows the first photo large, with rotation buttons and thumbnails", {
  url_fun <- function(survey_id, file_uuid, thumb = FALSE, large = FALSE) {
    paste0("f/", file_uuid, ifelse(thumb, "?thumb", ifelse(large, "?large", "")))
  }
  photos <- data.frame(source_survey = 33, file_uuid = c("a", "b"),
                       type = c("Verjüngungskreis", "Bodendecker erw."), detail = c("VK 1", NA))
  html <- as.character(photo_viewer(photos, url_fun))

  expect_match(html, 'class="veg-viewer-img" src="f/a?large"', fixed = TRUE)
  expect_match(html, 'data-key="a" onload="vegViewer.fit(this)"', fixed = TRUE)
  expect_match(html, "vegViewer.rotate(this, -1)", fixed = TRUE)
  expect_match(html, "vegViewer.rotate(this, 1)", fixed = TRUE)
  expect_match(html, 'href="f/a"', fixed = TRUE)
  expect_match(html, "Verjüngungskreis · VK 1", fixed = TRUE)
  # The second photo: thumbnail that shows its large copy when clicked.
  expect_match(html, 'src="f/b?thumb"', fixed = TRUE)
  expect_match(html, 'data-src="f/b?large" data-href="f/b" data-caption="Bodendecker erw."', fixed = TRUE)

  # A single photo needs no thumbnails.
  expect_no_match(as.character(photo_viewer(photos[1, ], url_fun)), "veg-viewer-thumbs")
})

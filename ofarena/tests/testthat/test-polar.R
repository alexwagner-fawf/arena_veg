centre <- function() sf::st_sfc(sf::st_point(c(400000, 5500000)), crs = 25832)

test_that("polar_xy: gon bearings run clockwise from north", {
  p <- polar_xy(0, 0, c(0, 100, 200, 300, 400, 50), 10)
  expect_equal(p$x, c(0, 10, 0, -10, 0, 10 * sqrt(0.5)), tolerance = 1e-12)
  expect_equal(p$y, c(10, 0, -10, 0, 10, 10 * sqrt(0.5)), tolerance = 1e-12)
})

test_that("polar_offset moves points in metres and keeps the CRS", {
  pts <- sf::st_transform(centre()[c(1, 1, 1)], 4326)
  moved <- polar_offset(pts, c(0, 100, NA), c(10, 5, 3), crs_metric = 25832)
  expect_equal(sf::st_crs(moved)$epsg, 4326L)
  expect_true(sf::st_is_empty(moved)[3])

  xy <- sf::st_coordinates(sf::st_transform(moved[1:2], 25832))
  expect_equal(unname(xy[1, ]), c(400000, 5500010), tolerance = 1e-6)
  expect_equal(unname(xy[2, ]), c(400005, 5500000), tolerance = 1e-6)
})

test_that("polygon_from_rays builds a regular octagon from equal rays", {
  bearings <- seq(50, 400, by = 50)
  poly <- polygon_from_rays(centre(), bearings, matrix(2, nrow = 1, ncol = 8))
  expect_equal(as.numeric(sf::st_area(poly)), 2 * sqrt(2) * 2^2, tolerance = 1e-9)
  expect_true(sf::st_is_valid(poly))
  expect_true(sf::st_within(centre(), poly, sparse = FALSE)[1, 1])
})

test_that("polygon_from_rays skips missing rays and needs enough of them", {
  bearings <- seq(50, 400, by = 50)
  d <- rbind(c(2, NA, 2, NA, 2, NA, 2, NA), c(1, NA, NA, NA, NA, NA, 1, NA))
  polys <- polygon_from_rays(centre()[c(1, 1)], bearings, d)
  # Four rays of 2 m at 50, 150, 250, 350 gon: a square with diagonal 4 m.
  expect_equal(as.numeric(sf::st_area(polys[1])), 8, tolerance = 1e-9)
  expect_true(sf::st_is_empty(polys)[2])
})

test_that("buffer_m draws circles with a radius in metres", {
  circles <- buffer_m(sf::st_transform(centre()[c(1, 1)], 4326), c(5, NA), crs_metric = 25832)
  expect_equal(as.numeric(sf::st_area(sf::st_transform(circles[1], 25832))), pi * 25, tolerance = 0.01)
  expect_true(sf::st_is_empty(circles)[2])
})

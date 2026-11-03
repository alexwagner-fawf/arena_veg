test_that("parse_srs accepts plain and EPSG-prefixed codes", {
  expect_identical(parse_srs(c("4326", "EPSG:25832", "epsg: 3035", "", NA, "WGS84")),
                   c(4326L, 25832L, 3035L, NA, NA, NA))
})

test_that("arena_points_sf handles mixed reference systems", {
  # The same location once in WGS 84 and once in ETRS89 / UTM 32N.
  utm <- sf::st_coordinates(sf::st_transform(sf::st_sfc(sf::st_point(c(7.77, 49.36)), crs = 4326), 25832))
  df <- data.frame(
    id = 1:3,
    pos_x = c(7.77, utm[1], NA),
    pos_y = c(49.36, utm[2], 49),
    pos_srs = c("4326", "25832", "4326")
  )
  pts <- arena_points_sf(df, "pos")
  expect_equal(nrow(pts), 2)
  expect_equal(sf::st_crs(pts)$epsg, 4326L)
  xy <- sf::st_coordinates(pts)
  expect_equal(xy[1, ], xy[2, ], tolerance = 1e-7)

  kept <- arena_points_sf(df, "pos", keep_empty = TRUE)
  expect_equal(nrow(kept), 3)
  expect_true(sf::st_is_empty(kept)[3])
})

test_that("utm_crs picks the zone", {
  expect_identical(utm_crs(7.77, 49.36), 32632L)
  expect_identical(utm_crs(-70, -33), 32719L)
})

test_that("parse_ewkt_point reads Arena's geometryPoint extra properties", {
  p <- parse_ewkt_point(c("SRID=25832;POINT(394170.56000000006 5620370.440000001)", "POINT(1 2)", NA, "srid=4326;point(7.5 -49e-1)"))
  expect_equal(p$x, c(394170.56000000006, NA, NA, 7.5))
  expect_equal(p$y, c(5620370.440000001, NA, NA, -4.9))
  expect_equal(p$srs, c("25832", NA, NA, "4326"))
})

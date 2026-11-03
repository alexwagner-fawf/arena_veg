write_raster <- function(path, bands, type = "Byte", size = 64, nodata = NULL) {
  # A small raster in ETRS89 / UTM 32N near Trippstadt, 1 m pixels.
  drv <- matrix(seq_len(size * size) %% 250, size, size)
  values <- lapply(seq_len(bands), function(b) drv + b)
  if (!is.null(nodata)) values[[1]][1:4, ] <- nodata
  asc <- vapply(seq_len(bands), function(b) {
    f <- tempfile(fileext = ".asc")
    writeLines(c(sprintf("ncols %d", size), sprintf("nrows %d", size), "xllcorner 410000",
                 "yllcorner 5470000", "cellsize 1", if (!is.null(nodata)) sprintf("NODATA_value %s", nodata),
                 apply(values[[b]], 1, paste, collapse = " ")), f)
    f
  }, "")
  vrt <- tempfile(fileext = ".vrt")
  sf::gdal_utils("buildvrt", asc, vrt, options = "-separate", quiet = TRUE)
  sf::gdal_utils("translate", vrt, path, quiet = TRUE,
                 options = c("-ot", type, "-a_srs", "EPSG:25832"))
  path
}

tile_at <- function(prepared, z) {
  lon <- mean(prepared$bbox[c("xmin", "xmax")])
  lat <- mean(prepared$bbox[c("ymin", "ymax")])
  x <- floor((lon + 180) / 360 * 2^z)
  y <- floor((1 - log(tan(lat * pi / 180) + 1 / cos(lat * pi / 180)) / pi) / 2 * 2^z)
  raster_tile(prepared, z, x, y)
}

png_bands <- function(path) {
  info <- jsonlite::fromJSON(sf::gdal_utils("info", path, options = "-json", quiet = TRUE), simplifyVector = FALSE)
  vapply(info$bands, `[[`, "", "colorInterpretation")
}

test_that("RGB rasters become RGBA tiles", {
  cache <- withr::local_tempdir()
  path <- write_raster(tempfile(fileext = ".tif"), bands = 3)
  info <- raster_info(path)
  expect_equal(info$name, tools::file_path_sans_ext(basename(path)))
  expect_equal(info$size, c(64L, 64L))
  expect_equal(info$bbox[["xmin"]], 7.76, tolerance = 0.01)
  expect_equal(info$bbox[["ymin"]], 49.38, tolerance = 0.01)
  expect_true(info$max_zoom %in% 17:18)

  prepared <- raster_prepare(info, cache)
  expect_false(prepared$needs_overviews)
  expect_true(file.exists(prepared$vrt))
  png <- tile_at(prepared, 17)
  expect_true(file.exists(png))
  expect_equal(png_bands(png), c("Red", "Green", "Blue", "Alpha"))
  # Second request comes from the cache.
  mtime <- file.mtime(png)
  expect_equal(file.mtime(tile_at(prepared, 17)), mtime)
})

test_that("floating-point rasters are stretched to grey with transparent nodata", {
  cache <- withr::local_tempdir()
  path <- write_raster(tempfile(fileext = ".tif"), bands = 1, type = "Float32", nodata = -9999)
  prepared <- raster_prepare(raster_info(path), cache)
  expect_equal(prepared$resampling, "bilinear")
  expect_equal(png_bands(tile_at(prepared, 17)), c("Gray", "Alpha"))
})

test_that("invalid tile coordinates are refused", {
  cache <- withr::local_tempdir()
  prepared <- raster_prepare(raster_info(write_raster(tempfile(fileext = ".tif"), bands = 1)), cache)
  expect_null(raster_tile(prepared, 2, 4, 0))
  expect_null(raster_tile(prepared, "a", 0, 0))
  expect_null(raster_tile(prepared, 1, -1, 0))
})

test_that("large rasters without overviews get them in the cache", {
  cache <- withr::local_tempdir()
  path <- write_raster(tempfile(fileext = ".tif"), bands = 1, size = 2100)
  prepared <- raster_prepare(raster_info(path), cache)
  expect_true(prepared$needs_overviews)
  raster_build_overviews(prepared$vrt)
  expect_true(file.exists(paste0(prepared$vrt, ".ovr")))
  expect_false(raster_prepare(raster_info(path), cache)$needs_overviews)
  # Nothing is written next to the source file.
  expect_equal(list.files(dirname(path), pattern = basename(path)), basename(path))
})

test_that("rasters without a coordinate system are rejected", {
  path <- tempfile(fileext = ".asc")
  writeLines(c("ncols 2", "nrows 2", "xllcorner 0", "yllcorner 0", "cellsize 1", "1 2", "3 4"), path)
  expect_error(raster_info(path), "kein Koordinatensystem")
})

test_that("mod_raster_server shows chosen GeoTIFFs as tile overlays", {
  dir <- withr::local_tempdir()
  cache <- withr::local_tempdir()
  write_raster(file.path(dir, "ortho.tif"), bands = 3)
  added <- list()
  map <- list(
    add_tiles = function(group, url, bbox, max_native_zoom) added[[group]] <<- url,
    remove_tiles = function(group) added[[group]] <<- NULL,
    fit = function(features) NULL
  )
  shiny::testServer(mod_raster_server, args = list(map = map, roots = c(Test = dir), cache_dir = cache), {
    session$setInputs(pick = list(files = list(`0` = list("", "ortho.tif")), root = "Test"))
    expect_named(added, "ortho")
    expect_match(added$ortho, "z=\\{z\\}&x=\\{x\\}&y=\\{y\\}$")
    expect_length(session$returned(), 1)

    session$setInputs(remove = names(session$returned()))
    expect_length(added, 0)
    expect_length(session$returned(), 0)
  })
})

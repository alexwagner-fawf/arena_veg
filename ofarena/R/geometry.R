#' Parse Arena spatial reference codes
#'
#' Arena stores the reference system of each coordinate as a code such as
#' `"4326"`; `"EPSG:4326"` is accepted as well.
#'
#' @param x Character vector.
#' @return Integer EPSG codes, `NA` where `x` cannot be parsed.
#' @export
parse_srs <- function(x) {
  code <- sub("^\\s*(epsg:)?\\s*", "", tolower(as.character(x)))
  suppressWarnings(as.integer(ifelse(grepl("^[0-9]+$", code), code, NA)))
}

#' Point geometries from Arena coordinate columns
#'
#' Builds points from `<prefix>_x`, `<prefix>_y` and `<prefix>_srs`. Rows may
#' use different reference systems; each group is transformed to `crs_out`.
#'
#' @param df A data frame.
#' @param prefix Attribute name, e.g. `"curr_position"`.
#' @param crs_out Output CRS.
#' @param keep_empty Keep rows without a usable coordinate (as empty points)
#'   instead of dropping them.
#' @return An `sf` object in `crs_out`.
#' @export
arena_points_sf <- function(df, prefix, crs_out = 4326, keep_empty = FALSE) {
  x <- df[[paste0(prefix, "_x")]]
  y <- df[[paste0(prefix, "_y")]]
  srs <- parse_srs(df[[paste0(prefix, "_srs")]])
  if (is.null(x) || is.null(y)) {
    rlang::abort(paste0("`df` has no columns ", prefix, "_x / ", prefix, "_y."))
  }
  ok <- !is.na(x) & !is.na(y) & !is.na(srs)

  geom <- rep(list(sf::st_point()), nrow(df))
  for (code in unique(srs[ok])) {
    idx <- which(ok & srs == code)
    pts <- sf::st_sfc(lapply(idx, function(i) sf::st_point(c(x[i], y[i]))), crs = code)
    geom[idx] <- unclass(sf::st_transform(pts, crs_out))
  }
  out <- sf::st_sf(df, geometry = sf::st_sfc(geom, crs = crs_out))
  if (keep_empty) out else out[ok, ]
}

#' Local metric CRS for a location
#'
#' The WGS 84 / UTM zone containing the mean of the given coordinates. Good
#' enough for offsets of a few hundred metres.
#'
#' @param lon,lat Longitude and latitude in degrees.
#' @return An EPSG code.
#' @export
utm_crs <- function(lon, lat) {
  lon <- mean(lon, na.rm = TRUE)
  lat <- mean(lat, na.rm = TRUE)
  zone <- floor((lon + 180) / 6) %% 60 + 1
  as.integer(if (lat >= 0) 32600 + zone else 32700 + zone)
}

metric_crs_for <- function(pts, crs_metric) {
  if (!is.null(crs_metric)) return(crs_metric)
  ll <- sf::st_coordinates(sf::st_transform(pts[!sf::st_is_empty(pts)], 4326))
  if (nrow(ll) == 0) return(3857L)
  utm_crs(ll[, 1], ll[, 2])
}

as_sfc <- function(pts) {
  if (inherits(pts, "sf")) sf::st_geometry(pts) else pts
}

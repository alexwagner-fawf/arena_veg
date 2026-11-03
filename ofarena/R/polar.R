#' Polar offsets in gon
#'
#' Forestry field protocols locate objects by bearing and horizontal distance
#' from a centre point. Bearings are in gon (400 gon = full circle), measured
#' clockwise from north.
#'
#' `polar_xy()` works on plain metric coordinates. `polar_offset()` takes
#' points in any CRS, applies the offsets in a metric CRS and returns points in
#' the input CRS.
#'
#' @param x,y Metric coordinates of the centre (east, north).
#' @param bearing_gon Bearing in gon, clockwise from north.
#' @param dist_m Horizontal distance in metres.
#' @return `polar_xy()`: a list with `x` and `y`.
#' @export
polar_xy <- function(x, y, bearing_gon, dist_m) {
  rad <- bearing_gon * pi / 200
  list(x = x + dist_m * sin(rad), y = y + dist_m * cos(rad))
}

#' @rdname polar_xy
#' @param pts Centre points (`sf` or `sfc` POINT).
#' @param crs_metric Metric CRS for the calculation. Defaults to the local UTM
#'   zone ([utm_crs()]).
#' @return `polar_offset()`: an `sfc` of points in the CRS of `pts`; empty
#'   points where the centre, bearing or distance is missing.
#' @export
polar_offset <- function(pts, bearing_gon, dist_m, crs_metric = NULL) {
  pts <- as_sfc(pts)
  crs_in <- sf::st_crs(pts)
  n <- length(pts)
  bearing_gon <- rep_len(bearing_gon, n)
  dist_m <- rep_len(dist_m, n)
  ok <- !sf::st_is_empty(pts) & !is.na(bearing_gon) & !is.na(dist_m)

  geom <- rep(list(sf::st_point()), n)
  if (any(ok)) {
    crs_metric <- metric_crs_for(pts[ok], crs_metric)
    xy <- sf::st_coordinates(sf::st_transform(pts[ok], crs_metric))
    moved <- polar_xy(xy[, 1], xy[, 2], bearing_gon[ok], dist_m[ok])
    new <- sf::st_sfc(lapply(seq_along(moved$x), function(i) sf::st_point(c(moved$x[i], moved$y[i]))), crs = crs_metric)
    geom[ok] <- unclass(sf::st_transform(new, crs_in))
  }
  sf::st_sfc(geom, crs = crs_in)
}

#' Polygon from distances measured along rays
#'
#' Builds one polygon per centre from distances measured at fixed bearings,
#' e.g. the extent of a regeneration patch at 50, 100, ..., 400 gon. Rays with
#' a missing or non-positive distance are skipped; centres with fewer than
#' `min_rays` usable rays give an empty polygon.
#'
#' @param pts Centre points (`sf` or `sfc` POINT).
#' @param bearings_gon Bearings of the rays in gon, one per column of `dists`.
#' @param dists Numeric matrix (or data frame) of distances in metres, one row
#'   per centre.
#' @param crs_metric Metric CRS for the calculation, as in [polar_offset()].
#' @param min_rays Minimum number of usable rays.
#' @return An `sfc` of polygons in the CRS of `pts`.
#' @export
polygon_from_rays <- function(pts, bearings_gon, dists, crs_metric = NULL, min_rays = 3) {
  pts <- as_sfc(pts)
  dists <- as.matrix(dists)
  if (ncol(dists) != length(bearings_gon)) {
    rlang::abort("`dists` needs one column per bearing.")
  }
  crs_in <- sf::st_crs(pts)
  geom <- rep(list(sf::st_polygon()), length(pts))
  usable <- rowSums(!is.na(dists) & dists > 0) >= min_rays & !sf::st_is_empty(pts)

  if (any(usable)) {
    crs_metric <- metric_crs_for(pts[usable], crs_metric)
    xy <- sf::st_coordinates(sf::st_transform(pts[usable], crs_metric))
    # Decreasing bearing = counter-clockwise, the conventional exterior ring order.
    order_rays <- order(bearings_gon %% 400, decreasing = TRUE)
    polys <- lapply(seq_len(nrow(xy)), function(i) {
      d <- dists[which(usable)[i], order_rays]
      b <- (bearings_gon %% 400)[order_rays]
      keep <- !is.na(d) & d > 0
      ring <- polar_xy(xy[i, 1], xy[i, 2], b[keep], d[keep])
      ring <- cbind(ring$x, ring$y)
      sf::st_polygon(list(rbind(ring, ring[1, ])))
    })
    geom[usable] <- unclass(sf::st_transform(sf::st_sfc(polys, crs = crs_metric), crs_in))
  }
  sf::st_sfc(geom, crs = crs_in)
}

#' Circles with a radius in metres
#'
#' @param pts Centre points (`sf` or `sfc` POINT).
#' @param radius_m Radius in metres, recycled. Missing radii give empty
#'   polygons.
#' @param crs_metric Metric CRS for the calculation, as in [polar_offset()].
#' @param n_segments Segments per quarter circle.
#' @return An `sfc` of polygons in the CRS of `pts`.
#' @export
buffer_m <- function(pts, radius_m, crs_metric = NULL, n_segments = 16) {
  pts <- as_sfc(pts)
  crs_in <- sf::st_crs(pts)
  radius_m <- rep_len(radius_m, length(pts))
  ok <- !sf::st_is_empty(pts) & !is.na(radius_m) & radius_m > 0

  geom <- rep(list(sf::st_polygon()), length(pts))
  if (any(ok)) {
    crs_metric <- metric_crs_for(pts[ok], crs_metric)
    circles <- sf::st_buffer(sf::st_transform(pts[ok], crs_metric), radius_m[ok], nQuadSegs = n_segments)
    geom[ok] <- unclass(sf::st_transform(circles, crs_in))
  }
  sf::st_sfc(geom, crs = crs_in)
}

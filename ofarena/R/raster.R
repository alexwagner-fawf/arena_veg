#' Describe a raster file
#'
#' Reads the header of a GeoTIFF (or any raster GDAL can open) with
#' approximate band statistics. Nothing is written next to the file, so it
#' works on read-only network drives.
#'
#' @param path Raster file.
#' @return A list with `path` (normalised), `name` (file name without
#'   extension), `key` (identifies path, size and modification time; changes
#'   when the file changes), `size` (`c(columns, rows)`), `bands` (a list per
#'   band with `type`, `colour`, `nodata`, `min`, `max`, `overviews`),
#'   `palette` (first band has a colour table), `bbox` (WGS 84 bounding
#'   box: `xmin`, `ymin`, `xmax`, `ymax`) and `max_zoom` (web map zoom
#'   level matching the native resolution).
#' @export
raster_info <- function(path) {
  path <- normalizePath(path, mustWork = TRUE)
  json <- sf::gdal_utils("info", path, options = c("-json", "-approx_stats"), quiet = TRUE,
                         config_options = c(GDAL_PAM_ENABLED = "NO"))
  info <- jsonlite::fromJSON(json, simplifyVector = FALSE)
  if (is.null(info$wgs84Extent) || is.null(info$coordinateSystem$wkt) || !nzchar(info$coordinateSystem$wkt)) {
    stop("Die Datei hat kein Koordinatensystem: ", basename(path), call. = FALSE)
  }

  bands <- lapply(info$bands, function(b) list(
    type = b$type, colour = b$colorInterpretation %||% "Undefined",
    nodata = if (is.numeric(b$noDataValue)) b$noDataValue else NA_real_,
    min = b$minimum %||% NA_real_, max = b$maximum %||% NA_real_,
    overviews = length(b$overviews)
  ))
  ring <- matrix(unlist(info$wgs84Extent$coordinates[[1]]), ncol = 2, byrow = TRUE)
  bbox <- c(xmin = min(ring[, 1]), ymin = min(ring[, 2]), xmax = max(ring[, 1]), ymax = max(ring[, 2]))
  size <- as.integer(unlist(info$size))

  # Pixel size in web mercator units -> zoom level where one tile pixel is
  # about one raster pixel. Leaflet enlarges tiles beyond that zoom.
  merc <- sf::st_bbox(sf::st_transform(sf::st_as_sfc(sf::st_bbox(bbox, crs = sf::st_crs(4326))), 3857))
  pixel <- (merc[["xmax"]] - merc[["xmin"]]) / size[1]
  max_zoom <- min(map_max_zoom, max(0L, as.integer(ceiling(log2(2 * pi * 6378137 / (256 * pixel))))))

  stat <- file.info(path)
  list(
    path = path,
    name = tools::file_path_sans_ext(basename(path)),
    key = digest_key(path, stat$size, as.numeric(stat$mtime)),
    size = size,
    bands = bands,
    palette = length(info$bands) > 0 && !is.null(info$bands[[1]]$colorTable),
    bbox = bbox,
    max_zoom = max_zoom
  )
}

digest_key <- function(...) {
  tmp <- tempfile()
  on.exit(unlink(tmp))
  writeLines(paste(..., sep = "\r"), tmp)
  unname(tools::md5sum(tmp))
}

#' Prepare a raster for serving map tiles
#'
#' Writes `<cache_dir>/<key>/source.vrt`, a virtual raster that turns the
#' file into 8-bit RGB(A) or grey values without copying it:
#' colour tables are expanded, 16-bit and floating-point bands are stretched
#' from their minimum to maximum, and a 4th band is kept as transparency
#' when it is an alpha band. Tiles are cut from this VRT by [raster_tile()].
#'
#' Large rasters without overviews would have to be read completely for
#' every zoomed-out tile. For those, `needs_overviews` is `TRUE` and
#' [raster_build_overviews()] should be run once (it reads the file once
#' and writes the overviews into the cache, not next to the file).
#'
#' @param info Result of [raster_info()].
#' @param cache_dir Tile cache directory.
#' @return `info` with `dir` (cache directory of this raster), `vrt`,
#'   `resampling` and `needs_overviews` added.
#' @export
raster_prepare <- function(info, cache_dir) {
  dir <- file.path(cache_dir, info$key)
  vrt <- file.path(dir, "source.vrt")
  bands <- info$bands
  n <- length(bands)
  byte <- all(vapply(bands, function(b) b$type == "Byte", TRUE))
  alpha <- n >= 2 && bands[[n]]$colour == "Alpha"

  if (info$palette) {
    opts <- c("-b", "1", "-expand", "rgba")
  } else {
    keep <- if (n >= 3) 1:3 else 1L
    opts <- as.vector(rbind("-b", c(keep, if (alpha) n)))
    if (!byte) {
      # One stretch for all colour bands keeps their balance; 0 stays nodata.
      lo <- min(vapply(bands[keep], `[[`, 0, "min"), na.rm = TRUE)
      hi <- max(vapply(bands[keep], `[[`, 0, "max"), na.rm = TRUE)
      if (!is.finite(lo) || !is.finite(hi) || hi <= lo) hi <- lo + 1
      opts <- c(opts, "-ot", "Byte", "-scale", format(lo, digits = 15), format(hi, digits = 15), "1", "255")
      if (!alpha) opts <- c(opts, "-a_nodata", "0")
    }
  }

  if (!file.exists(vrt)) {
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    tmp <- tempfile(tmpdir = dir, fileext = ".vrt")
    sf::gdal_utils("translate", info$path, tmp, options = c("-of", "VRT", opts), quiet = TRUE,
                   config_options = c(GDAL_PAM_ENABLED = "NO"))
    file.rename(tmp, vrt)
  }

  has_overviews <- all(vapply(bands, `[[`, 0L, "overviews") > 0)
  info$dir <- dir
  info$vrt <- vrt
  # Classes must not be blended into colours that do not exist.
  info$resampling <- if (info$palette) "near" else "bilinear"
  info$needs_overviews <- !has_overviews && max(info$size) > 2048 && !file.exists(paste0(vrt, ".ovr"))
  info
}

#' Build overviews for a prepared raster
#'
#' Writes `source.vrt.ovr` next to the VRT from [raster_prepare()], atomically,
#' so several processes may build at the same time. This reads the whole
#' file once; run it in a background process for large files (see
#' [mod_raster_server()]).
#'
#' @param vrt Path of the prepared VRT.
#' @return `vrt`, invisibly.
#' @export
raster_build_overviews <- function(vrt) {
  info <- jsonlite::fromJSON(sf::gdal_utils("info", vrt, options = "-json", quiet = TRUE), simplifyVector = FALSE)
  size <- max(unlist(info$size))
  levels <- 2^seq_len(max(1, ceiling(log2(size / 256))))
  work <- tempfile(tmpdir = dirname(vrt), fileext = ".vrt")
  on.exit(unlink(c(work, paste0(work, ".ovr"))))
  file.copy(vrt, work)
  sf::gdal_addo(work, overviews = levels, method = "AVERAGE", read_only = TRUE,
                config_options = c(COMPRESS_OVERVIEW = "DEFLATE", GDAL_PAM_ENABLED = "NO"))
  file.rename(paste0(work, ".ovr"), paste0(vrt, ".ovr"))
  invisible(vrt)
}

#' Cut one web map tile from a prepared raster
#'
#' Reprojects the tile area to web mercator (EPSG:3857) and writes a
#' 256 x 256 PNG with transparency outside the raster and for nodata.
#' Tiles are cached below the raster's cache directory.
#'
#' @param prepared Result of [raster_prepare()].
#' @param z,x,y Tile coordinates (XYZ scheme, as used by leaflet).
#' @return Path of the PNG, or `NULL` for invalid tile coordinates.
#' @export
raster_tile <- function(prepared, z, x, y) {
  z <- suppressWarnings(as.integer(z))
  x <- suppressWarnings(as.integer(x))
  y <- suppressWarnings(as.integer(y))
  if (anyNA(c(z, x, y)) || z < 0 || z > map_max_zoom || x < 0 || y < 0 || x >= 2^z || y >= 2^z) return(NULL)

  png <- file.path(prepared$dir, z, x, paste0(y, ".png"))
  if (file.exists(png)) return(png)

  origin <- 20037508.342789244
  step <- 2 * origin / 2^z
  extent <- c(-origin + x * step, origin - (y + 1) * step, -origin + (x + 1) * step, origin - y * step)
  # sf's gdalwarp crashes when writing a VRT, so warp into a small GeoTIFF.
  warped <- tempfile(fileext = ".tif")
  on.exit(unlink(warped))
  alpha <- prepared$palette || (length(prepared$bands) >= 2 && prepared$bands[[length(prepared$bands)]]$colour == "Alpha")
  sf::gdal_utils(
    "warp", prepared$vrt, warped, quiet = TRUE,
    options = c("-of", "GTiff", "-t_srs", "EPSG:3857", "-te", format(extent, digits = 17),
                "-ts", "256", "256", "-r", prepared$resampling, if (!alpha) "-dstalpha")
  )
  dir.create(dirname(png), recursive = TRUE, showWarnings = FALSE)
  tmp <- tempfile(tmpdir = dirname(png), fileext = ".part")
  sf::gdal_utils("translate", warped, tmp, options = c("-of", "PNG"), quiet = TRUE,
                 config_options = c(GDAL_PAM_ENABLED = "NO"))
  file.rename(tmp, png)
  png
}

#' Serve raster tiles to the browser
#'
#' Registers a per-session URL that answers leaflet tile requests for rasters
#' registered with `add()`. Only registered rasters are served.
#'
#' @param session Shiny session.
#' @return A list with `add(prepared)`, which registers a result of
#'   [raster_prepare()] and returns the tile URL template
#'   (`...&z={z}&x={x}&y={y}`), and `remove(key)`.
#' @export
raster_tile_server <- function(session = shiny::getDefaultReactiveDomain()) {
  rasters <- new.env(parent = emptyenv())
  base <- session$registerDataObj("arena_raster", NULL, function(data, req) {
    q <- shiny::parseQueryString(req$QUERY_STRING)
    prepared <- if (is.character(q$k) && length(q$k) == 1) rasters[[q$k]]
    path <- if (!is.null(prepared)) tryCatch(raster_tile(prepared, q$z, q$x, q$y), error = function(e) NULL)
    if (is.null(path)) {
      return(shiny::httpResponse(404L, "text/plain", "Kachel nicht gefunden"))
    }
    shiny::httpResponse(200L, "image/png", readBin(path, "raw", file.size(path)),
                        headers = list("Cache-Control" = "private, max-age=86400"))
  })
  list(
    add = function(prepared) {
      assign(prepared$key, prepared, envir = rasters)
      paste0(base, "&k=", prepared$key, "&z={z}&x={x}&y={y}")
    },
    remove = function(key) if (exists(key, envir = rasters, inherits = FALSE)) rm(list = key, envir = rasters)
  )
}

# Overview builds running in this R process, by raster key. Shared by all
# sessions, so a file opened in two sessions is read only once.
raster_jobs <- new.env(parent = emptyenv())

#' GeoTIFF overlays for the map module
#'
#' A button opens a file browser (shinyFiles) on the given root folders,
#' e.g. a mounted network drive. Chosen GeoTIFFs are shown on the map as
#' overlays named after the file, served as tiles from a disk cache. Large
#' files without overviews get them built once in a background process
#' before they appear.
#'
#' @param id Module id.
#' @export
mod_raster_ui <- function(id) {
  ns <- shiny::NS(id)
  htmltools::div(
    class = "d-flex flex-wrap align-items-center gap-2",
    shinyFiles::shinyFilesButton(ns("pick"), "GeoTIFF laden", "GeoTIFF-Dateien wählen", multiple = TRUE,
                                 icon = shiny::icon("layer-group"), class = "btn-sm btn-outline-secondary"),
    shiny::uiOutput(ns("loaded"), class = "d-flex flex-wrap gap-1")
  )
}

#' @rdname mod_raster_ui
#' @param map Result of [mod_map_server()].
#' @param roots Named character vector of root folders the file browser may
#'   show (names are shown to the user).
#' @param cache_dir Tile cache directory; must be writable.
#' @return A reactive with the loaded rasters (list of [raster_prepare()]
#'   results, by key).
#' @export
mod_raster_server <- function(id, map, roots, cache_dir) {
  shiny::moduleServer(id, function(input, output, session) {
    roots <- vapply(roots, function(r) normalizePath(path.expand(r), mustWork = FALSE), "")
    shinyFiles::shinyFileChoose(input, "pick", roots = roots, session = session,
                                filetypes = c("tif", "tiff", "TIF", "TIFF"))
    tiles <- raster_tile_server(session)
    loaded <- shiny::reactiveVal(list())
    pending <- shiny::reactiveVal(list())

    show <- function(prepared) {
      url <- tiles$add(prepared)
      group <- unique_group(prepared$name, vapply(loaded(), `[[`, "", "group"))
      prepared$group <- group
      map$add_tiles(group, url, prepared$bbox, max_native_zoom = prepared$max_zoom)
      map$fit(sf::st_as_sf(sf::st_as_sfc(sf::st_bbox(prepared$bbox, crs = sf::st_crs(4326)))))
      l <- loaded()
      l[[prepared$key]] <- prepared
      loaded(l)
    }

    shiny::observeEvent(input$pick, {
      files <- shinyFiles::parseFilePaths(roots, input$pick)
      for (path in files$datapath) {
        prepared <- tryCatch(raster_prepare(raster_info(path), cache_dir), error = identity)
        if (inherits(prepared, "error")) {
          shiny::showNotification(paste0(basename(path), ": ", conditionMessage(prepared)), type = "error", duration = 10)
          next
        }
        if (prepared$key %in% c(names(loaded()), names(pending()))) next
        if (!prepared$needs_overviews) {
          show(prepared)
          next
        }
        if (is.null(raster_jobs[[prepared$key]])) {
          raster_jobs[[prepared$key]] <- callr::r_bg(
            function(vrt) ofarena::raster_build_overviews(vrt), args = list(vrt = prepared$vrt), supervise = TRUE
          )
        }
        p <- pending()
        p[[prepared$key]] <- prepared
        pending(p)
        shiny::showNotification(
          sprintf("%s wird für die Karte vorbereitet (einmalig, %s Pixel) …",
                  prepared$name, paste(prepared$size, collapse = " × ")),
          id = prepared$key, duration = NULL, closeButton = FALSE
        )
      }
    })

    # Watch overview builds; show each raster when its build is done.
    shiny::observe({
      p <- pending()
      if (length(p) == 0) return()
      shiny::invalidateLater(1000)
      for (key in names(p)) {
        job <- raster_jobs[[key]]
        if (!is.null(job) && job$is_alive()) next
        result <- if (!is.null(job)) tryCatch({ job$get_result(); NULL }, error = identity)
        rm(list = intersect(key, ls(raster_jobs)), envir = raster_jobs)
        shiny::removeNotification(key)
        if (inherits(result, "error")) {
          shiny::showNotification(paste0(p[[key]]$name, ": ", conditionMessage(result)), type = "error", duration = 10)
        } else {
          p[[key]]$needs_overviews <- FALSE
          show(p[[key]])
        }
        p[[key]] <- NULL
      }
      pending(p)
    })

    output$loaded <- shiny::renderUI({
      l <- loaded()
      lapply(names(l), function(key) htmltools::span(
        class = "badge text-bg-light border d-inline-flex align-items-center gap-1",
        l[[key]]$group,
        htmltools::tags$a(href = "#", title = "Entfernen", class = "text-secondary", shiny::icon("xmark"),
                          onclick = sprintf("Shiny.setInputValue('%s', '%s', {priority: 'event'}); return false;",
                                            session$ns("remove"), key))
      ))
    })

    shiny::observeEvent(input$remove, {
      l <- loaded()
      key <- input$remove
      if (is.null(l[[key]])) return()
      map$remove_tiles(l[[key]]$group)
      tiles$remove(key)
      l[[key]] <- NULL
      loaded(l)
    })

    loaded
  })
}

unique_group <- function(name, taken) {
  group <- name
  i <- 1
  while (group %in% taken) {
    i <- i + 1
    group <- sprintf("%s (%d)", name, i)
  }
  group
}

#' File references in a data table
#'
#' Arena flattens a file attribute `foo` into `foo_file_uuid` and
#' `foo_file_name`. This collects all of them into one long table.
#'
#' @param df A data frame from [arena_read_entity()].
#' @param id_col Column identifying the row (kept as `entity_uuid`).
#' @return A tibble with `source_survey` (if present in `df`), `entity_uuid`,
#'   `field` (attribute name), `file_uuid` and `file_name`, one row per
#'   referenced file.
#' @export
arena_file_cols <- function(df, id_col = "uuid") {
  uuid_cols <- grep("_file_uuid$", names(df), value = TRUE)
  empty <- tibble::tibble(
    source_survey = integer(), entity_uuid = character(), field = character(),
    file_uuid = character(), file_name = character()
  )
  parts <- lapply(uuid_cols, function(col) {
    field <- sub("_file_uuid$", "", col)
    name_col <- paste0(field, "_file_name")
    has <- !is.na(df[[col]])
    tibble::tibble(
      source_survey = if ("source_survey" %in% names(df)) df$source_survey[has] else NA_integer_,
      entity_uuid = as.character(df[[id_col]][has]),
      field = rep(field, sum(has)),
      file_uuid = as.character(df[[col]][has]),
      file_name = if (name_col %in% names(df)) as.character(df[[name_col]][has]) else NA_character_
    )
  })
  out <- dplyr::bind_rows(empty, parts)
  if (!"source_survey" %in% names(df)) out$source_survey <- NULL
  out
}

#' File metadata of a survey
#'
#' Reads only the properties, never the content.
#'
#' @param con A DBI connection or pool.
#' @param survey_id Survey id.
#' @return A tibble with `file_uuid`, `name`, `size`, `record_uuid`,
#'   `node_uuid`, `deleted` and `has_content`.
#' @export
arena_file_meta <- function(con, survey_id) {
  sql <- paste0("
    SELECT uuid::text AS file_uuid, props->>'name' AS name, (props->>'size')::bigint AS size,
           props->>'recordUuid' AS record_uuid, props->>'nodeUuid' AS node_uuid,
           COALESCE((props->>'deleted')::boolean, FALSE) AS deleted,
           content IS NOT NULL AS has_content
    FROM ", qualified(con, survey_schema(survey_id), "file"))
  tibble::as_tibble(DBI::dbGetQuery(con, sql))
}

image_exts <- c("jpg", "jpeg", "png", "gif", "webp", "bmp", "tif", "tiff", "heic")

file_ext <- function(name) {
  ext <- tolower(sub(".*\\.", "", name))
  ifelse(is.na(name) | !grepl("\\.", name) | !grepl("^[a-z0-9]{1,5}$", ext), "bin", ext)
}

#' Fetch a file into the disk cache
#'
#' Reads `content` from `survey_<id>.file` once and keeps it under
#' `<cache_dir>/<survey_id>/`. For images a thumbnail (`<uuid>_thumb.jpg`,
#' rotated according to EXIF) is written as well when the magick package is
#' installed; otherwise the original doubles as thumbnail. Files are written
#' to a temporary name and renamed, so concurrent sessions never see partial
#' files.
#'
#' @param con A DBI connection or pool.
#' @param survey_id Survey id.
#' @param file_uuid File uuid.
#' @param cache_dir Cache directory.
#' @param thumb Return the thumbnail rather than the original.
#' @param thumb_width Thumbnail width in pixels.
#' @return The path of the cached file, or `NULL` if the file does not exist
#'   or has no content.
#' @export
arena_file_fetch <- function(con, survey_id, file_uuid, cache_dir, thumb = FALSE, thumb_width = 360) {
  survey_id <- as_survey_id(survey_id)
  if (length(file_uuid) != 1 || !grepl("^[0-9a-fA-F-]{36}$", file_uuid)) return(NULL)
  dir <- file.path(cache_dir, survey_id)
  cached <- list.files(dir, pattern = paste0("^", file_uuid, "\\.[a-z0-9]+$"), full.names = TRUE)

  if (length(cached) == 0) {
    sql <- paste0("SELECT props->>'name' AS name, content FROM ",
                  qualified(con, survey_schema(survey_id), "file"), " WHERE uuid = $1::uuid")
    row <- DBI::dbGetQuery(con, sql, params = list(file_uuid))
    if (nrow(row) == 0 || is.null(row$content[[1]])) return(NULL)
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    cached <- file.path(dir, paste0(file_uuid, ".", file_ext(row$name)))
    write_atomic(row$content[[1]], cached)
  }
  cached <- cached[[1]]
  if (!thumb || !file_ext(cached) %in% image_exts || !requireNamespace("magick", quietly = TRUE)) {
    return(cached)
  }

  thumb_path <- file.path(dir, paste0(file_uuid, "_thumb.jpg"))
  if (!file.exists(thumb_path)) {
    # For JPEGs, let libjpeg decode at reduced size: about 3x faster for camera photos.
    hint <- if (file_ext(cached) %in% c("jpg", "jpeg")) c("jpeg:size" = paste0(2 * thumb_width, "x", 2 * thumb_width))
    img <- tryCatch(magick::image_read(cached, defines = hint), error = function(e) NULL)
    if (is.null(img)) return(cached)
    img <- magick::image_scale(magick::image_orient(img[1]), as.character(thumb_width))
    write_atomic(magick::image_write(img, format = "jpeg", quality = 80), thumb_path)
  }
  thumb_path
}

write_atomic <- function(bytes, path) {
  tmp <- tempfile(tmpdir = dirname(path), fileext = ".part")
  writeBin(bytes, tmp)
  file.rename(tmp, path)
}

#' Serve Arena files to the browser
#'
#' Registers a per-session URL that streams files from the disk cache,
#' fetching them from the database on first request ([arena_file_fetch()]).
#' Images therefore load only when the browser actually shows them (popups,
#' lazy `<img>` tags), not when a page is rendered.
#'
#' @param pool A DBI pool (connections are checked out per request).
#' @param cache_dir Cache directory.
#' @param session Shiny session.
#' @return A function `url(survey_id, file_uuid, thumb = FALSE)` returning
#'   URLs, vectorised over its arguments.
#' @export
arena_file_server <- function(pool, cache_dir, session = shiny::getDefaultReactiveDomain()) {
  base <- session$registerDataObj("arena_file", NULL, function(data, req) {
    q <- shiny::parseQueryString(req$QUERY_STRING)
    path <- tryCatch(
      arena_file_fetch(pool, q$s, q$f, cache_dir, thumb = identical(q$t, "1")),
      error = function(e) NULL
    )
    if (is.null(path)) {
      return(shiny::httpResponse(404L, "text/plain", "Datei nicht gefunden"))
    }
    shiny::httpResponse(
      200L,
      mime::guess_type(path),
      readBin(path, "raw", file.size(path)),
      headers = list("Cache-Control" = "private, max-age=86400")
    )
  })

  function(survey_id, file_uuid, thumb = FALSE) {
    ifelse(
      is.na(file_uuid), NA_character_,
      paste0(base, "&s=", survey_id, "&f=", file_uuid, "&t=", as.integer(thumb))
    )
  }
}

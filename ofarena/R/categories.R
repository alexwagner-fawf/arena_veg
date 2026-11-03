#' Category items of a survey
#'
#' Arena's flat data tables only carry the code and label of a category item.
#' Extra properties of items (e.g. the target location of a sample plot) are
#' only available here.
#'
#' @param con A DBI connection or pool.
#' @param survey_id Survey id.
#' @param category_uuid Category uuid (see `category_uuid` in
#'   [arena_node_defs()]), or `NULL` for all categories.
#' @param lang Preferred label language.
#' @return A tibble with `category_uuid`, `category`, `level` (0 = top),
#'   `item_uuid`, `code`, `label`, `parent_code` and one column per extra
#'   property. Point properties (EWKT text or `{x, y, srs}` objects) are split
#'   into `<prop>_x`, `<prop>_y` and `<prop>_srs`.
#' @export
arena_category_items <- function(con, survey_id, category_uuid = NULL, lang = "de") {
  schema <- survey_schema(survey_id)
  sql <- paste0("
    SELECT c.uuid::text AS category_uuid, c.props->>'name' AS category, l.index AS level,
           i.uuid::text AS item_uuid, i.props->>'code' AS code, i.props->'labels' AS labels,
           p.props->>'code' AS parent_code, COALESCE(i.props->'extra', '{}') AS extra
    FROM ", qualified(con, schema, "category"), " c
    JOIN ", qualified(con, schema, "category_level"), " l ON l.category_uuid = c.uuid
    JOIN ", qualified(con, schema, "category_item"), " i ON i.level_uuid = l.uuid
    LEFT JOIN ", qualified(con, schema, "category_item"), " p ON p.uuid = i.parent_uuid
    WHERE $1::uuid IS NULL OR c.uuid = $1::uuid
    ORDER BY c.props->>'name', l.index, i.id")
  rows <- DBI::dbGetQuery(con, sql, params = list(category_uuid %||% NA_character_))
  labels <- lapply(rows$labels, jsonlite::fromJSON, simplifyVector = FALSE)
  extra <- lapply(rows$extra, jsonlite::fromJSON, simplifyVector = FALSE)

  out <- tibble::tibble(
    category_uuid = rows$category_uuid,
    category = rows$category,
    level = as.integer(rows$level),
    item_uuid = rows$item_uuid,
    code = rows$code,
    label = mapply(function(l, code) pick_label(l, lang, code), labels, rows$code, USE.NAMES = FALSE),
    parent_code = rows$parent_code
  )
  for (prop in unique(unlist(lapply(extra, names)))) {
    # Points come as EWKT text or as {x, y, srs} objects; the latter are
    # turned into EWKT first.
    values <- vapply(extra, function(e) {
      v <- e[[prop]]
      if (is.list(v) && !is.null(v$x) && !is.null(v$y)) {
        return(sprintf("SRID=%s;POINT(%.10g %.10g)", v$srs %||% "4326", as.numeric(v$x), as.numeric(v$y)))
      }
      if (is.null(v) || length(v) != 1) NA_character_ else as.character(v)
    }, character(1))
    point <- parse_ewkt_point(values)
    if (any(!is.na(point$srs))) {
      out[[paste0(prop, "_x")]] <- point$x
      out[[paste0(prop, "_y")]] <- point$y
      out[[paste0(prop, "_srs")]] <- point$srs
    } else {
      out[[prop]] <- values
    }
  }
  out
}

#' Parse EWKT points
#'
#' @param x Character vector like `"SRID=25832;POINT(394170.5 5620370.4)"`.
#' @return A list with numeric `x`, `y` and character `srs`; `NA` where `x`
#'   is not an EWKT point.
#' @noRd
parse_ewkt_point <- function(x) {
  pattern <- "^\\s*SRID=([0-9]+);\\s*POINT\\s*\\(\\s*([-0-9.eE+]+)\\s+([-0-9.eE+]+)\\s*\\)\\s*$"
  ok <- !is.na(x) & grepl(pattern, x, ignore.case = TRUE)
  part <- function(i) ifelse(ok, sub(pattern, paste0("\\", i), x, ignore.case = TRUE), NA_character_)
  list(x = as.numeric(part(2)), y = as.numeric(part(3)), srs = part(1))
}

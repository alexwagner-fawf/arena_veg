#' Node definitions (entities and attributes) of a survey
#'
#' Uses the published properties (`props`), which are what the data tables
#' reflect. Deleted definitions are dropped.
#'
#' @param con A DBI connection or pool.
#' @param survey_id Survey id.
#' @param lang Preferred label language.
#' @return A tibble with `uuid`, `parent_uuid`, `type`, `name`, `label`,
#'   `multiple`, `key` and `category_uuid` (code attributes).
#' @export
arena_node_defs <- function(con, survey_id, lang = "de") {
  sql <- paste0("
    SELECT uuid::text, parent_uuid::text, type, props::text AS props
    FROM ", qualified(con, survey_schema(survey_id), "node_def"), "
    WHERE NOT deleted AND props ? 'name'
    ORDER BY id")
  rows <- DBI::dbGetQuery(con, sql)
  props <- lapply(rows$props, jsonlite::fromJSON, simplifyVector = FALSE)
  name <- vapply(props, function(p) p$name, character(1))

  tibble::tibble(
    uuid = rows$uuid,
    parent_uuid = rows$parent_uuid,
    type = rows$type,
    name = name,
    label = mapply(function(p, n) pick_label(p$labels, lang, n), props, name, USE.NAMES = FALSE),
    multiple = vapply(props, function(p) isTRUE(p$multiple), logical(1)),
    key = vapply(props, function(p) isTRUE(p$key), logical(1)),
    category_uuid = vapply(props, function(p) p$categoryUuid %||% NA_character_, character(1))
  )
}

# Suffixes Arena appends to an attribute name when flattening it into columns.
column_suffixes <- c(
  "_label", "_file_uuid", "_file_name", "_x", "_y", "_srs", "_accuracy",
  "_scientific_name", "_vernacular_name", "_code"
)
suffix_labels <- c(
  "_label" = "", "_file_uuid" = " (Datei-ID)", "_file_name" = " (Datei)", "_x" = " X",
  "_y" = " Y", "_srs" = " SRS", "_accuracy" = " Genauigkeit", "_scientific_name" = "",
  "_vernacular_name" = " (Trivialname)", "_code" = " (Code)"
)

#' Human-readable labels for data table columns
#'
#' Maps each column to the label of the attribute it was flattened from
#' (`bodendecker_art_label` -> label of `bodendecker_art`). Columns without a
#' matching node definition keep their name.
#'
#' @param columns Column names.
#' @param node_defs Output of [arena_node_defs()], possibly of several surveys
#'   bound together (the first label per name wins).
#' @return A named character vector: names are `columns`, values the labels.
#' @export
arena_column_labels <- function(columns, node_defs) {
  labels <- node_defs$label[!duplicated(node_defs$name)]
  names(labels) <- node_defs$name[!duplicated(node_defs$name)]

  out <- vapply(columns, function(col) {
    if (!is.na(labels[col])) return(unname(labels[col]))
    for (suffix in column_suffixes) {
      base <- sub(paste0(suffix, "$"), "", col)
      if (base != col && !is.na(labels[base])) {
        return(paste0(labels[base], suffix_labels[[suffix]]))
      }
    }
    col
  }, character(1))
  names(out) <- columns
  out
}

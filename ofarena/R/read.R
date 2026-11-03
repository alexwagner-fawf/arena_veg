#' Columns of a table
#'
#' Read from the system catalog, which is visible to every role, so this also
#' works for diagnosing tables the role cannot read.
#'
#' @param con A DBI connection or pool.
#' @param schema,table Schema and table name.
#' @return A tibble with `name` and `type` (as shown by `format_type`), in
#'   table order. Empty if the table does not exist.
#' @export
arena_columns <- function(con, schema, table) {
  tibble::as_tibble(DBI::dbGetQuery(con, "
    SELECT a.attname AS name, format_type(a.atttypid, a.atttypmod) AS type
    FROM pg_attribute a
    JOIN pg_class c ON c.oid = a.attrelid
    JOIN pg_namespace n ON n.oid = c.relnamespace
    WHERE n.nspname = $1 AND c.relname = $2 AND a.attnum > 0 AND NOT a.attisdropped
    ORDER BY a.attnum", params = list(schema, table)))
}

#' Read one entity table of a survey
#'
#' Reads the base table `survey_<id>_data.data_<entity>` (not the wide
#' `_view`, which repeats all ancestor columns). PostGIS geometry columns are
#' skipped: Arena writes them without SRID, and the same point is available as
#' `<name>_x`, `<name>_y` and `<name>_srs` (see [arena_points_sf()]).
#'
#' @param con A DBI connection or pool.
#' @param survey_id Survey id.
#' @param entity Entity name, without the `data_` prefix.
#' @param cols Optional columns to read. Columns missing in this survey's
#'   version of the table are silently left out, so the same call works across
#'   survey versions.
#' @return A tibble, with an integer `source_survey` column added first.
#'   Returns `NULL` if the table does not exist in this survey.
#' @export
arena_read_entity <- function(con, survey_id, entity, cols = NULL) {
  schema <- data_schema(survey_id)
  table <- paste0("data_", entity)
  available <- arena_columns(con, schema, table)
  if (nrow(available) == 0) return(NULL)

  available <- available[!grepl("(^|\\.)geometry", available$type), ]
  keep <- if (is.null(cols)) available$name else intersect(cols, available$name)
  select <- paste(DBI::dbQuoteIdentifier(con, keep), collapse = ", ")
  sql <- paste0("SELECT ", select, " FROM ", qualified(con, schema, table), " ORDER BY id")
  df <- tibble::as_tibble(DBI::dbGetQuery(con, sql))

  tibble::add_column(df, source_survey = as_survey_id(survey_id), .before = 1)
}

#' Add parent columns to a child entity
#'
#' Joins `child$parent_uuid` to `parent$uuid`. When both tables carry
#' `source_survey`, it is part of the key, so tables of several surveys can be
#' joined at once.
#'
#' @param child,parent Tibbles from [arena_read_entity()].
#' @param keep Parent columns to add. They must not already exist in `child`.
#' @param parent_id Name under which the parent's `uuid` is kept in the result.
#' @return `child` with `parent_id` and the `keep` columns added.
#' @export
arena_join_parent <- function(child, parent, keep, parent_id = "parent_uuid") {
  clash <- intersect(setdiff(keep, "source_survey"), names(child))
  if (length(clash) > 0) {
    rlang::abort(paste0("Columns already in `child`: ", paste(clash, collapse = ", "), "."))
  }
  by <- c(parent_uuid = "uuid")
  if ("source_survey" %in% names(child) && "source_survey" %in% names(parent)) {
    by <- c(source_survey = "source_survey", by)
  }
  lookup <- parent[unique(c(unname(by), setdiff(keep, "source_survey")))]
  out <- dplyr::left_join(child, lookup, by = by)
  if (parent_id != "parent_uuid") {
    out[[parent_id]] <- out$parent_uuid
  }
  out
}

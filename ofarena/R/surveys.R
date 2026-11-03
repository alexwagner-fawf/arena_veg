#' Surveys on the Arena server
#'
#' Reads `public.survey`. Published surveys keep their properties in `props`;
#' surveys that were never published only have `props_draft`, which is used as
#' a fallback.
#'
#' @param con A DBI connection or pool.
#' @param lang Preferred language for the label.
#' @return A tibble with one row per survey: `id`, `uuid`, `name`, `label`,
#'   `published`, `draft`, list-columns `languages`, `srs` (EPSG codes as
#'   character) and `cycles` (tibble of `cycle`, `date_start`, `date_end`),
#'   plus `has_data_schema` and `data_readable`.
#' @export
arena_surveys <- function(con, lang = "de") {
  rows <- DBI::dbGetQuery(con, "
    SELECT s.id, s.uuid::text AS uuid, s.published, s.draft,
           (CASE WHEN s.props = '{}'::jsonb THEN s.props_draft ELSE s.props END)::text AS props,
           ns.oid IS NOT NULL AS has_data_schema,
           COALESCE(has_schema_privilege(ns.oid, 'USAGE'), FALSE) AS data_readable
    FROM public.survey s
    LEFT JOIN pg_namespace ns ON ns.nspname = 'survey_' || s.id || '_data'
    WHERE NOT s.template
    ORDER BY s.id")
  props <- lapply(rows$props, jsonlite::fromJSON, simplifyVector = FALSE)

  tibble::tibble(
    id = as.integer(rows$id),
    uuid = rows$uuid,
    name = vapply(props, function(p) p$name %||% NA_character_, character(1)),
    label = vapply(props, function(p) pick_label(p$labels, lang, p$name), character(1)),
    published = rows$published,
    draft = rows$draft,
    languages = lapply(props, function(p) unlist(p$languages)),
    srs = lapply(props, function(p) vapply(p$srs, function(s) as.character(s$code), character(1))),
    cycles = lapply(props, function(p) cycles_tbl(p$cycles)),
    has_data_schema = rows$has_data_schema,
    data_readable = rows$data_readable
  )
}

cycles_tbl <- function(cycles) {
  if (length(cycles) == 0) {
    return(tibble::tibble(cycle = character(), date_start = as.Date(character()), date_end = as.Date(character())))
  }
  tibble::tibble(
    cycle = names(cycles),
    date_start = as.Date(vapply(cycles, function(c) c$dateStart %||% NA_character_, character(1))),
    date_end = as.Date(vapply(cycles, function(c) c$dateEnd %||% NA_character_, character(1)))
  )
}

# Label in the preferred language, else the first label, else the fallback.
pick_label <- function(labels, lang, fallback = NA_character_) {
  if (!is.null(labels[[lang]]) && nzchar(labels[[lang]])) return(labels[[lang]])
  if (length(labels) > 0 && !is.null(labels[[1]])) return(labels[[1]])
  fallback %||% NA_character_
}

#' Check read access to the objects an app needs
#'
#' Looks at schema USAGE and table SELECT privileges of the connected role for
#' `public.survey`, the given metadata tables of each survey and every table
#' and view in each survey's data schema.
#'
#' @param con A DBI connection or pool.
#' @param survey_ids Integer survey ids.
#' @param meta_tables Tables in the `survey_<id>` schema that are read.
#' @return A tibble with `object`, `privilege` and `ok` (FALSE also when the
#'   object does not exist). All rows are TRUE when access is complete.
#' @export
arena_check_access <- function(con, survey_ids, meta_tables = c("file", "node_def")) {
  schemas <- c(vapply(survey_ids, survey_schema, ""), vapply(survey_ids, data_schema, ""))
  schema_rows <- DBI::dbGetQuery(con, "
    SELECT s.name AS object, 'USAGE' AS privilege,
           COALESCE(has_schema_privilege(ns.oid, 'USAGE'), FALSE) AS ok
    FROM unnest($1::text[]) AS s(name)
    LEFT JOIN pg_namespace ns ON ns.nspname = s.name", params = list(pg_array(c("public", schemas))))

  wanted <- c("public.survey", as.vector(outer(vapply(survey_ids, survey_schema, ""), meta_tables, paste, sep = ".")))
  table_rows <- DBI::dbGetQuery(con, "
    SELECT w.name AS object, 'SELECT' AS privilege,
           COALESCE(has_table_privilege(c.oid, 'SELECT'), FALSE) AS ok
    FROM unnest($1::text[]) AS w(name)
    -- Catalog lookup instead of to_regclass(), which fails without schema USAGE.
    LEFT JOIN pg_namespace n ON n.nspname = split_part(w.name, '.', 1)
    LEFT JOIN pg_class c ON c.relnamespace = n.oid AND c.relname = split_part(w.name, '.', 2)",
    params = list(pg_array(wanted)))

  data_rows <- DBI::dbGetQuery(con, "
    SELECT n.nspname || '.' || c.relname AS object, 'SELECT' AS privilege,
           has_table_privilege(c.oid, 'SELECT') AS ok
    FROM pg_class c JOIN pg_namespace n ON n.oid = c.relnamespace
    WHERE n.nspname = ANY($1::text[]) AND c.relkind IN ('r', 'v')
    ORDER BY 1", params = list(pg_array(vapply(survey_ids, data_schema, ""))))

  tibble::as_tibble(rbind(schema_rows, table_rows, data_rows))
}

#' Connection pool to an Arena database
#'
#' Connection settings default to the environment variables `ARENA_DB_HOST`,
#' `ARENA_DB_PORT`, `ARENA_DB_NAME`, `ARENA_DB_USER` and `ARENA_DB_PASSWORD`,
#' so credentials can live in `.Renviron` rather than in code.
#'
#' `bigint = "numeric"` keeps Arena's bigint ids as plain doubles instead of
#' `integer64`, which dplyr and reactable handle poorly.
#'
#' @param host,port,dbname,user,password Connection settings.
#' @param ... Passed on to [pool::dbPool()].
#' @return A pool (see [pool::dbPool()]). Close it with [pool::poolClose()].
#' @export
arena_pool <- function(host = Sys.getenv("ARENA_DB_HOST", "localhost"),
                       port = as.integer(Sys.getenv("ARENA_DB_PORT", "5432")),
                       dbname = Sys.getenv("ARENA_DB_NAME", "arena"),
                       user = Sys.getenv("ARENA_DB_USER", "geouser"),
                       password = Sys.getenv("ARENA_DB_PASSWORD"),
                       ...) {
  pool::dbPool(
    RPostgres::Postgres(),
    host = host,
    port = port,
    dbname = dbname,
    user = user,
    password = password,
    bigint = "numeric",
    ...
  )
}

# Schema names Arena uses for a survey.
survey_schema <- function(survey_id) paste0("survey_", as_survey_id(survey_id))
data_schema <- function(survey_id) paste0("survey_", as_survey_id(survey_id), "_data")

as_survey_id <- function(survey_id) {
  id <- suppressWarnings(as.integer(survey_id))
  if (length(id) != 1 || is.na(id)) {
    rlang::abort(paste0("`survey_id` must be a single integer, not ", deparse(survey_id), "."))
  }
  id
}

# Quote a schema-qualified identifier.
qualified <- function(con, schema, table) {
  DBI::dbQuoteIdentifier(con, DBI::Id(schema = schema, table = table))
}

# A character vector as one Postgres array literal. RPostgres binds vectors as
# one row per element, so array parameters have to be passed as a single string.
pg_array <- function(x) {
  paste0("{", paste0('"', gsub('(["\\\\])', "\\\\\\1", x), '"', collapse = ","), "}")
}

# Fill a survey's flat data tables (survey_<id>_data.data_*) from its nodes
# (survey_<id>.node), for dumps that contain the survey schema but not the
# data schema. Development only: on the server, Arena maintains these tables.
#
#   Rscript dev/rebuild_data_schema.R [database] [survey_id]
#
# The tables must exist already (dev/extract_schema.sh). Their columns decide
# what is filled, following Arena's naming: attribute `a` becomes `a`;
# codes add `a_label`; coordinates `a_x`, `a_y`, `a_srs` ("EPSG:4326") and the
# geometry `a`; files `a_file_uuid`, `a_file_name`. Checked against Arena's own
# survey_33_data: identical apart from ids and timestamps. Attributes of single entities go
# into the table of the nearest multiple entity; multiple attributes get their
# own table. Preview records and records merged into others are skipped.

library(DBI)

args <- commandArgs(trailingOnly = TRUE)
db <- if (length(args) >= 1) args[1] else "arena_real"
survey_id <- if (length(args) >= 2) as.integer(args[2]) else 33L
meta <- paste0("survey_", survey_id)
data_schema <- paste0(meta, "_data")

con <- dbConnect(RPostgres::Postgres(), host = "localhost", port = 55432, dbname = db,
                 user = "postgres", password = "postgres", bigint = "numeric")
on.exit(dbDisconnect(con))

# Labels in the survey's first language (the one Arena uses for *_label columns).
lang <- dbGetQuery(con, "SELECT COALESCE(props, props_draft)->'languages'->>0 AS l FROM public.survey WHERE id = $1",
                   params = list(survey_id))$l
if (length(lang) == 0 || is.na(lang)) lang <- "de"
defs <- dbGetQuery(con, paste0("
  SELECT uuid::text, parent_uuid::text, type, props->>'name' AS name,
         COALESCE((props->>'multiple')::boolean, FALSE) AS multiple
  FROM ", meta, ".node_def WHERE NOT deleted AND NOT analysis AND props ? 'name'"))
items <- dbGetQuery(con, paste0("
  SELECT uuid::text AS item_uuid, props->>'code' AS code, props->'labels'->>$1 AS label
  FROM ", meta, ".category_item"), params = list(lang))
records <- dbGetQuery(con, paste0("
  SELECT uuid::text AS record_uuid, cycle AS record_cycle, step AS record_step, owner_uuid::text AS record_owner_uuid
  FROM ", meta, ".record WHERE NOT preview AND merged_into_record_uuid IS NULL"))
nodes <- dbGetQuery(con, paste0("
  SELECT n.uuid::text, n.parent_uuid::text, n.record_uuid::text, n.node_def_uuid::text AS def_uuid,
         n.date_created, n.date_modified,
         CASE WHEN jsonb_typeof(n.value) IN ('string', 'number', 'boolean') THEN n.value #>> '{}' END AS scalar,
         n.value->>'itemUuid' AS item_uuid, n.value->>'x' AS x, n.value->>'y' AS y, n.value->>'srs' AS srs,
         n.value->>'fileUuid' AS file_uuid, n.value->>'fileName' AS file_name
  FROM ", meta, ".node n
  JOIN ", meta, ".record r ON r.uuid = n.record_uuid
  WHERE NOT r.preview AND r.merged_into_record_uuid IS NULL"))
cat(sprintf("%s: %d node definitions, %d records, %d nodes\n", meta, nrow(defs), nrow(records), nrow(nodes)))

nodes$def_name <- defs$name[match(nodes$def_uuid, defs$uuid)]
nodes <- nodes[!is.na(nodes$def_name), ]
def_of <- function(name) defs[defs$name == name, ][1, ]

# Tables belong to the root entity, multiple entities and multiple attributes.
is_table_def <- defs$type == "entity" & (defs$multiple | is.na(defs$parent_uuid)) |
  defs$type != "entity" & defs$multiple
table_defs <- defs$uuid[is_table_def]

# Owner of each node = the nearest node (itself or an ancestor) with a table.
parent_of <- setNames(nodes$parent_uuid, nodes$uuid)
def_uuid_of <- setNames(nodes$def_uuid, nodes$uuid)
owner <- nodes$uuid
todo <- !nodes$def_uuid %in% table_defs
while (any(todo)) {
  owner[todo] <- parent_of[owner[todo]]
  todo <- todo & !is.na(owner) & !def_uuid_of[owner] %in% table_defs
}
nodes$owner <- owner
# For table rows, the parent row is the owner of the parent node.
nodes$parent_owner <- owner[match(nodes$parent_uuid, nodes$uuid)]

item_code <- setNames(items$code, items$item_uuid)
item_label <- setNames(items$label, items$item_uuid)

# Column value for one attribute and suffix, for the given owner rows.
attribute_values <- function(attr_def, suffix, owners) {
  a <- nodes[nodes$def_uuid == attr_def$uuid, ]
  a <- a[match(owners, if (attr_def$multiple) a$uuid else a$owner), ]
  if (suffix == "") return(if (attr_def$type == "code") unname(item_code[a$item_uuid]) else a$scalar)
  switch(
    suffix,
    "_label" = unname(item_label[a$item_uuid]),
    "_x" = a$x, "_y" = a$y, "_srs" = ifelse(is.na(a$srs), NA_character_, paste0("EPSG:", a$srs)),
    "_file_uuid" = a$file_uuid,
    # Arena writes an empty name for a file attribute without file.
    "_file_name" = ifelse(!is.na(a$uuid) & is.na(a$file_name), "", a$file_name),
    rep(NA_character_, length(owners))
  )
}

# Values are passed as text so decimals keep their exact digits. Like Arena,
# non-numbers become NULL, and so do fractions in integer columns.
cast <- function(x, type) {
  num <- suppressWarnings(as.numeric(x))
  if (grepl("^(bigint|integer)", type)) return(ifelse(!is.na(num) & num == round(num), sprintf("%.0f", num), NA_character_))
  if (grepl("^numeric|^double", type)) return(ifelse(is.na(num), NA_character_, x))
  if (type == "date") return(as.Date(substr(x, 1, 10)))
  as.character(x)
}

tables <- dbGetQuery(con, "SELECT table_name FROM information_schema.tables
                           WHERE table_schema = $1 AND table_type = 'BASE TABLE' ORDER BY 1",
                     params = list(data_schema))$table_name
system_cols <- c("id", "date_created", "date_modified", "uuid", "parent_uuid", "record_uuid",
                 "record_cycle", "record_step", "record_owner_uuid")
suffixes <- c("_label", "_x", "_y", "_srs", "_file_uuid", "_file_name")

invisible(dbWithTransaction(con, {
  dbExecute(con, paste0("TRUNCATE ", paste(paste0(data_schema, ".", tables), collapse = ", "), " CASCADE"))
  # Parents first, so foreign keys are satisfied.
  depth <- function(uuid) { d <- 0; while (!is.na(uuid)) { uuid <- defs$parent_uuid[defs$uuid == uuid]; d <- d + 1 }; d }
  table_def <- lapply(tables, function(t) def_of(sub("^data_", "", t)))
  order_tables <- order(vapply(table_def, function(d) if (is.na(d$uuid)) Inf else depth(d$uuid), 0))

  for (i in order_tables) {
    table <- tables[i]
    def <- table_def[[i]]
    if (is.na(def$uuid)) { cat("skip", table, "(no node definition)\n"); next }
    rows <- nodes[nodes$def_uuid == def$uuid & !is.na(nodes$owner), ]
    cols <- ofarena::arena_columns(con, data_schema, table)
    out <- data.frame(uuid = rows$uuid, parent_uuid = rows$parent_owner,
                      date_created = rows$date_created, date_modified = rows$date_modified)
    if ("record_uuid" %in% cols$name) {
      out <- cbind(out, records[match(rows$record_uuid, records$record_uuid), ])
    }
    # Attributes owned by this table: its own (multiple attribute) or those below the entity.
    owned <- unique(nodes$def_uuid[nodes$owner %in% rows$uuid])
    owned_defs <- defs[defs$uuid %in% c(owned, if (def$type != "entity") def$uuid) & defs$type != "entity", ]
    unmatched <- character()
    for (k in seq_len(nrow(cols))) {
      col <- cols$name[k]
      if (col %in% system_cols || grepl("(^|\\.)geometry", cols$type[k])) next
      hit <- NULL
      for (suffix in c("", suffixes)) {
        name <- if (suffix == "") col else sub(paste0(suffix, "$"), "", col)
        if (name != col || suffix == "") {
          d <- owned_defs[owned_defs$name == name, ]
          if (nrow(d) == 1) { hit <- list(def = d, suffix = suffix); break }
        }
      }
      if (is.null(hit)) { unmatched <- c(unmatched, col); out[[col]] <- NA; next }
      out[[col]] <- cast(attribute_values(hit$def, hit$suffix, rows$uuid), cols$type[k])
    }
    dbAppendTable(con, Id(schema = data_schema, table = table), out[intersect(names(out), cols$name)])
    # Geometry columns from the coordinate parts, as Arena does.
    for (g in cols$name[grepl("(^|\\.)geometry", cols$type)]) {
      dbExecute(con, sprintf(
        "UPDATE %s.%s SET %s = ST_SetSRID(ST_MakePoint(%s_x, %s_y), replace(%s_srs, 'EPSG:', '')::int) WHERE %s_x IS NOT NULL AND %s_y IS NOT NULL",
        data_schema, table, g, g, g, g, g, g
      ))
    }
    cat(sprintf("%-32s %6d rows%s\n", table, nrow(out),
                if (length(unmatched)) paste0("  (no values for: ", paste(unmatched, collapse = ", "), ")") else ""))
  }
}))

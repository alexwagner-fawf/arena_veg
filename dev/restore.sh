#!/usr/bin/env bash
# Restore Arena pg_dumps (plain SQL or custom format) into the local dev container.
#
#   dev/restore.sh [-d DB_NAME] DUMP [DUMP ...]
#
#   dev/restore.sh arena_structure.dump                                   # structure only (DB "arena")
#   dev/restore.sh -d arena_real arena_structure_public.dump arena_structure_s33.dump
#
# Recreates the database, creates the roles and extensions the dumps rely on,
# restores the dumps in the given order (public schema first), and applies
# sql/geouser_grants.sql. Surveys restored without their data schema
# (survey_N but no survey_N_data) get it from arena_structure.dump, filled
# from the survey's nodes by dev/rebuild_data_schema.R.
set -euo pipefail

here="$(cd "$(dirname "$0")" && pwd)"
root="$(dirname "$here")"
db="arena"
if [ "${1:-}" = "-d" ]; then db="$2"; shift 2; fi
[ "$#" -gt 0 ] || set -- "$root/arena_structure.dump"
geouser_pw="${GEOUSER_PASSWORD:-geouser}"

compose=(docker compose -f "$here/docker-compose.yml")
psql=("${compose[@]}" exec -T db psql -U postgres -v ON_ERROR_STOP=1 -q)

"${compose[@]}" up -d --wait

echo "Recreating database $db"
"${psql[@]}" -d postgres <<SQL
DROP DATABASE IF EXISTS "$db" WITH (FORCE);
DO \$\$
DECLARE r text;
BEGIN
  FOREACH r IN ARRAY ARRAY['arena', 'geoadmin', 'fsadmin', 'fawfuser'] LOOP
    IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = r) THEN
      EXECUTE format('CREATE ROLE %I NOLOGIN', r);
    END IF;
  END LOOP;
  IF NOT EXISTS (SELECT 1 FROM pg_roles WHERE rolname = 'geouser') THEN
    CREATE ROLE geouser LOGIN;
  END IF;
END
\$\$;
ALTER ROLE geouser LOGIN PASSWORD '$geouser_pw';
CREATE DATABASE "$db";
\c "$db"
-- Data-only dumps of single schemas do not create these.
CREATE EXTENSION IF NOT EXISTS postgis;
CREATE EXTENSION IF NOT EXISTS "uuid-ossp";
SQL

for dump in "$@"; do
  echo "Restoring $dump"
  if [ "$(head -c 5 "$dump")" = "PGDMP" ]; then
    # Roles exist, so owners and grants are restored as on the server. Errors are shown, not fatal.
    "${compose[@]}" exec -T db pg_restore -U postgres -d "$db" < "$dump" 2>&1 \
      | grep -E 'error' | sort | uniq -c || true
  else
    # Plain SQL: continue past errors but show them. Expected ones are only counted:
    # grants on PostGIS functions of another PostGIS version, and schema public existing.
    "${compose[@]}" exec -T db psql -U postgres -d "$db" -q -v ON_ERROR_STOP=0 < "$dump" 2>&1 \
      | grep -E 'ERROR' | sort | uniq \
      | awk '/function (public|topology)\..* does not exist|schema "public" already exists/ { n++; next } { print } END { if (n) print n " expected errors skipped (PostGIS function grants, existing public schema)" }' \
      || true  # grep finds nothing for a clean restore
  fi
done

surveys=$("${psql[@]}" -d "$db" -At -c "
  SELECT substring(nspname FROM '^survey_([0-9]+)$') FROM pg_namespace s
  WHERE nspname ~ '^survey_[0-9]+$'
    AND NOT EXISTS (SELECT 1 FROM pg_namespace d WHERE d.nspname = s.nspname || '_data')")
for id in $surveys; do
  # Unpublished surveys have no data schema on the server either.
  if ! grep -q "^CREATE SCHEMA survey_${id}_data;" "$root/arena_structure.dump"; then
    echo "survey_$id has no data schema, and none in arena_structure.dump (not published?): skipped"
    continue
  fi
  echo "survey_$id has no data schema: adding it from arena_structure.dump and filling it from the nodes"
  "$here/extract_schema.sh" "$root/arena_structure.dump" "survey_${id}_data" | "${psql[@]}" -d "$db" >/dev/null
  (cd "$root" && Rscript dev/rebuild_data_schema.R "$db" "$id")
done

echo "Applying geouser grants"
"${psql[@]}" -d "$db" < "$root/sql/geouser_grants.sql"

echo "Done: postgresql://geouser:***@localhost:55432/$db"

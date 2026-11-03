#!/usr/bin/env bash
# Print the objects of one schema from a plain-SQL pg_dump (structure, grants,
# default privileges), e.g. to add a survey's data schema to a database that
# was restored from a dump without it:
#
#   dev/extract_schema.sh arena_structure.dump survey_33_data | psql ...
set -euo pipefail
dump="$1"
schema="$2"
LC_ALL=C awk -v schema="$schema" '
  # Session settings at the top of the dump, up to the first object.
  !started && /^-- Name: / { started = 1 }
  !started { print; next }
  /^-- Name: / {
    keep = index($0, "; Schema: " schema ";") > 0 ||
           index($0, "-- Name: " schema "; Type: SCHEMA;") == 1 ||
           index($0, "-- Name: SCHEMA " schema "; Type: ACL;") == 1
    if (keep) print "--"
  }
  keep { print }
' "$dump"

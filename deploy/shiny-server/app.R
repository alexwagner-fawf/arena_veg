# Shiny Server entry point for the vegetation app (see README.md, "Deployment").
# Database settings (ARENA_DB_*) and R_CONFIG_ACTIVE=production come from the
# .Renviron next to this file, which is not in git. R reads it at startup when
# started in this directory; reading it again here makes that independent of
# how Shiny Server starts R.
#
# Settings from golem-config.yml can be overridden in the run_app() call below.
# Shiny uses the value of the last line, so keep a single run_app() call.
if (file.exists(".Renviron")) readRenviron(".Renviron")
arenaveg::run_app(
  # Folders the "GeoTIFF laden" file browser may open:
  # raster_roots = c(Geodaten = "/mnt/geodaten")
)

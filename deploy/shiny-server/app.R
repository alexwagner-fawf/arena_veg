# Shiny Server entry point for the vegetation app (see README.md, "Deployment").
# Database settings (ARENA_DB_*) and R_CONFIG_ACTIVE=production come from the
# .Renviron next to this file, which is not in git. R reads it at startup when
# started in this directory; reading it again here makes that independent of
# how Shiny Server starts R.
#
# Settings from golem-config.yml can be overridden here, e.g. the folders the
# "GeoTIFF laden" file browser may open:
#   arenaveg::run_app(raster_roots = c(Geodaten = "/mnt/geodaten"))
if (file.exists(".Renviron")) readRenviron(".Renviron")
arenaveg::run_app()

# Arena Shiny apps

Shiny apps on top of the FAWF Open Foris Arena server (PostgreSQL + PostGIS).

- **`ofarena/`**: an R package with generic building blocks that work for any Arena survey:
  - connection pool, survey registry, read-access check
  - node definitions (German labels), reading the flat `survey_<id>_data` tables
  - point geometries from Arena coordinates, polar offsets in gon, ray polygons and circles
  - photo delivery from `survey_<id>.file` with a disk cache
  - GeoTIFF overlays: picked in a file browser, served to the map as tiles from a disk cache
  - Shiny modules for map, table, gallery and step/cycle filter
- **`arenaveg/`**: a golem app for the vegetation survey (Verjüngungsaufnahme). It is configured for Arena survey **33**; survey 31, the previous version, can be stacked in (`veg_surveys: [31, 33]`). Its pages are Karte, Fotos, Datenqualität and Info.
  - **Karte:** plots (PK) with their 17.84 m plot area, GPS positions, regeneration circles (Verjk, drawn as outlines so drone imagery stays visible inside; placed by bearing/distance from the plot centre, `verjk_groesse` = area in m²) and patch polygons (Horste, 8 rays around a centre placed from the plot centre).
  - **GeoTIFF laden:** opens a file browser on the folders in `raster_roots` (e.g. a mounted network drive). Each chosen GeoTIFF becomes an overlay named after the file (see [GeoTIFF overlays](#geotiff-overlays)).
  - **Plot centre:** the true plot location from the plot category (`sampling_point_data`, property `location`). Arena's flat tables don't contain it, so it is read from `survey_33.category_item`. If a plot has none (no plot number), the GPS position (`curr_position`) is used. GPS positions are shown as their own layer, with their distance from the plot centre.
  - **Detail panel:** extended ground cover, simple ground cover, regeneration, droppings and photos. The Fotos tab shows the circle photo (nadir) in full, as large as the visible panel allows, and rotates it in 90° steps; a dropdown switches to the circle's other photos. Opening the tab, or clicking another circle while it is open, scrolls the panel to the photo. The open tab stays open when another circle is clicked, and the panel can be switched to full screen. Rotations are remembered per photo until the page is reloaded.

Other surveys (e.g. soil sampling 34/36, camera traps 8/37) get their own app, or a generic explorer, built on `ofarena`.

## Layout

| Path | Content |
|---|---|
| `ofarena/` | Core package (tests: `ofarena/tests`) |
| `arenaveg/` | Vegetation app, golem (config: `arenaveg/inst/golem-config.yml`) |
| `sql/geouser_grants.sql` | Read-only grants for `geouser`, idempotent |
| `sql/geouser_event_trigger.sql` | Optional: re-grant automatically when Arena recreates a data schema |
| `dev/` | Local PostGIS (Docker), restoring dumps, rebuilding a missing data schema from the nodes, synthetic data for surveys 31/33 |
| `deploy/shiny-server/` | Entry point and `.Renviron` template for Shiny Server |
| `arena_structure.dump` | Schema-only dump of the server. Dumps with data are git-ignored: they contain user accounts and sessions |

## Development

```bash
# 1a. Real data (database "arena_real"): public schema first, then the survey schema,
#     then its data schema. Without the data schema dump, survey_33_data is rebuilt
#     from the nodes instead.
dev/restore.sh -d arena_real arena_structure_public.dump arena_structure_s33.dump arena_structure_s33_data.dump

# 1b. Or synthetic data (database "arena"), used by the tests
dev/restore.sh arena_structure.dump
Rscript dev/seed_synthetic.R

# 2. R packages (renv at the repo root; development only)
Rscript -e 'renv::restore(); renv::install(c("./ofarena", "./arenaveg"))'

# 3. Run the app against the local database
ARENA_DB_PORT=55432 ARENA_DB_NAME=arena_real ARENA_DB_USER=geouser ARENA_DB_PASSWORD=geouser \
  Rscript -e 'arenaveg::run_app(cache_dir = "cache", raster_roots = c(Geodaten = "/mnt/geodaten"))'

# 4. Tests (the database tests in ofarena run only with ARENA_TEST_DB set)
Rscript -e 'devtools::test("arenaveg")'
ARENA_TEST_DB=1 ARENA_DB_PORT=55432 ARENA_DB_PASSWORD=geouser Rscript -e 'devtools::test("ofarena")'
```

`dev/rebuild_data_schema.R` fills `survey_N_data` from `survey_N.node`, following Arena's column naming and the tables defined in `arena_structure.dump`. Compared with Arena's own `survey_33_data`, the result is identical apart from ids and timestamps, so it can stand in when only the survey schema is dumped. On the server the app always reads Arena's own tables.

The synthetic database uses surveys 31 and 33; start the app with `veg_surveys = c(31, 33)` for it.

The synthetic data follows the real conventions (plot locations in the category, circles at 8 m with 10 m², fence codes 0/1). It contains deliberate gaps, so the Datenqualität page has something to show:
- a plot without GPS
- a GPS reading 150 m off
- a Verjk flagged for extended ground cover but without rows
- a plot-year that was not surveyed

## Deployment (Shiny Server)

Shiny Server serves every folder below `/srv/shiny-server/` as an app. These steps set up the app at `http://<server>:3838/verjuengung/`. Commands are for Ubuntu/Debian; paths are examples.

### 1. Database (once, as a PostgreSQL superuser)

Create the read-only role and grant it access to the surveys:

```bash
sudo -u postgres psql -d arena -c "CREATE ROLE geouser LOGIN PASSWORD 'secret'"
sudo -u postgres psql -d arena -f sql/geouser_grants.sql
```

- `-d arena` is the Arena database; adjust it if yours has another name. If Arena's PostgreSQL runs in Docker, run `psql` inside the container (`docker exec -i <container> psql -U postgres -d arena < sql/geouser_grants.sql`).
- The Shiny server must be able to reach the database: port reachable, and a `pg_hba.conf` line that allows `geouser` from the Shiny server's address.
- Arena may drop the grants when a survey is republished. Then either run `geouser_grants.sql` again, or install `sql/geouser_event_trigger.sql` once, which re-grants automatically.
- The app checks its access at startup and lists what is missing (also on the Info page).
- Adding a survey: add its id to the lists in both SQL files and run them again.

### 2. R, system libraries and Shiny Server

- **R ≥ 4.1.** Tested with R 4.5.2 and R 4.3.2.
- **Shiny Server (Open Source):** install it as described at <https://posit.co/download/shiny-server/>. It runs apps as the user `shiny`.
- **System libraries.** R packages compiled from source need the `-dev` packages:

  ```bash
  sudo apt install build-essential libpq-dev libgdal-dev libgeos-dev libproj-dev \
    libsqlite3-dev libudunits2-dev libssl-dev libcurl4-openssl-dev libpng-dev libuv1-dev
  # optional, for photo thumbnails (R package magick):
  sudo apt install libmagick++-dev
  ```

  Precompiled packages from Posit Package Manager (see step 3) need only the runtime libraries, which the `-dev` packages include.

### 3. R packages

Clone the repository to the server, e.g. `/opt/arena-shiny`, and install into the site library so the `shiny` user sees the packages:

```bash
cd /opt/arena-shiny
Rscript deploy/install_packages.R --dry-run    # lists what is missing or too old
sudo Rscript deploy/install_packages.R         # installs that, then ofarena and arenaveg
```

- The script leaves packages that are already installed and recent enough alone. It only installs missing packages and those older than a required version (e.g. bslib ≥ 0.9.0, dplyr ≥ 1.1.0, sf ≥ 1.0-12).
- Packages come from the configured CRAN mirror. `--repos=https://packagemanager.posit.co/cran/__linux__/jammy/latest` (use your Ubuntu codename) gets precompiled binaries, which install in minutes instead of compiling.
- The admin may also install packages any other way. The only requirement is that `library(arenaveg)` works for the `shiny` user.
- The script never uses renv. The repository's `.Rprofile` switches renv on for R sessions started in the repository root; the script notices this and restarts itself without renv. It prints the library it installs into.
- `renv.lock` records the versions used in development. Using it on the server (`renv::restore()`) is optional.

#### Without root: your own package library (e.g. as an RStudio Server user)

```bash
cd ~/arena-shiny
Rscript deploy/install_packages.R --user --dry-run
Rscript deploy/install_packages.R --user
```

- `--user` installs into your personal library (`R_LIBS_USER`, e.g. `~/R/x86_64-pc-linux-gnu-library/4.3`) and creates it if needed. Packages already in the site library count as installed.
- Only R sessions running as this user see these packages. For Shiny Server, the app must run as this user too (e.g. `run_as` or `user_dirs`), not as `shiny`.
- **Starting the app from RStudio in the repository folder:** the `.Rprofile` switches renv on there, and the app would load the renv library instead of your library. To keep renv off for this user, add this line to `~/.Renviron` and restart the R session:

  ```
  RENV_CONFIG_AUTOLOADER_ENABLED=FALSE
  ```

  R reads only one `.Renviron`: if the folder R starts in has its own `.Renviron`, put the line there instead. Check with `.libPaths()`: no path may contain `renv`.

### 4. App directory

```bash
sudo mkdir -p /srv/shiny-server/verjuengung
sudo cp deploy/shiny-server/app.R /srv/shiny-server/verjuengung/
sudo cp deploy/shiny-server/Renviron.example /srv/shiny-server/verjuengung/.Renviron
sudoedit /srv/shiny-server/verjuengung/.Renviron     # database host, port, name, password
sudo chown shiny:shiny /srv/shiny-server/verjuengung/.Renviron
sudo chmod 600 /srv/shiny-server/verjuengung/.Renviron
```

`app.R` reads `.Renviron` from the app directory. It holds the credentials, so it must stay unreadable for other users. `R_CONFIG_ACTIVE=production` in it selects the `production` section of the configuration.

### 5. Cache directories

The app caches photos and GeoTIFF tiles on disk. For the photo viewer it also keeps a copy of each viewed photo at 2048 px (about 1.5 MB, versus 5–20 MB for the originals). `/var/cache` belongs to root, so create the directory for the `shiny` user:

```bash
sudo mkdir -p /var/cache/arenaveg/raster
sudo chown -R shiny:shiny /var/cache/arenaveg
```

Without it, photos do not show (the log only shows warnings). The cache can be deleted at any time.

### 6. Network drive for GeoTIFFs (optional)

Mount the drive read-only at boot, readable by `shiny`. For example, a Windows share in `/etc/fstab` (needs `cifs-utils`):

```
//fileserver/geodaten  /mnt/geodaten  cifs  ro,credentials=/etc/cifs-geodaten,uid=shiny,gid=shiny,_netdev,x-systemd.automount  0  0
```

Then name the folders the file browser may open in `/srv/shiny-server/verjuengung/app.R`, inside its single `run_app()` call (Shiny uses the value of the last line, so a second `run_app()` below it would win):

```r
arenaveg::run_app(
  raster_roots = c(Geodaten = "/mnt/geodaten")
)
```

Then `sudo touch /srv/shiny-server/verjuengung/restart.txt`. Without `raster_roots`, the "GeoTIFF laden" button is hidden. GDAL comes with the R package sf; no GDAL command-line tools are needed.

If the button does not appear, check what the app sees (as the `shiny` user, in the app directory):

```bash
cd /srv/shiny-server/verjuengung && sudo -u shiny Rscript -e 'readRenviron(".Renviron"); cat("arenaveg", format(packageVersion("arenaveg")), find.package("arenaveg"), "| shiny", format(packageVersion("shiny")), "| config", Sys.getenv("R_CONFIG_ACTIVE", "default"), "\nyml:   "); str(config::get("raster_roots", file = system.file("golem-config.yml", package = "arenaveg"))); cat("app.R: "); str(source("app.R")$value$appOptions$golem_options$raster_roots)'
```

`app.R: NULL` means `app.R` does not pass `raster_roots`. Shiny older than 1.6.0 ignores options from `app.R`. A value in `golem-config.yml` takes effect only after reinstalling the package (`deploy/install_packages.R`): the app reads the installed copy, not the one in the repository.

### 7. Shiny Server settings (`/etc/shiny-server/shiny-server.conf`)

- `app_idle_timeout 3600;` in the `location /` block keeps the R process alive for an hour after the last user leaves. With the default (5 s), each first visit reloads the data. The default also stops overview builds for large GeoTIFFs that are still running.
- `preserve_logs true;` at the top level keeps the logs of apps that ran without errors.
- Behind a reverse proxy (nginx, Apache), WebSockets must be forwarded.
- Shiny Server Open Source runs one R process per app, shared by all users.

Then `sudo systemctl restart shiny-server` and open `http://<server>:3838/verjuengung/`.

### Checking and troubleshooting

- **Logs:** `/var/log/shiny-server/verjuengung-shiny-*.log`.
- **Info page:** the database user, host and access check.
- **"Keine Verbindung zur Datenbank":** check `.Renviron`, `pg_hba.conf` and the firewall.
- **"Fehlende Leserechte":** run `sql/geouser_grants.sql` again.
- **Photos missing:** the cache directory is not writable (step 5).

### Updating

```bash
cd /opt/arena-shiny && git pull
sudo Rscript deploy/install_packages.R          # or, into your own library: Rscript deploy/install_packages.R --user
sudo touch /srv/shiny-server/verjuengung/restart.txt
```

`restart.txt` makes Shiny Server start a new R process with the new packages when the next user opens the app. An app started from RStudio needs a restarted R session (Session → Restart R) to load the new packages.

## Configuration (`arenaveg/inst/golem-config.yml`)

| Key | Meaning |
|---|---|
| `veg_surveys` | Arena survey ids of the vegetation survey, oldest first (`[33]`; `[31, 33]` adds the previous version) |
| `cache_dir` | Photo cache |
| `data_ttl_min` | Minutes before data is read again (the "Daten neu laden" button forces a reload) |
| `metric_crs` | Metric CRS for polar offsets and polygons (ETRS89 / UTM 32N) |
| `pk_radius_m` | Radius of the sample plot (17.84 m = 1000 m²) |
| `pk_centre` | `category` (true location from `sampling_point_data`, else GPS) or `gps` (GPS, else category) |
| `max_gps_offset_m` | QA: GPS positions further than this from the plot centre are flagged (20 m) |
| `raster_roots` | Folders the GeoTIFF file browser may open, as `Name: path` (none: the button is hidden) |
| `raster_cache_dir` | Tile cache for GeoTIFFs |

Any key can be overridden in `run_app()`, e.g. `arenaveg::run_app(veg_surveys = 33)`.

## GeoTIFF overlays

`ofarena::mod_raster_ui()` / `mod_raster_server()` can be used by any app that has a `mod_map_server()` map.

- **Choosing files:** users pick one or more `.tif` files below the configured roots. Only files that were picked are served.
- **Display:** nothing is copied. The app writes a small VRT per file into the cache and cuts 256 px web-mercator PNG tiles from it on demand, cached on disk. The cache key includes the path, size and modification time, so a changed file gets new tiles.
- **Zoom:** the map zooms to level 25 (about 3 mm per screen pixel). Tiles are cut up to the zoom that matches the file's resolution (2 cm → 23, 5 mm → 25); beyond that the map enlarges them. Each extra zoom level has four times as many tiles, so very high resolution files fill the tile cache faster.
- **Any CRS:** files are reprojected per tile. A file without a coordinate system is rejected with a message.
- **Band handling:**
  - RGB(A) 8-bit: shown as is.
  - Colour tables: expanded, with nearest-neighbour resampling.
  - 16-bit or float (e.g. DGM): stretched to grey from approximate min/max.
  - Nodata and areas outside the raster: transparent.
- **Large files without overviews** (over 2048 px): overviews are built once in a background R process (`callr`), written to the cache and never next to the file. This reads the file over the network once. The layer appears when the build is done; other users are not blocked meanwhile. Files that already have internal overviews, such as COGs, are shown right away.
- **Scope:** layers belong to the browser session and are gone after a reload. The tile cache is shared, so a file opened again loads quickly.
- **Stale cache:** old cache folders are not cleaned up automatically. Deleting `raster_cache_dir` is safe at any time.

## What the real data (survey 33, 2025–2026) showed

Checked against `arena_structure_s33.dump` and `arena_structure_s33_data.dump` (292 records, 976 circles, 1514 photos):

- **Confirmed:**
  - `gon`/`gab` are gon (0–400, clockwise from north) and metres, from the plot centre (in practice always 8 m).
  - The patch centre is measured from the plot centre; `gon_horst`/`gab_horst` are read-only copies of `gon`/`gab`.
  - Photos are stored in `file.content`, with the extension in the file name. Thumbnails take about 0.1 s each.
  - About 300 plot-years: everything is loaded into memory (0.4 s).
- **Corrected:**
  - `verjk_groesse` is the circle **area** in m² (10 m²), not a radius.
  - In the data tables, coordinates come as `"EPSG:4326"` or `"EPSG:25832"` (in the nodes: `"4326"`).
  - Point extras in categories come as EWKT text or `{x, y, srs}` objects.
- **Not used:**
  - `x_horstmittelpunkt`/`y_horstmittelpunkt` are computed in the Arena form as `gab_horst * Math.sin(gon_horst * 0.9)`, which passes degrees to a function expecting radians, so they are wrong. The app computes positions from `gon_horst`/`gab_horst`.
  - `aktiv_filter` only drives the category filter in the Arena form.
  - No patches (Horste) have been recorded yet.
- **Data problems the Datenqualität page flags:**
  - 8 plots at "Auf dem Knopf" have survey years 20251–20258 and no plot number (it looks like "2025" plus the plot number was entered as the year).
  - 3 plots in cycle 2025 have year 2026.
  - Plots were recorded twice or more (up to 5 times).
  - GPS readings far from the true plot location: Knopf_42 (2026) about 120 km (recorded in the Hunsrück), Erbeskopf_teils_stehend_1 3.5 km, Butterhecker plots about 390 m in both years.
  - 12 circles without a photo, 5 circles without any ground-cover rows.

Not yet included: survey 31, which has data. The focus is on survey 33 for now.

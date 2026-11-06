colours <- list(
  pk = "#1f4e79", selected = "#d7263d", fenced = "#2e7d32", unfenced = "#e09f00",
  horst = "#8d5524", species = "#c2185b", muted = "#9e9e9e", gps = "#6a1b9a", position = "#00c2ff"
)

#' Map page: map with plots, circles and patches; detail panel for the
#' selected plot and circle
#' @param rasters Show the button for loading GeoTIFFs.
#' @noRd
mod_karte_ui <- function(id, rasters = FALSE) {
  ns <- shiny::NS(id)
  bslib::layout_columns(
    col_widths = c(7, 5),
    bslib::card(
      full_screen = TRUE,
      if (rasters) bslib::card_header(class = "py-1", ofarena::mod_raster_ui(ns("raster"))),
      bslib::card_body(padding = 0, ofarena::mod_map_ui(ns("map")))
    ),
    bslib::card(
      bslib::card_header(shiny::uiOutput(ns("title"), inline = TRUE)),
      bslib::card_body(
        fillable = FALSE,
        shiny::uiOutput(ns("pk_detail")),
        shiny::uiOutput(ns("verjk_section"))
      )
    )
  )
}

#' @param filtered Reactive with the filtered data (`veg_filter()`).
#' @param species Reactive with species labels to highlight.
#' @param selection `reactiveValues` with `pk` and `verjk` (uuids), shared
#'   with the other pages.
#' @param url_fun File URL builder ([ofarena::arena_file_server()]).
#' @param positions Corrected circle positions (`position_store()`).
#' @param raster_roots Named folders (e.g. network drives) to load GeoTIFFs
#'   from; none disables it.
#' @param raster_cache_dir Tile cache for the GeoTIFFs.
#' @noRd
mod_karte_server <- function(id, filtered, species, selection, url_fun, positions, raster_roots = NULL,
                             raster_cache_dir = NULL) {
  shiny::moduleServer(id, function(input, output, session) {
    ns <- session$ns

    species_hits <- shiny::reactive(veg_species_circles(filtered(), species()))

    layers <- shiny::reactive({
      d <- filtered()
      hits <- species_hits()
      pk <- d$pk_sf
      verjk <- d$verjk_sf
      horst <- d$horst_sf

      # Outlines only, so drone imagery and nadir photos can be compared with
      # the circle underneath. The invisible fill keeps the inside clickable.
      verjk_colour <- ifelse(is_fenced(verjk$gezauent, verjk$gezauent_label), colours$fenced, colours$unfenced)
      if (length(species()) > 0) {
        verjk_colour <- ifelse(verjk$verjk_uuid %in% hits, colours$species, colours$muted)
      }
      area <- d$pk_area_sf
      gps <- d$gps_sf
      layers <- list(
        ofarena::map_layer(
          area, "Probekreisfl\u00e4che", area$pk_uuid, label = area$pk_label,
          color = colours$pk, fill_opacity = 0.04, weight = 1, z = 0
        ),
        ofarena::map_layer(
          gps, "GPS-Position", gps$pk_uuid,
          label = ifelse(is.na(gps$gps_offset_m), paste("GPS", gps$pk_label),
                         sprintf("GPS %s \u00b7 %.1f m vom Probekreismittelpunkt", gps$pk_label, gps$gps_offset_m)),
          color = colours$gps, fill_opacity = 0.9, radius = 3, weight = 1, z = 2.5
        ),
        ofarena::map_layer(
          horst, "Horste", horst$verjk_uuid, label = paste("Horst", horst$verjk_label),
          color = colours$horst, fill_opacity = 0.25, weight = 1.5, z = 1
        ),
        ofarena::map_layer(
          verjk, "Verj\u00fcngungskreise", verjk$verjk_uuid,
          label = sprintf("%s \u00b7 %s", verjk$verjk_label, verjk$pk_label),
          popup = verjk_popup(verjk, url_fun),
          color = verjk_colour, fill_opacity = 0, weight = 2, z = 2
        ),
        ofarena::map_layer(
          pk, "Probekreise", pk$pk_uuid, label = pk$pk_label, popup = pk_popup(pk, d$verjk, url_fun),
          color = "white", fill_color = colours$pk, fill_opacity = 0.9, radius = 7, weight = 2, z = 3
        )
      )
      sel <- pk[pk$pk_uuid %in% selection$pk, ]
      if (nrow(sel) > 0) {
        layers <- c(layers, list(ofarena::map_layer(
          sel, "Auswahl", sel$pk_uuid, label = sel$pk_label, color = colours$selected,
          fill_opacity = 0, radius = 13, weight = 3, z = 4
        )))
      }
      layers
    })

    placing <- shiny::reactiveVal(NULL)
    draft <- shiny::reactiveVal(NULL)
    # New circle and connecting line, dashed until saved. On top, so that
    # moving the draft redraws nothing else.
    position_layers <- shiny::reactive({
      f <- position_features(positions$data(), filtered()$verjk_sf, draft())
      label <- sprintf("Korrigierte Lage %s \u00b7 %s \u00b7 %.2f m", f$circles$verjk_label, f$circles$pk_label, f$circles$offset_m)
      dash <- ifelse(f$circles$is_draft, "6 6", "")
      list(
        ofarena::map_layer(f$lines, "Verschiebung", f$lines$verjk_uuid, label = label, color = colours$position,
                           dash_array = dash, weight = 2, z = 5),
        ofarena::map_layer(f$circles, "Korrigierte Lage", f$circles$verjk_uuid, label = label, color = colours$position,
                           fill_opacity = 0, dash_array = dash, weight = 2.5, z = 5.1)
      )
    })
    map <- ofarena::mod_map_server("map", shiny::reactive(c(layers(), position_layers())))
    if (length(raster_roots) > 0) {
      ofarena::mod_raster_server("raster", map, raster_roots, raster_cache_dir)
    }

    # Zoom to the plots whenever the filter changes the set of plots. A
    # reactiveVal only invalidates on a new value, unlike filtered().
    plot_ids <- shiny::reactiveVal()
    shiny::observe(plot_ids(sort(filtered()$pk_sf$pk_uuid)))
    shiny::observeEvent(plot_ids(), map$fit(shiny::isolate(filtered()$pk_sf)))

    shiny::observeEvent(map$click(), {
      click <- map$click()
      if (click$group %in% c("Probekreise", "Auswahl", "Probekreisfl\u00e4che", "GPS-Position")) {
        select_pk(selection, click$id)
      } else {
        verjk <- filtered()$verjk
        selection$pk <- verjk$pk_uuid[verjk$verjk_uuid == click$id][1]
        selection$verjk <- click$id
      }
    })

    # Selections from other pages move the map to the plot.
    shiny::observeEvent(selection$nonce, {
      pt <- filtered()$pk_sf[filtered()$pk_sf$pk_uuid %in% selection$pk, ]
      pt <- pt[!sf::st_is_empty(pt), ]
      if (nrow(pt) > 0) {
        xy <- sf::st_coordinates(pt)[1, ]
        map$fly_to(xy[["X"]], xy[["Y"]], zoom = 19)
      }
    }, ignoreInit = TRUE)

    current_pk <- shiny::reactive({
      pk <- filtered()$pk
      pk[pk$pk_uuid %in% selection$pk, ][1, ]
    })
    has_pk <- shiny::reactive(!is.null(selection$pk) && !is.na(current_pk()$pk_uuid))

    output$title <- shiny::renderUI({
      if (!has_pk()) return("Details")
      p <- current_pk()
      htmltools::tagList(
        htmltools::strong(p$pk_label), " ",
        htmltools::span(class = "badge text-bg-secondary", paste("Survey", p$source_survey)),
        " ", step_badge(p$record_step)
      )
    })

    output$pk_detail <- shiny::renderUI({
      if (!has_pk()) {
        return(htmltools::p(class = "text-muted", "Probekreis oder Verj\u00fcngungskreis auf der Karte anklicken."))
      }
      p <- current_pk()
      panorama <- if (!is.na(p$panoramafoto_file_uuid)) {
        htmltools::tags$a(
          href = url_fun(p$source_survey, p$panoramafoto_file_uuid), target = "_blank",
          htmltools::tags$img(src = url_fun(p$source_survey, p$panoramafoto_file_uuid, thumb = TRUE),
                              class = "veg-panorama", alt = "Panoramafoto", title = "Panoramafoto \u00f6ffnen")
        )
      } else {
        htmltools::p(class = "text-muted small", "Kein Panoramafoto.")
      }
      htmltools::div(
        class = "veg-pk-head",
        panorama,
        definition_list(c(
          "Versuchsfl\u00e4che" = p$vf,
          "Probekreis" = p$pk_nr,
          "Hauptaufnahmejahr" = p$jahr,
          "Zyklus" = as.integer(p$record_cycle) + 1,
          "Aufnehmende Personen" = p$aufnehmende_personen,
          "Dateneintragung durch" = p$dateneintragung_durch,
          "Mittelpunkt" = format_position(p)[["Mittelpunkt"]],
          "GPS-Position" = format_position(p)[["GPS"]],
          "Notiz" = p$note
        ))
      )
    })

    verjk_of_pk <- shiny::reactive({
      v <- filtered()$verjk
      v[v$pk_uuid %in% selection$pk, ]
    })

    output$verjk_section <- shiny::renderUI({
      if (!has_pk()) return(NULL)
      htmltools::tagList(
        htmltools::h6(class = "mt-3", sprintf("Verj\u00fcngungskreise (%d)", nrow(verjk_of_pk()))),
        ofarena::mod_table_ui(ns("verjk_table")),
        shiny::uiOutput(ns("verjk_detail"))
      )
    })

    verjk_cols <- c("source_survey", "verjk_uuid", "verjk_label", "gezauent_label", "verjk_groesse",
                    "verjuengungshorst", "bodendecker_extended", "verjk_bild_file_uuid")
    labels <- shiny::reactive(filtered()$labels)
    verjk_table <- ofarena::mod_table_server(
      "verjk_table",
      shiny::reactive({
        v <- verjk_of_pk()[verjk_cols]
        v$verjuengungshorst <- yes_no(v$verjuengungshorst)
        v$bodendecker_extended <- yes_no(v$bodendecker_extended)
        v
      }),
      labels = c(verjk_label = "Kreis", gezauent_label = "Gez\u00e4unt", verjk_groesse = "m\u00b2",
                 verjuengungshorst = "Horst", bodendecker_extended = "BD erw."),
      hidden = c("source_survey", "verjk_uuid"),
      columns = function(df) list(
        verjk_label = reactable::colDef(name = "Kreis", minWidth = 150),
        verjk_bild_file_uuid = ofarena::photo_col(df, url_fun, size = 40)
      ),
      wrap = TRUE,
      selectable = TRUE,
      # Circles clicked on the map are shown selected in the table.
      selected = shiny::reactive(match(selection$verjk %||% NA, verjk_of_pk()$verjk_uuid)),
      defaultPageSize = 8
    )

    shiny::observeEvent(verjk_table$selected(), {
      selection$verjk <- verjk_table$selected()$verjk_uuid
    })

    current_verjk <- shiny::reactive({
      v <- verjk_of_pk()
      v[v$verjk_uuid %in% selection$verjk, ]
    })

    output$verjk_detail <- shiny::renderUI({
      v <- current_verjk()
      if (nrow(v) == 0) return(htmltools::p(class = "text-muted small mt-2", "Kreis in der Tabelle oder auf der Karte w\u00e4hlen."))
      bslib::navset_card_underline(
        id = ns("verjk_tab"),
        # Stay on the open tab (e.g. Fotos) when another circle is clicked.
        selected = shiny::isolate(input$verjk_tab),
        full_screen = TRUE,
        title = v$verjk_label,
        bslib::nav_panel("Bodendecker erw.", ofarena::mod_table_ui(ns("bd_ext"))),
        bslib::nav_panel("Bodendecker", ofarena::mod_table_ui(ns("bd"))),
        bslib::nav_panel("Verj\u00fcngung", ofarena::mod_table_ui(ns("verj"))),
        bslib::nav_panel("Losung", ofarena::mod_table_ui(ns("losung"))),
        bslib::nav_panel("Kreisdaten", shiny::uiOutput(ns("verjk_attrs"))),
        bslib::nav_panel("Fotos", htmltools::div(
          class = "veg-photos",
          shiny::uiOutput(ns("position_controls")),
          shiny::uiOutput(ns("verjk_photos"))
        ))
      )
    })

    rows_of <- function(table, cols) shiny::reactive({
      df <- filtered()[[table]]
      df <- df[df$verjk_uuid %in% selection$verjk, ]
      df[intersect(c("source_survey", cols), names(df))]
    })
    photo_cols <- function(col) function(df) stats::setNames(list(ofarena::photo_col(df, url_fun, size = 40)), col)

    # Arena's labels are whole sentences; the detail tables need short headers.
    short <- c(
      manual_id = "Nr.", bodendecker_ext_group_label = "Gruppe", bodendecker_ext_art_label = "Art",
      cover_bodendecker_ext = "Art %", cover_funct_group_perc = "Gruppe %",
      identification_certainty_label = "Sicherheit", kommentar = "Kommentar",
      bodendecker_label = "Gruppe", bodendecker_art_label = "Art", bodendecker_art = "Code",
      bodendecker_deckungsgrad = "Deckung %", art_label = "Baumart", wuchsstufe_label = "Wuchsstufe",
      anzahl = "Anzahl", anzahl_gver = "verbissen", schaden_label = "Schaden",
      tierart_label = "Tierart", anzahl_losung = "Anzahl", bemerkung_losung = "Bemerkung"
    )
    short_labels <- shiny::reactive({
      l <- labels()
      l[names(short)] <- short
      l
    })
    ofarena::mod_table_server(
      "bd_ext",
      rows_of("bd_ext", c("manual_id", "bodendecker_ext_group_label", "bodendecker_ext_art_label",
                          "cover_bodendecker_ext", "cover_funct_group_perc", "identification_certainty_label",
                          "kommentar", "foto_bodendecker_ext_file_uuid")),
      labels = short_labels, hidden = "source_survey", columns = photo_cols("foto_bodendecker_ext_file_uuid"),
      wrap = TRUE
    )
    ofarena::mod_table_server(
      "bd", rows_of("bd", c("bodendecker_label", "bodendecker_art_label", "bodendecker_art", "bodendecker_deckungsgrad")),
      labels = short_labels, hidden = "source_survey", wrap = TRUE
    )
    ofarena::mod_table_server(
      "verj",
      rows_of("verj", c("art_label", "wuchsstufe_label", "anzahl", "anzahl_gver", "schaden_label",
                        "verjuengung_photo_file_uuid")),
      labels = short_labels, hidden = "source_survey", columns = photo_cols("verjuengung_photo_file_uuid"),
      wrap = TRUE
    )
    ofarena::mod_table_server(
      "losung", rows_of("losung", c("tierart_label", "anzahl_losung", "bemerkung_losung")),
      labels = short_labels, hidden = "source_survey", wrap = TRUE
    )

    output$verjk_attrs <- shiny::renderUI({
      v <- current_verjk()
      shown <- setdiff(names(v), c("source_survey", "id", "uuid", "parent_uuid", "date_created", "date_modified",
                                   "pk_uuid", "verjk_uuid", "pk_label", "vf", "pk_nr", "jahr", "record_step",
                                   "record_cycle", "verjk_nr", grep("_file_(uuid|name)$", names(v), value = TRUE),
                                   # Computed in the Arena form with degrees passed to Math.sin/cos, so wrong.
                                   "x_horstmittelpunkt", "y_horstmittelpunkt"))
      values <- vapply(shown, function(col) format_value(v[[col]]), "")
      keep <- values != ""
      definition_list(stats::setNames(values[keep], labels()[shown][keep]))
    })

    output$verjk_photos <- shiny::renderUI({
      p <- filtered()$photos
      p <- p[p$verjk_uuid %in% selection$verjk, ]
      if (nrow(p) == 0) return(htmltools::p(class = "text-muted", "Keine Fotos."))
      # Isolated: saving must not reset the photo that is being looked at.
      saved <- shiny::isolate(saved_position())
      saved <- saved[saved$photo_uuid %in% p$file_uuid, ]
      # The saved photo first, then the circle photo (taken at nadir) and the others by type.
      p <- p[order(!p$file_uuid %in% saved$photo_uuid, match(p$type, photo_types)), ]
      photo_viewer(p, url_fun, context = current_verjk()$verjk_label[1], input_id = ns("photo_state"),
                   rotation = if (nrow(saved) == 1) stats::setNames(saved$rotation_deg, saved$photo_uuid))
    })

    saved_position <- shiny::reactive({
      p <- positions$data()
      p[p$verjk_uuid %in% selection$verjk, ]
    })

    # The photo on screen, if it belongs to the selected circle.
    shown_photo <- shiny::reactive({
      state <- input$photo_state
      p <- filtered()$photos
      if (is.null(state) || !state$file_uuid %in% p$file_uuid[p$verjk_uuid %in% selection$verjk]) {
        return(list(file_uuid = NA_character_, rotation = NA_integer_))
      }
      list(file_uuid = state$file_uuid, rotation = as.integer(state$rotation))
    })

    rotation_changed <- shiny::reactive({
      saved <- saved_position()
      photo <- shown_photo()
      if (nrow(saved) == 0 || is.na(photo$file_uuid)) return(FALSE)
      was <- if (photo$file_uuid %in% saved$photo_uuid) saved$rotation_deg else 0L
      photo$rotation != was
    })

    set_placing <- function(verjk_uuid) {
      placing(verjk_uuid)
      draft(NULL)
      session$sendCustomMessage("veg-map-placing", list(id = ns("map-map"), on = !is.null(verjk_uuid)))
    }

    selected_circle <- shiny::reactive({
      v <- filtered()$verjk_sf
      v[v$verjk_uuid %in% selection$verjk & !sf::st_is_empty(v), ]
    })

    # Placing belongs to one circle; selecting or filtering away ends it.
    shiny::observe({
      if (!is.null(placing()) && !placing() %in% selected_circle()$verjk_uuid) set_placing(NULL)
    })

    shiny::observeEvent(input$position_start, {
      set_placing(selected_circle()$verjk_uuid)
      if (nrow(saved_position()) == 1) draft(saved_position())
    })
    shiny::observeEvent(input$position_cancel, set_placing(NULL))

    shiny::observeEvent(map$map_click(), {
      if (is.null(placing())) return()
      click <- map$map_click()
      point <- sf::st_sfc(sf::st_point(c(click$lng, click$lat)), crs = 4326)
      draft(position_row(selected_circle(), point, positions$crs))
    })

    save_position <- function(row) {
      photo <- shown_photo()
      row$photo_uuid <- photo$file_uuid
      row$rotation_deg <- photo$rotation
      row$edited_at <- as.POSIXct(Sys.time(), tz = "UTC")
      tryCatch({
        positions$save(row)
        TRUE
      }, error = function(e) {
        shiny::showNotification(paste("Nicht gespeichert:", conditionMessage(e)), type = "error", duration = NULL)
        FALSE
      })
    }
    shiny::observeEvent(input$position_save, if (save_position(draft())) set_placing(NULL))
    shiny::observeEvent(input$position_rotation, save_position(saved_position()))

    shiny::observeEvent(input$position_delete, shiny::showModal(shiny::modalDialog(
      title = "Korrigierte Lage l\u00f6schen?",
      sprintf("%s: der Kreis erh\u00e4lt wieder nur seine Originallage.", current_verjk()$verjk_label),
      footer = htmltools::tagList(
        shiny::modalButton("Abbrechen"),
        shiny::actionButton(ns("position_delete_ok"), "L\u00f6schen", class = "btn-danger")
      )
    )))
    shiny::observeEvent(input$position_delete_ok, {
      shiny::removeModal()
      tryCatch(positions$remove(selection$verjk), error = function(e) {
        shiny::showNotification(paste("Nicht gel\u00f6scht:", conditionMessage(e)), type = "error", duration = NULL)
      })
    })

    output$position_controls <- shiny::renderUI({
      if (nrow(selected_circle()) == 0) return(NULL)
      position_controls(ns, saved_position(), !is.null(placing()), draft(), rotation_changed())
    })
  })
}

select_pk <- function(selection, pk_uuid, verjk_uuid = NULL) {
  selection$pk <- pk_uuid
  selection$verjk <- verjk_uuid
}

pk_popup <- function(pk, verjk, url_fun) {
  n_verjk <- table(factor(verjk$pk_uuid, levels = pk$pk_uuid))
  img <- ifelse(
    is.na(pk$panoramafoto_file_uuid), "",
    sprintf('<img src="%s" class="veg-popup-img" loading="lazy">',
            url_fun(pk$source_survey, pk$panoramafoto_file_uuid, thumb = TRUE))
  )
  sprintf("<strong>%s</strong><br>%s<br>%d Verj\u00fcngungskreise \u00b7 Survey %s",
          htmltools::htmlEscape(pk$pk_label), img, as.integer(n_verjk), pk$source_survey)
}

verjk_popup <- function(verjk, url_fun) {
  img <- ifelse(
    is.na(verjk$verjk_bild_file_uuid), "",
    sprintf('<img src="%s" class="veg-popup-img" loading="lazy">',
            url_fun(verjk$source_survey, verjk$verjk_bild_file_uuid, thumb = TRUE))
  )
  sprintf("<strong>%s</strong><br>%s<br>%s \u00b7 %s",
          htmltools::htmlEscape(verjk$verjk_label), img, htmltools::htmlEscape(verjk$pk_label),
          htmltools::htmlEscape(fence_label(verjk$gezauent_label)))
}

step_badge <- function(step) {
  label <- c("1" = "Erfassung", "2" = "Bereinigung", "3" = "Analyse")[step]
  colour <- c("1" = "warning", "2" = "info", "3" = "success")[step]
  htmltools::span(class = paste0("badge text-bg-", ifelse(is.na(colour), "secondary", colour)),
                  ifelse(is.na(label), paste("Schritt", step), label))
}

format_position <- function(p) {
  point <- function(x, y, srs) {
    if (is.na(x) || is.na(y)) return("keine")
    # Arena writes "EPSG:4326" in the data tables, "4326" in categories.
    sprintf("%s / %s (EPSG:%s)", format_num(round(x, 6)), format_num(round(y, 6)), ofarena::parse_srs(srs))
  }
  gps <- point(p$curr_position_x, p$curr_position_y, p$curr_position_srs)
  if (!is.na(p$gps_offset_m)) gps <- sprintf("%s, %.1f m vom Mittelpunkt", gps, p$gps_offset_m)
  c(Mittelpunkt = point(p$location_x, p$location_y, p$location_srs), GPS = gps)
}

# Arena stores booleans as the text "true" / "false".
yes_no <- function(x) ifelse(is.na(x), NA_character_, ifelse(tolower(x) == "true", "ja", "nein"))

format_value <- function(x) {
  if (length(x) == 0 || is.na(x)) return("")
  if (identical(x, "true")) return("ja")
  if (identical(x, "false")) return("nein")
  if (inherits(x, "Date")) return(format(x, "%d.%m.%Y"))
  if (is.numeric(x)) return(format_num(x))
  as.character(x)
}

definition_list <- function(values) {
  values <- vapply(values, function(v) format_value(v), "")
  values <- values[values != ""]
  htmltools::tags$dl(
    class = "row small mb-0",
    lapply(seq_along(values), function(i) htmltools::tagList(
      htmltools::tags$dt(class = "col-5 fw-normal text-muted", names(values)[i]),
      htmltools::tags$dd(class = "col-7 mb-1", values[[i]])
    ))
  )
}

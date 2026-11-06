# Highest zoom level of the map, about 3 mm per screen pixel in Germany, for
# very high resolution drone imagery. Tile layers are enlarged beyond their
# native zoom up to this level; a layer with a lower maxZoom would vanish.
map_max_zoom <- 25L

#' Base map with OpenStreetMap and Esri satellite imagery
#'
#' @param overlay_groups Overlay group names for the layer control.
#' @return A leaflet map.
#' @export
arena_basemap <- function(overlay_groups = character()) {
  leaflet::leaflet(options = leaflet::leafletOptions(maxZoom = map_max_zoom)) |>
    leaflet::addProviderTiles(leaflet::providers$OpenStreetMap, group = "OpenStreetMap",
                              options = leaflet::providerTileOptions(maxNativeZoom = 19, maxZoom = map_max_zoom)) |>
    leaflet::addProviderTiles(leaflet::providers$Esri.WorldImagery, group = "Satellit (Esri)",
                              options = leaflet::providerTileOptions(maxNativeZoom = 19, maxZoom = map_max_zoom)) |>
    leaflet::addLayersControl(
      baseGroups = c("OpenStreetMap", "Satellit (Esri)"),
      overlayGroups = overlay_groups,
      options = leaflet::layersControlOptions(collapsed = FALSE)
    ) |>
    leaflet::addScaleBar(position = "bottomleft", options = leaflet::scaleBarOptions(imperial = FALSE))
}

#' A map layer for [mod_map_server()]
#'
#' @param data An `sf` object with POINT, LINESTRING or POLYGON geometries.
#'   Empty geometries are dropped.
#' @param group Group name shown in the layer control.
#' @param ids Feature ids, reported back on click. Must be unique per layer.
#' @param label Hover labels (character, recycled), or `NULL`.
#' @param popup Popup HTML (character, recycled), or `NULL`.
#' @param color,fill_color Stroke and fill colours (recycled).
#' @param radius Marker radius in pixels (points only).
#' @param weight Stroke width.
#' @param opacity,fill_opacity Stroke and fill opacity.
#' @param dash_array Stroke dash pattern, e.g. `"6 6"`, `""` for a solid
#'   stroke (recycled). Not used for points.
#' @param z Drawing order: layers with higher `z` are drawn on top.
#' @return A layer specification (list).
#' @export
map_layer <- function(data, group, ids, label = NULL, popup = NULL, color = "#1f78b4",
                      fill_color = color, radius = 6, weight = 2, opacity = 1,
                      fill_opacity = 0.4, dash_array = NULL, z = 0) {
  keep <- !sf::st_is_empty(data)
  pick <- function(x) if (length(x) > 1) x[keep] else x
  list(
    data = sf::st_transform(data[keep, ], 4326), group = group, ids = ids[keep],
    label = pick(label), popup = pick(popup), color = pick(color),
    fill_color = pick(fill_color), radius = radius, weight = weight,
    opacity = opacity, fill_opacity = fill_opacity, dash_array = pick(dash_array), z = z
  )
}

#' Leaflet map module
#'
#' Draws the layers returned by `layers()`. Only layers whose specification
#' changed are redrawn. Clicks on features are reported with the layer's
#' group and the feature id.
#'
#' @param id Module id.
#' @param height Map height (CSS).
#' @export
mod_map_ui <- function(id, height = "100%") {
  leaflet::leafletOutput(shiny::NS(id, "map"), height = height)
}

#' @rdname mod_map_ui
#' @param layers Reactive returning a list of [map_layer()] specifications.
#' @return A list with `click` (reactive: `list(group, id)` of the last
#'   clicked feature), `map_click` (reactive: `list(lat, lng)` of the last
#'   click beside the features), `fit(sf)` to zoom to features, `fly_to(lng, lat,
#'   zoom)`, `add_tiles(group, url, bbox, max_native_zoom)` to add a tile
#'   overlay (URL template with `{z}`, `{x}`, `{y}`; `bbox` in WGS 84 limits
#'   the requests) and `remove_tiles(group)`.
#' @export
mod_map_server <- function(id, layers) {
  shiny::moduleServer(id, function(input, output, session) {
    output$map <- leaflet::renderLeaflet(arena_basemap())
    drawn <- list()
    # Stable short key per group, used as prefix of the leaflet layer ids.
    registry <- character()
    key_of <- function(group) {
      if (!group %in% registry) registry <<- c(registry, group)
      paste0("l", match(group, registry))
    }

    shiny::observe({
      specs <- layers()
      specs <- specs[order(vapply(specs, function(s) as.numeric(s$z), 0))]
      groups <- vapply(specs, `[[`, "", "group")
      proxy <- leaflet::leafletProxy("map", session)

      # Redraw from the first changed layer on, so drawing order stays intact.
      changed <- which(!vapply(seq_along(specs), function(i) identical(specs[[i]], drawn[[groups[i]]]), TRUE))
      removed <- setdiff(names(drawn), groups)
      for (g in removed) leaflet::clearGroup(proxy, g)
      if (length(changed) > 0) {
        for (i in seq(min(changed), length(specs))) {
          leaflet::clearGroup(proxy, groups[i])
          draw_layer(proxy, specs[[i]], key_of(groups[i]))
        }
      }
      drawn <<- stats::setNames(specs, groups)

      if (length(changed) > 0 || length(removed) > 0) update_control(proxy)
    })

    # Tile overlays (e.g. GeoTIFFs, see mod_raster_server()), by group.
    tile_groups <- character()
    update_control <- function(proxy = leaflet::leafletProxy("map", session)) {
      leaflet::addLayersControl(
        proxy,
        baseGroups = c("OpenStreetMap", "Satellit (Esri)"),
        overlayGroups = unique(c(names(drawn), tile_groups)),
        options = leaflet::layersControlOptions(collapsed = FALSE)
      )
    }

    click <- shiny::reactiveVal(NULL)
    on_click <- function(event) {
      if (is.null(event$id)) return()
      key <- as.integer(sub("^l([0-9]+):.*$", "\\1", event$id))
      click(list(group = registry[key], id = sub("^[^:]*:", "", event$id), nonce = stats::runif(1)))
    }
    shiny::observeEvent(input$map_marker_click, on_click(input$map_marker_click))
    shiny::observeEvent(input$map_shape_click, on_click(input$map_shape_click))

    list(
      click = click,
      map_click = shiny::reactive(input$map_click),
      fit = function(features) {
        features <- features[!sf::st_is_empty(features), ]
        if (nrow(features) == 0) return(invisible())
        bb <- sf::st_bbox(sf::st_transform(features, 4326))
        leaflet::fitBounds(leaflet::leafletProxy("map", session),
                           bb[["xmin"]], bb[["ymin"]], bb[["xmax"]], bb[["ymax"]],
                           # Room for the layer control in the top right corner.
                           options = list(maxZoom = 19, paddingTopLeft = c(30, 30), paddingBottomRight = c(160, 30)))
      },
      fly_to = function(lng, lat, zoom = 19) {
        leaflet::flyTo(leaflet::leafletProxy("map", session), lng, lat, zoom)
      },
      add_tiles = function(group, url, bbox = NULL, max_native_zoom = 22) {
        tile_groups <<- union(tile_groups, group)
        bounds <- if (!is.null(bbox)) list(c(bbox[["ymin"]], bbox[["xmin"]]), c(bbox[["ymax"]], bbox[["xmax"]]))
        proxy <- leaflet::leafletProxy("map", session)
        leaflet::clearGroup(proxy, group)
        leaflet::addTiles(
          proxy, urlTemplate = url, group = group,
          # Above the base maps, which leaflet re-adds on top when switched.
          options = leaflet::tileOptions(maxZoom = map_max_zoom, maxNativeZoom = max_native_zoom, bounds = bounds,
                                         zIndex = 100 + length(tile_groups))
        )
        update_control(proxy)
      },
      remove_tiles = function(group) {
        tile_groups <<- setdiff(tile_groups, group)
        proxy <- leaflet::leafletProxy("map", session)
        leaflet::clearGroup(proxy, group)
        update_control(proxy)
      }
    )
  })
}

draw_layer <- function(proxy, spec, key) {
  if (nrow(spec$data) == 0) return(proxy)
  layer_id <- paste0(key, ":", spec$ids)
  type <- unique(as.character(sf::st_geometry_type(spec$data)))

  if (all(type %in% c("POINT", "MULTIPOINT"))) {
    leaflet::addCircleMarkers(
      proxy, data = spec$data, group = spec$group, layerId = layer_id,
      radius = spec$radius, color = spec$color, fillColor = spec$fill_color,
      weight = spec$weight, opacity = spec$opacity, fillOpacity = spec$fill_opacity,
      label = spec$label, popup = spec$popup,
      popupOptions = leaflet::popupOptions(maxWidth = 380)
    )
  } else if (all(type %in% c("LINESTRING", "MULTILINESTRING"))) {
    leaflet::addPolylines(
      proxy, data = spec$data, group = spec$group, layerId = layer_id,
      color = spec$color, weight = spec$weight, opacity = spec$opacity,
      dashArray = spec$dash_array, smoothFactor = 0, label = spec$label, popup = spec$popup
    )
  } else {
    leaflet::addPolygons(
      proxy, data = spec$data, group = spec$group, layerId = layer_id,
      color = spec$color, fillColor = spec$fill_color, weight = spec$weight,
      opacity = spec$opacity, fillOpacity = spec$fill_opacity, dashArray = spec$dash_array,
      # No simplification: leaflet would draw small circles as visible polygons.
      smoothFactor = 0,
      label = spec$label, popup = spec$popup,
      popupOptions = leaflet::popupOptions(maxWidth = 380)
    )
  }
}

#' Interactive Viewer for SAM Detections
#'
#' Opens an interactive map viewer to view SAM detections.
#'
#' @param x A geosam object from `sam_detect()`.
#' @param source Imagery source for basemap: "mapbox", "esri", or "maptiler".
#'   Defaults to "mapbox" but falls back to "esri" if MAPBOX_PUBLIC_TOKEN
#'   is not set. Esri is free and requires no API key.
#'
#' @return The geosam object when the user clicks "Done".
#'
#' @details
#' The viewer shows the satellite imagery with current detections overlaid.
#'
#' Note: Point-based refinement is not yet fully implemented. The current
#' version is view-only.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- sam_detect(image = "satellite.tif", text = "building")
#' refined <- sam_view(result)
#' }
sam_view <- function(x, source = c("mapbox", "esri", "maptiler")) {
  validate_geosam(x)
  rlang::check_installed(c("shiny", "mapgl"), reason = "for interactive refinement")
  source <- match.arg(source)

  # Check for required API keys and fall back to Esri if missing
  source <- .resolve_map_source(source)

  # Run the gadget and return result
  shiny::runGadget(
    app = shiny::shinyApp(
      ui = .view_ui(x, source),
      server = .view_server(x, source)
    ),
    viewer = shiny::dialogViewer("geosam - Refine", width = 1000, height = 700)
  )
}


#' @keywords internal
.view_ui <- function(x, source) {
  # Choose correct output function based on source
  map_output <- if (source == "mapbox") {
    mapgl::mapboxglOutput("map", height = "100%")
  } else {
    mapgl::maplibreOutput("map", height = "100%")
  }

  shiny::fillPage(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML("
        body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; }

        .control-panel {
          position: absolute;
          top: 10px;
          right: 10px;
          z-index: 1000;
          background: white;
          border-radius: 8px;
          box-shadow: 0 2px 10px rgba(0,0,0,0.15);
          padding: 16px;
          width: 260px;
        }

        .control-panel h4 {
          margin: 0 0 4px 0;
          font-size: 16px;
          font-weight: 600;
          color: #333;
        }

        .subtitle {
          font-size: 11px;
          color: #888;
          margin-bottom: 12px;
        }

        .section-label {
          font-size: 10px;
          font-weight: 600;
          color: #999;
          text-transform: uppercase;
          letter-spacing: 0.5px;
          margin: 12px 0 6px 0;
        }

        .help-text {
          font-size: 11px;
          color: #666;
          margin-bottom: 8px;
          line-height: 1.4;
        }

        .info-row {
          display: flex;
          justify-content: space-between;
          font-size: 11px;
          color: #666;
          margin-bottom: 4px;
        }

        .info-label { font-weight: 500; }
        .info-value { color: #333; }

        .btn-primary {
          width: 100%;
          padding: 10px;
          background: #154733;
          color: white;
          border: none;
          border-radius: 6px;
          font-size: 13px;
          font-weight: 600;
          cursor: pointer;
          margin-bottom: 6px;
        }

        .btn-primary:hover { background: #0d2e21; }

        .btn-secondary {
          width: 100%;
          padding: 8px;
          background: white;
          color: #666;
          border: 1px solid #ddd;
          border-radius: 6px;
          font-size: 12px;
          cursor: pointer;
          margin-bottom: 6px;
        }

        .btn-secondary:hover { background: #f5f5f5; }

        .btn-success {
          width: 100%;
          padding: 10px;
          background: #154733;
          color: white;
          border: none;
          border-radius: 6px;
          font-size: 13px;
          font-weight: 600;
          cursor: pointer;
        }

        .btn-success:hover { background: #0d2e21; }

        .status-box {
          font-size: 12px;
          color: #666;
          padding: 10px;
          background: #f9f9f9;
          border-radius: 6px;
          margin-top: 10px;
        }

        .status-success { color: #16a34a; }
        .status-error { color: #dc2626; }

        .divider { height: 1px; background: #eee; margin: 12px 0; }

        /* Radio button styling */
        .control-panel .shiny-input-radiogroup {
          display: flex;
          flex-wrap: wrap;
          gap: 4px;
          margin-bottom: 8px;
        }

        .control-panel .radio-inline {
          font-size: 11px;
          padding: 6px 10px;
          margin: 0;
          border: 1px solid #ddd;
          border-radius: 4px;
          cursor: pointer;
          white-space: nowrap;
        }

        .control-panel .radio-inline input { display: none; }
        .control-panel .radio-inline:hover { background: #f5f5f5; }

        .control-panel .radio-inline:has(input:checked) {
          background: #154733;
          color: white;
          border-color: #154733;
        }
      "))
    ),

    map_output,

    shiny::div(
      class = "control-panel",

      shiny::h4("View Detections"),

      # Current stats
      shiny::div(class = "section-label", "Results"),
      shiny::div(
        class = "info-row",
        shiny::span(class = "info-label", "Detections:"),
        shiny::span(class = "info-value", shiny::textOutput("n_detections", inline = TRUE))
      ),

      shiny::div(class = "divider"),

      shiny::div(class = "section-label", "Export"),
      shiny::downloadButton("download_geojson", "Download GeoJSON", class = "btn-secondary"),

      shiny::div(class = "divider"),

      shiny::actionButton("done", "Done", class = "btn-success")
    )
  )
}


#' @keywords internal
.view_server <- function(initial_geosam, source) {
  function(input, output, session) {
    # Reactive values
    rv <- shiny::reactiveValues(
      geosam = initial_geosam
    )

    # Initial detections as sf (already WGS84)
    shiny::showNotification("Converting masks to polygons...", id = "loading", duration = NULL)
    initial_sf <- sam_as_sf(initial_geosam)
    shiny::removeNotification("loading")

    # Helper to build base map based on source
    build_base_map <- function() {
      if (source == "mapbox") {
        mapgl::mapboxgl(
          style = mapgl::mapbox_style("satellite-streets"),
          bounds = initial_sf
        )
      } else if (source == "maptiler") {
        mapgl::maplibre(
          style = mapgl::maptiler_style("hybrid"),
          bounds = initial_sf
        )
      } else {
        # Esri - use maplibre with raster source
        mapgl::maplibre(bounds = initial_sf) |>
          mapgl::add_raster_source(
            id = "esri-satellite",
            tiles = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
            tileSize = 256
          ) |>
          mapgl::add_raster_layer(
            id = "esri-satellite-layer",
            source = "esri-satellite"
          )
      }
    }

    # Render map based on source
    if (source == "mapbox") {
      output$map <- mapgl::renderMapboxgl({
        m <- build_base_map() |>
          mapgl::add_navigation_control(position = "top-left") |>
          mapgl::add_scale_control(position = "bottom-left", unit = "imperial")

        if (!is.null(initial_sf) && nrow(initial_sf) > 0) {
          m <- m |>
            mapgl::add_fill_layer(
              id = "detections",
              source = initial_sf,
              fill_color = "#facc15",
              fill_opacity = 0.5,
              fill_outline_color = "#eab308"
            )
        }
        m
      })
    } else {
      output$map <- mapgl::renderMaplibre({
        m <- build_base_map() |>
          mapgl::add_navigation_control(position = "top-left") |>
          mapgl::add_scale_control(position = "bottom-left", unit = "imperial")

        if (!is.null(initial_sf) && nrow(initial_sf) > 0) {
          m <- m |>
            mapgl::add_fill_layer(
              id = "detections",
              source = initial_sf,
              fill_color = "#facc15",
              fill_opacity = 0.5,
              fill_outline_color = "#eab308"
            )
        }
        m
      })
    }

    # Display counts
    output$n_detections <- shiny::renderText({
      length(rv$geosam$masks)
    })

    # Download GeoJSON
    output$download_geojson <- shiny::downloadHandler(
      filename = function() {
        paste0("geosam_detections_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".geojson")
      },
      content = function(file) {
        sf::st_write(initial_sf, file, driver = "GeoJSON", quiet = TRUE)
      }
    )

    # Done - return result
    shiny::observeEvent(input$done, {
      shiny::stopApp(rv$geosam)
    })

    # Handle window close
    session$onSessionEnded(function() {
      shiny::stopApp(rv$geosam)
    })
  }
}

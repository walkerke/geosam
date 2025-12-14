#' Interactive Viewer for SAM Detections
#'
#' Opens an interactive map viewer to view SAM detections.
#'
#' @param x A geosam object from `sam_detect()`.
#' @param fill Fill color for detection polygons. Default is "#FACC15" (yellow).
#' @param border Border/outline color for polygons. Default is "#EAB308".
#' @param fill_opacity Fill opacity for detection polygons (0-1). Default is 0.5.
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
#'
#' # Custom colors
#' sam_view(result, fill = "#3B82F6", border = "#1D4ED8")
#' }
sam_view <- function(
  x,
  fill = "#FACC15",
  border = "#EAB308",
  fill_opacity = 0.5,
  source = c("mapbox", "esri", "maptiler")
) {
  if (!inherits(x, "geosam")) {
    cli::cli_abort("{.arg x} must be a {.cls geosam} object.")
  }
  rlang::check_installed(
    c("shiny", "mapgl"),
    reason = "for interactive refinement"
  )
  source <- match.arg(source)

  # Check for required API keys and fall back to Esri if missing
  source <- .resolve_map_source(source)

  # Run the gadget and return result
  shiny::runGadget(
    app = shiny::shinyApp(
      ui = .view_ui(x, source),
      server = .view_server(x, source, fill, border, fill_opacity)
    ),
    viewer = shiny::dialogViewer("geosam - Refine", width = 1000, height = 700)
  )
}


#' @noRd
.view_ui <- function(x, source) {
  # Get score range for slider
  scores <- x$scores
  if (length(scores) == 0) scores <- c(0, 1)
  score_min <- floor(min(scores) * 100) / 100
  score_max <- ceiling(max(scores) * 100) / 100

  # Choose correct output function based on source
  map_output <- if (source == "mapbox") {
    mapgl::mapboxglOutput("map", height = "100%")
  } else {
    mapgl::maplibreOutput("map", height = "100%")
  }

  shiny::fillPage(
    shiny::tags$head(
      shiny::tags$style(shiny::HTML(
        "
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

        /* Slider styling */
        .irs--shiny .irs-bar {
          background-color: #154733;
          border-top: 1px solid #154733;
          border-bottom: 1px solid #154733;
          height: 6px;
          top: 27px;
        }

        .irs--shiny .irs-line {
          height: 6px;
          top: 27px;
          background: #e5e5e5;
          border-radius: 3px;
        }

        .irs--shiny .irs-handle {
          border: 2px solid #154733;
          background-color: white;
          width: 18px;
          height: 18px;
          top: 21px;
          cursor: pointer;
        }

        .irs--shiny .irs-handle:hover {
          background-color: #f0f0f0;
        }

        .irs--shiny .irs-single {
          background-color: #154733;
          font-size: 10px;
          padding: 2px 6px;
        }

        .irs--shiny .irs-single:before {
          border-top-color: #154733;
        }

        .irs-grid {
          display: none !important;
        }

        .irs--shiny .irs-min, .irs--shiny .irs-max {
          font-size: 10px;
          color: #666;
          background: transparent;
          padding: 0;
          top: 35px;
        }
      "
      ))
    ),

    map_output,

    shiny::div(
      class = "control-panel",

      shiny::h4("View Detections"),

      # Current stats
      shiny::div(class = "section-label", "Results"),
      shiny::div(
        class = "info-row",
        shiny::span(class = "info-label", "Total detections:"),
        shiny::span(
          class = "info-value",
          shiny::textOutput("n_detections", inline = TRUE)
        )
      ),
      shiny::div(
        class = "info-row",
        shiny::span(class = "info-label", "Visible:"),
        shiny::span(
          class = "info-value",
          shiny::textOutput("n_visible", inline = TRUE)
        )
      ),

      shiny::div(class = "divider"),

      # Confidence filter
      shiny::div(class = "section-label", "Filter by Confidence"),
      shiny::sliderInput(
        "confidence",
        label = NULL,
        min = score_min,
        max = score_max,
        value = score_min,
        step = 0.01
      ),

      shiny::div(class = "divider"),

      shiny::div(class = "section-label", "Export"),
      shiny::downloadButton(
        "download_geojson",
        "Download GeoJSON",
        class = "btn-secondary"
      ),

      shiny::div(class = "divider"),

      shiny::actionButton("done", "Done", class = "btn-success")
    )
  )
}


#' @noRd
.view_server <- function(initial_geosam, source, fill, border, fill_opacity) {
  function(input, output, session) {
    # Reactive values
    rv <- shiny::reactiveValues(
      geosam = initial_geosam,
      n_visible = 0
    )

    # Initial detections as sf (already WGS84)
    shiny::showNotification(
      "Converting masks to polygons...",
      id = "loading",
      duration = NULL
    )
    initial_sf <- sam_as_sf(initial_geosam)
    shiny::removeNotification("loading")

    # Get count (works for both tiled and non-tiled)
    n_detect <- sam_count(initial_geosam)
    rv$n_visible <- n_detect

    # Create bbox sf from the geosam extent
    bbox_sf <- .create_bbox_sf(initial_geosam)

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

    # Helper to add detection and bbox layers to map
    add_layers <- function(m) {
      # Add detections with tooltip
      if (!is.null(initial_sf) && nrow(initial_sf) > 0) {
        m <- m |>
          mapgl::add_fill_layer(
            id = "detections",
            source = initial_sf,
            fill_color = fill,
            fill_opacity = fill_opacity,
            fill_outline_color = border,
            popup = mapgl::concat(
              "Confidence: ",
              mapgl::number_format(
                mapgl::get_column("score"),
                maximum_fraction_digits = 2
              )
            )
          )
      }

      # Add bbox outline
      if (!is.null(bbox_sf)) {
        m <- m |>
          mapgl::add_line_layer(
            id = "bbox-outline",
            source = bbox_sf,
            line_color = "#FFFFFF",
            line_width = 3,
            line_opacity = 0.9
          )
      }

      m
    }

    # Render map based on source
    if (source == "mapbox") {
      output$map <- mapgl::renderMapboxgl({
        build_base_map() |>
          mapgl::add_navigation_control(position = "top-left") |>
          mapgl::add_scale_control(
            position = "bottom-left",
            unit = "imperial"
          ) |>
          add_layers()
      })
    } else {
      output$map <- mapgl::renderMaplibre({
        build_base_map() |>
          mapgl::add_navigation_control(position = "top-left") |>
          mapgl::add_scale_control(
            position = "bottom-left",
            unit = "imperial"
          ) |>
          add_layers()
      })
    }

    # Confidence filter
    shiny::observeEvent(input$confidence, {
      filter_expr <- list(">=", mapgl::get_column("score"), input$confidence)

      proxy_fn <- if (source == "mapbox") mapgl::mapboxgl_proxy else
        mapgl::maplibre_proxy
      proxy_fn("map") |>
        mapgl::set_filter(layer_id = "detections", filter = filter_expr)

      # Update visible count
      rv$n_visible <- sum(initial_sf$score >= input$confidence)
    })

    # Display counts
    output$n_detections <- shiny::renderText({
      n_detect
    })

    output$n_visible <- shiny::renderText({
      rv$n_visible
    })

    # Download GeoJSON (filtered by current confidence threshold)
    output$download_geojson <- shiny::downloadHandler(
      filename = function() {
        paste0(
          "geosam_detections_",
          format(Sys.time(), "%Y%m%d_%H%M%S"),
          ".geojson"
        )
      },
      content = function(file) {
        filtered_sf <- initial_sf[initial_sf$score >= input$confidence, ]
        sf::st_write(filtered_sf, file, driver = "GeoJSON", quiet = TRUE)
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


#' Create bbox sf from geosam extent
#' @noRd
.create_bbox_sf <- function(x) {
  # Get extent from geosam object
  ext <- x$extent
  if (is.null(ext) || length(ext) != 4) {
    return(NULL)
  }

  # Extent is in the raster's CRS (likely EPSG:3857 for web tiles)
  # Create polygon from extent
  xmin <- ext[1]
  xmax <- ext[2]
  ymin <- ext[3]
  ymax <- ext[4]

  # Create polygon coordinates (closed ring)
  coords <- matrix(
    c(
      xmin,
      ymin,
      xmax,
      ymin,
      xmax,
      ymax,
      xmin,
      ymax,
      xmin,
      ymin # Close the ring
    ),
    ncol = 2,
    byrow = TRUE
  )

  # Create sf polygon
  poly <- sf::st_polygon(list(coords))
  bbox_sf <- sf::st_sf(geometry = sf::st_sfc(poly))

  # Set CRS from geosam (usually EPSG:3857)
  if (!is.null(x$crs) && x$crs != "") {
    sf::st_crs(bbox_sf) <- x$crs
  }

  # Transform to WGS84 for display
  bbox_sf <- sf::st_transform(bbox_sf, 4326)

  bbox_sf
}

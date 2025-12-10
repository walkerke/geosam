#' Interactive Refinement Viewer
#'
#' Opens an interactive map viewer to refine SAM detections using positive and
#' negative point clicks.
#'
#' @param x A geosam object from `sam_detect()`.
#'
#' @return A refined geosam object when the user clicks "Done".
#'
#' @details
#' The viewer shows the satellite imagery with current detections overlaid.
#' Users can:
#' - Toggle between "Add point (+)" and "Remove point (-)" modes
#' - Click on the map to add refinement points
#' - Click "Re-run" to apply refinements
#' - Click "Done" to return the refined result
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- sam_detect(image = "satellite.tif", text = "building")
#' refined <- sam_view(result)
#' }
sam_view <- function(x) {
  validate_geosam(x)
  rlang::check_installed(c("shiny", "mapgl"), reason = "for interactive refinement")

  # Run the gadget and return result
  shiny::runGadget(
    app = shiny::shinyApp(
      ui = .view_ui(x),
      server = .view_server(x)
    ),
    viewer = shiny::dialogViewer("geosam - Refine", width = 1000, height = 700)
  )
}


#' @keywords internal
.view_ui <- function(x) {
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
        .control-panel .radio-inline {
          font-size: 12px;
          padding: 6px 12px;
          margin-right: 4px;
          border: 1px solid #ddd;
          border-radius: 4px;
          cursor: pointer;
          display: inline-block;
        }

        .control-panel .radio-inline input { display: none; }
        .control-panel .radio-inline:hover { background: #f5f5f5; }

        .control-panel .radio-inline:has(input:checked).positive {
          background: #16a34a;
          color: white;
          border-color: #16a34a;
        }

        .control-panel .radio-inline:has(input:checked).negative {
          background: #dc2626;
          color: white;
          border-color: #dc2626;
        }
      "))
    ),

    mapgl::mapboxglOutput("map", height = "100%"),

    shiny::div(
      class = "control-panel",

      shiny::h4("Refine Detections"),
      shiny::div(class = "subtitle", "Add +/- points to improve results"),

      # Current stats
      shiny::div(class = "section-label", "Current Results"),
      shiny::div(
        class = "info-row",
        shiny::span(class = "info-label", "Detections:"),
        shiny::span(class = "info-value", shiny::textOutput("n_detections", inline = TRUE))
      ),

      shiny::div(class = "divider"),

      # Point mode
      shiny::div(class = "section-label", "Click Mode"),
      shiny::p(class = "help-text", "Green (+) to include areas, red (-) to exclude."),
      shiny::tags$div(
        class = "radio-group",
        shiny::tags$label(
          class = "radio-inline positive",
          shiny::tags$input(type = "radio", name = "point_mode", value = "positive", checked = "checked"),
          "Add (+)"
        ),
        shiny::tags$label(
          class = "radio-inline negative",
          shiny::tags$input(type = "radio", name = "point_mode", value = "negative"),
          "Remove (-)"
        )
      ),

      shiny::div(
        style = "margin: 8px 0; font-size: 11px;",
        "Points: ", shiny::textOutput("n_points", inline = TRUE)
      ),
      shiny::actionButton("clear_points", "Clear Points", class = "btn-secondary"),

      shiny::div(class = "divider"),

      shiny::actionButton("rerun", "Re-run Detection", class = "btn-primary"),

      shiny::div(class = "divider"),

      shiny::actionButton("done", "Done", class = "btn-success"),

      shiny::div(class = "status-box", shiny::uiOutput("status_ui"))
    )
  )
}


#' @keywords internal
.view_server <- function(initial_geosam) {
  function(input, output, session) {
    # Reactive values
    rv <- shiny::reactiveValues(
      geosam = initial_geosam,
      points = list(),
      status = "Click on the map to add refinement points.",
      status_type = "normal"
    )

    # Get image extent for map bounds
    template <- terra::rast(initial_geosam$image_path)
    ext <- terra::ext(template)
    # Convert from Web Mercator to WGS84 for map center
    center_merc <- c((ext$xmin + ext$xmax) / 2, (ext$ymin + ext$ymax) / 2)
    center_pt <- sf::st_sfc(sf::st_point(center_merc), crs = 3857)
    center_wgs <- sf::st_transform(center_pt, 4326)
    center_coords <- sf::st_coordinates(center_wgs)

    # Initial detections as sf
    initial_sf <- sam_as_sf(initial_geosam)

    # Render map
    output$map <- mapgl::renderMapboxgl({
      m <- mapgl::mapboxgl(
        style = mapgl::mapbox_style("satellite-streets"),
        center = c(center_coords[1, "X"], center_coords[1, "Y"]),
        zoom = 16
      ) |>
        mapgl::add_navigation_control(position = "top-left") |>
        mapgl::add_scale_control(position = "bottom-left", unit = "imperial")

      # Add detection polygons
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

    # Display counts
    output$n_detections <- shiny::renderText({
      length(rv$geosam$masks)
    })

    output$n_points <- shiny::renderText({
      length(rv$points)
    })

    # Status display
    output$status_ui <- shiny::renderUI({
      cls <- if (rv$status_type == "success") "status-success"
             else if (rv$status_type == "error") "status-error"
             else ""
      shiny::div(class = cls, rv$status)
    })

    # Handle map clicks - use custom input from radio buttons
    shiny::observeEvent(input$map_click, {
      click <- input$map_click
      if (!is.null(click)) {
        # Get point mode from the custom radio buttons
        mode <- input$point_mode %||% "positive"
        label <- if (mode == "positive") 1L else 0L

        rv$points <- c(rv$points, list(list(
          lng = click$lng,
          lat = click$lat,
          label = label
        )))

        # Add marker to map
        color <- if (label == 1L) "#22c55e" else "#ef4444"
        mapgl::mapboxgl_proxy("map") |>
          mapgl::add_markers(
            data = sf::st_sf(
              geometry = sf::st_sfc(sf::st_point(c(click$lng, click$lat)), crs = 4326)
            ),
            marker_options = list(color = color)
          )

        rv$status <- sprintf("Added %s point. Click 'Re-run' when ready.",
                            if (label == 1L) "positive" else "negative")
        rv$status_type <- "normal"
      }
    })

    # Clear points
    shiny::observeEvent(input$clear_points, {
      rv$points <- list()
      mapgl::mapboxgl_proxy("map") |>
        mapgl::clear_markers()
      rv$status <- "Points cleared."
      rv$status_type <- "normal"
    })

    # Re-run detection with refinement points
    shiny::observeEvent(input$rerun, {
      if (length(rv$points) == 0) {
        rv$status <- "Add some refinement points first."
        rv$status_type <- "normal"
        return()
      }

      rv$status <- "Running detection..."
      rv$status_type <- "normal"

      # Build points sf
      coords <- do.call(rbind, lapply(rv$points, function(p) c(p$lng, p$lat)))
      labels <- sapply(rv$points, function(p) p$label)

      pts_sf <- sf::st_as_sf(
        data.frame(x = coords[, 1], y = coords[, 2]),
        coords = c("x", "y"),
        crs = 4326
      )

      # Run refinement
      refined <- tryCatch({
        sam_refine(rv$geosam, points = pts_sf, labels = labels)
      }, error = function(e) {
        rv$status <- paste("Error:", e$message)
        rv$status_type <- "error"
        NULL
      })

      if (!is.null(refined)) {
        rv$geosam <- refined
        rv$points <- list()

        # Update map with new detections
        new_sf <- sam_as_sf(refined)
        if (!is.null(new_sf) && nrow(new_sf) > 0) {
          mapgl::mapboxgl_proxy("map") |>
            mapgl::clear_layer("detections") |>
            mapgl::clear_markers() |>
            mapgl::add_fill_layer(
              id = "detections",
              source = new_sf,
              fill_color = "#6366f1",
              fill_opacity = 0.4,
              fill_outline_color = "#4338ca"
            )
        }

        rv$status <- sprintf("Refined to %d detection(s).", length(refined$masks))
        rv$status_type <- "success"
      }
    })

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

#' Interactive Discovery and Detection
#'
#' Opens an interactive map to explore satellite imagery, draw prompts, and
#' run SAM detection.
#'
#' @param source Imagery source: "mapbox", "esri", or "maptiler".
#'   - "mapbox": Requires `MAPBOX_PUBLIC_TOKEN` environment variable
#'   - "esri": Free Esri World Imagery (no API key required)
#'   - "maptiler": Requires `MAPTILER_API_KEY` environment variable
#' @param center Initial map center as c(lng, lat). If NULL, defaults to US center.
#' @param bbox Initial bounding box as c(xmin, ymin, xmax, ymax) or sf object.
#'   If provided, map will zoom to this extent.
#' @param zoom Initial zoom level (default 15).
#'
#' @return A geosam object when the user clicks "Done", or NULL if cancelled.
#'
#' @details
#' The explorer provides a complete workflow:
#' 1. Navigate the map to find an area of interest
#' 2. Select a prompt type (text, box, points, or exemplar)
#' 3. Enter text or draw prompts on the map
#' 4. Click "Detect" to run SAM on the current viewport
#' 5. View results and optionally refine with +/- points
#' 6. Click "Done" to return the geosam object
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Start exploring with Mapbox satellite
#' result <- sam_explore(source = "mapbox", center = c(-102.5, 31.8), zoom = 15)
#'
#' # Extract results
#' if (!is.null(result)) {
#'   polygons <- sam_as_sf(result)
#' }
#' }
sam_explore <- function(
    source = c("mapbox", "esri", "maptiler"),
    center = NULL,
    bbox = NULL,
    zoom = 15
) {
  rlang::check_installed(c("shiny", "mapgl"), reason = "for interactive exploration")
  source <- match.arg(source)

 # Check for required API keys and fall back to Esri if missing
  source <- .resolve_map_source(source)

  # Default center to US
 if (is.null(center) && is.null(bbox)) {
    center <- c(-98, 39)
    zoom <- 4
  }

  # Convert bbox if sf
  if (!is.null(bbox)) {
    if (inherits(bbox, c("sf", "sfc"))) {
      bbox <- sf::st_bbox(sf::st_transform(bbox, 4326))
    }
    center <- c((bbox[1] + bbox[3]) / 2, (bbox[2] + bbox[4]) / 2)
    # Estimate zoom from bbox width
    width_deg <- bbox[3] - bbox[1]
    zoom <- max(1, min(18, round(8 - log2(width_deg))))
  }

  # Run gadget and return result
  result <- shiny::runGadget(
    app = shiny::shinyApp(
      ui = .explore_ui(source),
      server = .explore_server(source, center, zoom)
    ),
    viewer = shiny::dialogViewer("geosam", width = 1200, height = 800)
  )

  result
}


#' @noRd
.explore_ui <- function(source) {

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
          background: rgba(255, 255, 255, 0.8);
          backdrop-filter: blur(8px);
          -webkit-backdrop-filter: blur(8px);
          border-radius: 8px;
          box-shadow: 0 2px 10px rgba(0,0,0,0.15);
          padding: 16px;
          width: 280px;
          max-height: calc(100vh - 40px);
          overflow-y: auto;
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
        .btn-primary:disabled {
          background: #6b9080;
          cursor: not-allowed;
        }

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

        .control-panel input[type='text'] {
          width: 100%;
          padding: 8px 10px;
          border: 1px solid #ddd;
          border-radius: 6px;
          font-size: 13px;
          box-sizing: border-box;
          margin-bottom: 8px;
        }

        .control-panel input[type='text']:focus {
          outline: none;
          border-color: #154733;
        }

        .control-panel .form-group {
          margin-bottom: 8px;
        }

        .control-panel label.control-label {
          display: none;
        }

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
        .status-warning { color: #d97706; }

        .divider { height: 1px; background: #eee; margin: 12px 0; }

        /* Radio button styling */
        .control-panel .shiny-input-radiogroup {
          margin-bottom: 8px;
        }

        .control-panel .radio-inline {
          font-size: 11px;
          padding: 5px 8px;
          margin: 0 2px 0 0;
          border: 1px solid #ddd;
          border-radius: 4px;
          cursor: pointer;
        }

        .control-panel .radio-inline input { display: none; }
        .control-panel .radio-inline:hover { background: #f5f5f5; }

        .control-panel .radio-inline:has(input:checked) {
          background: #154733;
          color: white;
          border-color: #154733;
        }

        /* Point mode buttons */
        .point-mode-btn {
          display: inline-block;
          padding: 6px 12px;
          font-size: 11px;
          border: 1px solid #ddd;
          border-radius: 4px;
          cursor: pointer;
          margin-right: 4px;
        }

        .point-mode-btn:hover { background: #f5f5f5; }

        .point-mode-btn.positive.active {
          background: #16a34a;
          color: white;
          border-color: #16a34a;
        }

        .point-mode-btn.negative.active {
          background: #dc2626;
          color: white;
          border-color: #dc2626;
        }

        .point-badge {
          display: inline-block;
          font-size: 10px;
          font-weight: 600;
          padding: 2px 6px;
          border-radius: 10px;
          margin-left: 4px;
        }

        .point-badge.positive { background: #dcfce7; color: #16a34a; }
        .point-badge.negative { background: #fee2e2; color: #dc2626; }

        /* Multi-prompt styling */
        .prompt-row {
          display: flex;
          align-items: center;
          gap: 6px;
          margin-bottom: 6px;
        }
        .prompt-row input[type='text'] {
          flex: 1;
          margin-bottom: 0 !important;
        }
        .prompt-color {
          width: 16px;
          height: 16px;
          border-radius: 3px;
          flex-shrink: 0;
        }
        .prompt-remove {
          background: none;
          border: none;
          color: #999;
          cursor: pointer;
          padding: 2px 6px;
          font-size: 14px;
        }
        .prompt-remove:hover { color: #dc2626; }
        .btn-add-prompt {
          font-size: 11px;
          padding: 4px 8px;
          background: #f5f5f5;
          border: 1px dashed #ccc;
          border-radius: 4px;
          cursor: pointer;
          color: #666;
        }
        .btn-add-prompt:hover { background: #eee; border-color: #999; }

        /* Slider styling - cleaner look */
        .control-panel .irs--shiny .irs-bar { background: #154733; border-top-color: #154733; border-bottom-color: #154733; }
        .control-panel .irs--shiny .irs-single { background: #154733; }
        .control-panel .irs--shiny .irs-handle { border-color: #154733; }
        .control-panel .irs--shiny .irs-min,
        .control-panel .irs--shiny .irs-max { display: none; }
        .control-panel .shiny-input-container { margin-bottom: 0; }

        /* Loading spinner - satellite/radar style */
        .spinner {
          display: inline-block;
          width: 16px;
          height: 16px;
          border: 2px solid rgba(255,255,255,0.3);
          border-radius: 50%;
          border-top-color: white;
          border-right-color: white;
          animation: spin 0.6s linear infinite;
          margin-right: 8px;
          vertical-align: middle;
        }
        @keyframes spin { to { transform: rotate(360deg); } }
        .btn-primary.detecting {
          pointer-events: none;
          background: #1a5c42;
        }
      ")),
      shiny::tags$script(shiny::HTML("
        Shiny.addCustomMessageHandler('setDetecting', function(detecting) {
          var btn = document.getElementById('detect');
          if (detecting) {
            btn.innerHTML = '<span class=\"spinner\"></span> Detecting...';
            btn.classList.add('detecting');
          } else {
            btn.innerHTML = 'Detect in View';
            btn.classList.remove('detecting');
          }
        });
      "))
    ),

    map_output,

    shiny::div(
      class = "control-panel",

      shiny::h4("geosam"),
      shiny::div(class = "subtitle", "SAM 3 Object Detection"),

      # Current view info
      shiny::div(class = "section-label", "Current View"),
      shiny::div(
        class = "info-row",
        shiny::span(class = "info-label", "Zoom:"),
        shiny::span(class = "info-value", shiny::textOutput("current_zoom", inline = TRUE))
      ),
      shiny::div(
        class = "info-row",
        shiny::span(class = "info-label", "Area:"),
        shiny::span(class = "info-value", shiny::textOutput("view_area", inline = TRUE))
      ),
      shiny::uiOutput("zoom_warning"),

      shiny::div(class = "divider"),

      # Mode selector
      shiny::div(class = "section-label", "Detection Mode"),
      shiny::radioButtons(
        "prompt_type",
        label = NULL,
        choices = c("Text" = "text", "Example" = "exemplar", "Points" = "points"),
        selected = "text",
        inline = TRUE
      ),

      # Text mode panel
      shiny::conditionalPanel(
        condition = "input.prompt_type == 'text'",
        shiny::p(class = "help-text", "Describe objects to find. Add multiple prompts for different object types."),
        shiny::uiOutput("prompt_rows_ui"),
        shiny::actionButton("add_prompt", "+ Add prompt", class = "btn-add-prompt")
      ),

      # Exemplar mode panel
      shiny::conditionalPanel(
        condition = "input.prompt_type == 'exemplar'",
        shiny::p(class = "help-text", "Draw a rectangle around ONE example. SAM will find all similar objects."),
        shiny::actionButton("clear_drawing", "Clear Box", class = "btn-secondary")
      ),

      # Points mode panel
      shiny::conditionalPanel(
        condition = "input.prompt_type == 'points'",
        shiny::p(class = "help-text", "Click to add points. Green (+) includes, red (-) excludes."),
        shiny::radioButtons(
          "point_mode",
          label = NULL,
          choices = c("Add (+)" = "positive", "Remove (-)" = "negative"),
          selected = "positive",
          inline = TRUE
        ),
        shiny::div(
          style = "margin-bottom: 8px; font-size: 11px;",
          "Points: ", shiny::textOutput("n_points", inline = TRUE)
        ),
        shiny::actionButton("clear_points", "Clear Points", class = "btn-secondary")
      ),

      shiny::div(class = "divider"),

      # Threshold slider
      shiny::div(class = "section-label", "Confidence"),
      shiny::sliderInput("threshold", label = NULL, min = 0.1, max = 0.9, value = 0.5, step = 0.1),

      # Extraction zoom selector
      shiny::div(class = "section-label", "Extraction Zoom"),
      shiny::selectInput(
        "extraction_zoom",
        label = NULL,
        choices = c("16 (regional)" = "16", "17 (local)" = "17", "18 (detailed)" = "18"),
        selected = "17"
      ),
      shiny::div(
        class = "info-row",
        shiny::span(class = "info-label", "Est. tiles:"),
        shiny::span(class = "info-value", shiny::textOutput("tile_estimate", inline = TRUE))
      ),
      shiny::uiOutput("tile_warning"),

      # Detect button
      shiny::actionButton("detect", "Detect in View", class = "btn-primary"),

      shiny::div(class = "divider"),

      # Results
      shiny::div(class = "section-label", "Results"),
      shiny::div(
        class = "info-row",
        shiny::span(class = "info-label", "Detections:"),
        shiny::span(class = "info-value", shiny::textOutput("n_detections", inline = TRUE))
      ),
      shiny::actionButton("clear_results", "Clear Results", class = "btn-secondary"),

      shiny::div(class = "divider"),

      shiny::actionButton("done", "Done", class = "btn-success"),

      shiny::div(class = "status-box", shiny::uiOutput("status_ui"))
    )
  )
}


#' @noRd
.explore_server <- function(source, initial_center, initial_zoom) {
  function(input, output, session) {
    # Color palette for prompts
    prompt_colors <- c("#facc15", "#3b82f6", "#ef4444", "#22c55e", "#a855f7", "#f97316")

    # Reactive values
    rv <- shiny::reactiveValues(
      geosam = NULL,
      points = list(),
      drawn_bbox = NULL,
      image_path = NULL,
      status = "Navigate to an area and click 'Detect in View'.",
      status_type = "normal",
      prompts = list(list(id = 1, text = "", color = "#facc15")),
      next_prompt_id = 2,
      detecting = FALSE
    )

    # Helper to get correct proxy based on source
    get_proxy <- function() {
      if (source == "mapbox") {
        mapgl::mapboxgl_proxy("map")
      } else {
        mapgl::maplibre_proxy("map")
      }
    }

    # Build map based on source
    if (source == "mapbox") {
      # Mapbox - use mapboxgl
      output$map <- mapgl::renderMapboxgl({
        mapgl::mapboxgl(
          style = mapgl::mapbox_style("satellite-streets"),
          center = initial_center,
          zoom = initial_zoom
        ) |>
          mapgl::add_navigation_control(position = "top-left") |>
          mapgl::add_scale_control(position = "bottom-left", unit = "imperial")
      })
    } else if (source == "maptiler")

{
      # MapTiler - use maplibre with maptiler_style
      output$map <- mapgl::renderMaplibre({
        mapgl::maplibre(
          style = mapgl::maptiler_style("hybrid"),
          center = initial_center,
          zoom = initial_zoom
        ) |>
          mapgl::add_navigation_control(position = "top-left") |>
          mapgl::add_scale_control(position = "bottom-left", unit = "imperial")
      })
    } else {
      # Esri - use maplibre with raster source
      output$map <- mapgl::renderMaplibre({
        mapgl::maplibre(
          center = initial_center,
          zoom = initial_zoom
        ) |>
          mapgl::add_raster_source(
            id = "esri-satellite",
            tiles = "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{z}/{y}/{x}",
            tileSize = 256
          ) |>
          mapgl::add_raster_layer(
            id = "esri-satellite-layer",
            source = "esri-satellite"
          ) |>
          mapgl::add_navigation_control(position = "top-left") |>
          mapgl::add_scale_control(position = "bottom-left", unit = "imperial")
      })
    }

    # Add draw control once map is ready
    shiny::observe({
      shiny::req(input$map_bbox)
      get_proxy() |>
        mapgl::add_draw_control(
          position = "top-left",
          displayControlsDefault = FALSE,
          controls = list(polygon = TRUE, trash = TRUE),
          rectangle = TRUE
        )
    }) |> shiny::bindEvent(input$map_bbox, once = TRUE)

    # Render prompt rows UI
    output$prompt_rows_ui <- shiny::renderUI({
      prompts <- rv$prompts
      rows <- lapply(seq_along(prompts), function(i) {
        p <- prompts[[i]]
        shiny::div(
          class = "prompt-row",
          shiny::div(class = "prompt-color", style = sprintf("background: %s;", p$color)),
          shiny::textInput(
            inputId = paste0("prompt_text_", p$id),
            label = NULL,
            value = p$text,
            placeholder = if (i == 1) "e.g., swimming pool" else "another object type"
          ),
          if (length(prompts) > 1) {
            shiny::actionButton(
              paste0("remove_prompt_", p$id),
              "\u00d7",
              class = "prompt-remove"
            )
          }
        )
      })
      do.call(shiny::tagList, rows)
    })

    # Add prompt button - preserve existing text values
    shiny::observeEvent(input$add_prompt, {
      if (length(rv$prompts) < 6) {
        # Capture current text values before modifying
        current_prompts <- lapply(rv$prompts, function(p) {
          input_id <- paste0("prompt_text_", p$id)
          text <- input[[input_id]]
          list(id = p$id, text = if (is.null(text)) "" else text, color = p$color)
        })
        new_color <- prompt_colors[min(length(current_prompts) + 1, length(prompt_colors))]
        rv$prompts <- c(current_prompts, list(list(
          id = rv$next_prompt_id,
          text = "",
          color = new_color
        )))
        rv$next_prompt_id <- rv$next_prompt_id + 1
      }
    })

    # Remove prompt observers - preserve text values for remaining prompts
    shiny::observe({
      prompts <- rv$prompts
      lapply(prompts, function(p) {
        btn_id <- paste0("remove_prompt_", p$id)
        shiny::observeEvent(input[[btn_id]], {
          # Capture current text values before removing
          current_prompts <- lapply(rv$prompts, function(pr) {
            input_id <- paste0("prompt_text_", pr$id)
            text <- input[[input_id]]
            list(id = pr$id, text = if (is.null(text)) "" else text, color = pr$color)
          })
          rv$prompts <- Filter(function(x) x$id != p$id, current_prompts)
        }, ignoreInit = TRUE, once = TRUE)
      })
    })

    # Helper to get current prompt texts from inputs (avoids re-render issues)
    get_prompt_texts <- function() {
      lapply(rv$prompts, function(p) {
        input_id <- paste0("prompt_text_", p$id)
        text <- input[[input_id]]
        list(id = p$id, text = if (is.null(text)) "" else text, color = p$color)
      })
    }

    # Count displays
    output$n_detections <- shiny::renderText({
      result <- rv$geosam
      if (is.null(result)) {
        "0"
      } else if (inherits(result, "sf")) {
        as.character(nrow(result))
      } else {
        as.character(sam_count(result))
      }
    })

    output$n_points <- shiny::renderText({
      as.character(length(rv$points))
    })

    # Zoom and area displays
    output$current_zoom <- shiny::renderText({
      z <- input$map_zoom
      if (is.null(z)) "-" else sprintf("%.1f", z)
    })

    output$view_area <- shiny::renderText({
      bounds <- input$map_bbox
      if (is.null(bounds)) return("-")

      tryCatch({
        # mapgl returns xmin/ymin/xmax/ymax
        if (is.null(bounds$xmin) || is.null(bounds$xmax) ||
            is.null(bounds$ymin) || is.null(bounds$ymax)) {
          return("-")
        }

        # Calculate approximate area in km²
        lat_mid <- (bounds$ymax + bounds$ymin) / 2
        width_km <- (bounds$xmax - bounds$xmin) * 111.32 * cos(lat_mid * pi / 180)
        height_km <- (bounds$ymax - bounds$ymin) * 111.32
        area_km2 <- width_km * height_km

        if (area_km2 < 1) {
          sprintf("%.0f m²", area_km2 * 1e6)
        } else if (area_km2 < 100) {
          sprintf("%.1f km²", area_km2)
        } else {
          sprintf("%.0f km²", area_km2)
        }
      }, error = function(e) "-")
    })

    output$zoom_warning <- shiny::renderUI({
      # No zoom restrictions now - we support wide area extraction
      NULL
    })

    # Tile count estimate
    output$tile_estimate <- shiny::renderText({
      bounds <- input$map_bbox
      if (is.null(bounds)) return("-")

      ext_zoom <- as.integer(input$extraction_zoom %||% 17)
      bbox <- c(bounds$xmin, bounds$ymin, bounds$xmax, bounds$ymax)

      tryCatch({
        dims <- .calc_bbox_pixels(bbox, ext_zoom, source)
        n_tiles <- dims$n_tiles_x * dims$n_tiles_y
        format(n_tiles, big.mark = ",")
      }, error = function(e) "-")
    })

    # Tile warning
    output$tile_warning <- shiny::renderUI({
      bounds <- input$map_bbox
      if (is.null(bounds)) return(NULL)

      ext_zoom <- as.integer(input$extraction_zoom %||% 17)
      bbox <- c(bounds$xmin, bounds$ymin, bounds$xmax, bounds$ymax)

      tryCatch({
        dims <- .calc_bbox_pixels(bbox, ext_zoom, source)
        n_tiles <- dims$n_tiles_x * dims$n_tiles_y

        if (n_tiles > 500) {
          shiny::div(
            class = "status-box status-error",
            style = "margin-top: 6px; padding: 6px 8px;",
            "Too many tiles. Zoom in or use lower extraction zoom."
          )
        } else if (n_tiles > 200) {
          shiny::div(
            class = "status-box status-warning",
            style = "margin-top: 6px; padding: 6px 8px;",
            "Large extraction - may take several minutes."
          )
        } else {
          NULL
        }
      }, error = function(e) NULL)
    })

    # Status display
    output$status_ui <- shiny::renderUI({
      cls <- switch(rv$status_type,
        success = "status-success",
        error = "status-error",
        warning = "status-warning",
        ""
      )
      shiny::div(class = cls, rv$status)
    })

    # Handle map clicks for point mode
    shiny::observeEvent(input$map_click, {
      if (input$prompt_type == "points") {
        click <- input$map_click
        if (!is.null(click)) {
          # Default to positive if not yet initialized
          mode <- input$point_mode %||% "positive"
          label <- if (mode == "positive") 1L else 0L
          rv$points <- c(rv$points, list(list(
            lng = click$lng,
            lat = click$lat,
            label = label
          )))

          # Add marker - green for positive, red for negative
          marker_color <- if (label == 1L) "#16a34a" else "#ef4444"
          get_proxy() |>
            mapgl::add_markers(
              data = sf::st_sf(
                geometry = sf::st_sfc(sf::st_point(c(click$lng, click$lat)), crs = 4326)
              ),
              color = marker_color
            )
        }
      }
    })

    # Clear points
    shiny::observeEvent(input$clear_points, {
      rv$points <- list()
      get_proxy() |>
        mapgl::clear_markers()
      rv$status <- "Points cleared."
      rv$status_type <- "normal"
    })

    # Clear drawing
    shiny::observeEvent(input$clear_drawing, {
      rv$drawn_bbox <- NULL
      get_proxy() |>
        mapgl::clear_draw()
      rv$status <- "Box cleared. Draw around an example."
      rv$status_type <- "normal"
    })

    # Clear results
    shiny::observeEvent(input$clear_results, {
      rv$geosam <- NULL
      tryCatch({
        get_proxy() |>
          mapgl::clear_layer("detections") |>
          mapgl::clear_legend()
      }, error = function(e) NULL)
      rv$status <- "Results cleared."
      rv$status_type <- "normal"
    })

    # Run detection
    shiny::observeEvent(input$detect, {
      session$sendCustomMessage("setDetecting", TRUE)
      on.exit(session$sendCustomMessage("setDetecting", FALSE))

      # Get current map bounds
      bounds <- input$map_bbox
      if (is.null(bounds)) {
        rv$status <- "Could not get map bounds."
        rv$status_type <- "error"
        return()
      }

      bbox <- c(bounds$xmin, bounds$ymin, bounds$xmax, bounds$ymax)
      ext_zoom <- as.integer(input$extraction_zoom %||% 17)

      # Check tile count
      tryCatch({
        dims <- .calc_bbox_pixels(bbox, ext_zoom, source)
        n_tiles <- dims$n_tiles_x * dims$n_tiles_y
        if (n_tiles > 500) {
          rv$status <- "Area too large. Zoom in or use lower extraction zoom."
          rv$status_type <- "error"
          return()
        }
      }, error = function(e) NULL)

      rv$status <- "Running detection (this may take a moment)..."
      rv$status_type <- "normal"

      # Build prompts based on type
      result <- tryCatch({
        if (input$prompt_type == "text") {
          # Get all prompts with non-empty text (read directly from inputs)
          active_prompts <- Filter(function(p) nchar(p$text) > 0, get_prompt_texts())
          if (length(active_prompts) == 0) {
            rv$status <- "Enter at least one text description."
            rv$status_type <- "warning"
            return()
          }

          # Run detection for each prompt and combine results
          all_results <- list()
          for (p in active_prompts) {
            rv$status <- sprintf("Detecting '%s'...", p$text)
            det <- sam_detect(
              bbox = bbox,
              text = p$text,
              source = source,
              zoom = ext_zoom,
              threshold = input$threshold,
              chunked = TRUE
            )
            if (!is.null(det)) {
              det_sf <- sam_as_sf(det)
              if (!is.null(det_sf) && nrow(det_sf) > 0) {
                det_sf$prompt <- p$text
                det_sf$prompt_color <- p$color
                all_results <- c(all_results, list(det_sf))
              }
            }
          }

          if (length(all_results) == 0) {
            return(NULL)
          }

          # Combine all results - store as custom result
          combined_sf <- do.call(rbind, all_results)
          # Return a list with the combined sf for special handling
          list(combined_sf = combined_sf, prompts = active_prompts)

        } else if (input$prompt_type == "exemplar") {
          drawn <- mapgl::get_drawn_features(get_proxy())
          if (is.null(drawn) || nrow(drawn) == 0) {
            rv$status <- "Draw a rectangle around an example first."
            rv$status_type <- "warning"
            return()
          }
          # Exemplar needs imagery first
          rv$status <- "Downloading imagery..."
          img_path <- get_imagery(bbox = bbox, source = source, zoom = ext_zoom)
          rv$image_path <- img_path
          sam_detect(
            image = img_path,
            exemplar = drawn[1, ],
            threshold = input$threshold
          )

        } else if (input$prompt_type == "points") {
          if (length(rv$points) == 0) {
            rv$status <- "Click to add some points first."
            rv$status_type <- "warning"
            return()
          }
          # Points need imagery first
          rv$status <- "Downloading imagery..."
          img_path <- get_imagery(bbox = bbox, source = source, zoom = ext_zoom)
          rv$image_path <- img_path
          coords <- do.call(rbind, lapply(rv$points, function(p) c(p$lng, p$lat)))
          labels <- sapply(rv$points, function(p) p$label)
          pts_sf <- sf::st_as_sf(
            data.frame(x = coords[, 1], y = coords[, 2]),
            coords = c("x", "y"),
            crs = 4326
          )
          sam_detect(
            image = img_path,
            points = pts_sf,
            labels = labels,
            threshold = input$threshold
          )
        }
      }, error = function(e) {
        rv$status <- paste("Detection error:", e$message)
        rv$status_type <- "error"
        NULL
      })

      if (is.null(result)) {
        if (rv$status_type != "error") {
          rv$status <- "No objects detected."
          rv$status_type <- "normal"
        }
        rv$geosam <- NULL
        return()
      }

      # Clear existing layers and legend
      tryCatch({
        get_proxy() |>
          mapgl::clear_layer("detections") |>
          mapgl::clear_legend()
      }, error = function(e) NULL)

      # Handle multi-prompt results (list with combined_sf) vs single geosam
      if (is.list(result) && !is.null(result$combined_sf)) {
        # Multi-prompt text detection
        result_sf <- result$combined_sf
        active_prompts <- result$prompts
        rv$geosam <- result_sf  # Store sf for export

        if (nrow(result_sf) > 0) {
          # Use match_expr for categorical coloring
          prompt_labels <- sapply(active_prompts, function(p) p$text)
          prompt_colors_used <- sapply(active_prompts, function(p) p$color)

          fill_color <- if (length(prompt_labels) > 1) {
            mapgl::match_expr(
              column = "prompt",
              values = prompt_labels,
              stops = prompt_colors_used,
              default = "#cccccc"
            )
          } else {
            prompt_colors_used[1]
          }

          get_proxy() |>
            mapgl::add_fill_layer(
              id = "detections",
              source = result_sf,
              fill_color = fill_color,
              fill_opacity = 0.5,
              fill_outline_color = "#333333",
              popup = mapgl::concat("Prompt: ", mapgl::get_column("prompt"))
            )

          # Add legend if multiple prompts
          if (length(prompt_labels) > 1) {
            get_proxy() |>
              mapgl::add_categorical_legend(
                legend_title = "Detected Objects",
                values = prompt_labels,
                colors = prompt_colors_used,
                position = "bottom-left"
              )
          }

          rv$status <- sprintf("Found %d object(s) across %d prompt(s).", nrow(result_sf), length(active_prompts))
          rv$status_type <- "success"
        } else {
          rv$status <- "No objects detected."
          rv$status_type <- "normal"
        }
      } else {
        # Single geosam result (exemplar, points)
        rv$geosam <- result
        result_sf <- sam_as_sf(result)

        if (!is.null(result_sf) && nrow(result_sf) > 0) {
          get_proxy() |>
            mapgl::add_fill_layer(
              id = "detections",
              source = result_sf,
              fill_color = "#facc15",
              fill_opacity = 0.5,
              fill_outline_color = "#eab308"
            )

          rv$status <- sprintf("Found %d object(s).", sam_count(result))
          rv$status_type <- "success"
        } else {
          rv$status <- "No objects detected."
          rv$status_type <- "normal"
        }
      }
    })

    # Done - return result
    shiny::observeEvent(input$done, {
      shiny::stopApp(rv$geosam)
    })

    # Handle window close - also return result (don't lose work)
    session$onSessionEnded(function() {
      shiny::stopApp(rv$geosam)
    })
  }
}


#' Resolve map source with fallback
#'
#' Checks if required API keys are available for the requested source.
#' Falls back to Esri (free, no key required) if keys are missing.
#'
#' @param source Requested source: "mapbox", "esri", or "maptiler"
#' @return Resolved source (may be changed to "esri" if keys missing)
#' @noRd
.resolve_map_source <- function(source) {
  if (source == "mapbox") {
    token <- Sys.getenv("MAPBOX_PUBLIC_TOKEN", unset = "")
    if (token == "") {
      cli::cli_alert_warning(
        "MAPBOX_PUBLIC_TOKEN not set. Falling back to Esri satellite imagery."
      )
      return("esri")
    }
  } else if (source == "maptiler") {
    token <- Sys.getenv("MAPTILER_API_KEY", unset = "")
    if (token == "") {
      cli::cli_alert_warning(
        "MAPTILER_API_KEY not set. Falling back to Esri satellite imagery."
      )
      return("esri")
    }
  }
  # Esri requires no key
  source
}

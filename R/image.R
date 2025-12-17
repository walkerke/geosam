#' Detect Objects in a Local Image
#'
#' Run SAM3 object detection on a non-georeferenced image (PNG, JPG, etc.).
#' For georeferenced satellite imagery, use `sam_detect()` instead.
#'
#' @param image Path to image file (PNG, JPG, TIFF, etc.).
#' @param text Text prompt describing objects to detect (e.g., "car", "person").
#' @param points Matrix or data.frame of point coordinates in pixels (x, y).
#'   Column 1 is x (horizontal), column 2 is y (vertical, from top).
#' @param labels Integer vector of labels for point prompts (1 = foreground,
#'   0 = background). If NULL, all points are treated as foreground.
#' @param boxes Matrix or data.frame of box coordinates in pixels.
#'   Each row: (xmin, ymin, xmax, ymax).
#' @param exemplar Vector of box coordinates for an exemplar (xmin, ymin, xmax, ymax).
#' @param threshold Detection confidence threshold (0-1).
#'
#' @return A `geosam_image` object containing detection masks and metadata.
#'   Use `sam_as_sf()` to extract polygons (in pixel coordinates).
#'   Returns NULL if no objects are detected.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Text-based detection on a photo
#' result <- sam_image("photo.jpg", text = "dog")
#'
#' # Point prompts (click locations in pixels)
#' result <- sam_image("photo.jpg", points = matrix(c(100, 200, 150, 250), ncol = 2, byrow = TRUE))
#'
#' # Extract polygons
#' polys <- sam_as_sf(result)
#' }
sam_image <- function(
    image,
    text = NULL,
    points = NULL,
    labels = NULL,
    boxes = NULL,
    exemplar = NULL,
    threshold = 0.5
) {
  .ensure_python()

  # Validate image exists
 if (!file.exists(image)) {
    cli::cli_abort("Image file not found: {image}")
  }

  # Must have exactly one prompt type
  prompt_count <- sum(!is.null(text), !is.null(boxes), !is.null(points), !is.null(exemplar))
  if (prompt_count == 0) {
    cli::cli_abort("Must provide a prompt: {.arg text}, {.arg boxes}, {.arg points}, or {.arg exemplar}.")
  }
  if (prompt_count > 1) {
    cli::cli_abort("Provide only one prompt type at a time.")
  }

  # Read image using magick
  image_path <- normalizePath(image)
  cli::cli_alert_info("Reading image...")

  img <- magick::image_read(image_path)
  info <- magick::image_info(img)
  img_width <- info$width
  img_height <- info$height

  # Convert to RGB array for Python
  img_array <- .read_image_array_magick(img)

  # Get Python module
  module <- .get_module()

  # Determine prompt type and value for storing in result
  prompt_info <- if (!is.null(text)) {
    list(type = "text", value = text)
  } else if (!is.null(boxes)) {
    list(type = "boxes", value = boxes)
  } else if (!is.null(points)) {
    list(type = "points", value = points, labels = labels)
  } else if (!is.null(exemplar)) {
    list(type = "exemplar", value = exemplar)
  }

  # Run detection based on prompt type
  cli::cli_alert_info("Running SAM3 detection...")

  if (!is.null(text)) {
    # Text prompt detection
    result <- module$detect_text(
      img_array = img_array,
      text_prompt = text,
      threshold = threshold
    )

  } else if (!is.null(boxes)) {
    # Box prompt detection - already in pixel coords
    pixel_boxes <- .normalize_boxes(boxes)

    result <- module$detect_boxes(
      img_array = img_array,
      pixel_boxes = pixel_boxes,
      threshold = threshold
    )

  } else if (!is.null(points)) {
    # Point prompt detection - already in pixel coords
    pixel_points <- .normalize_points(points)

    if (is.null(labels)) {
      labels <- rep(1L, nrow(pixel_points))
    }

    result <- module$detect_points(
      img_array = img_array,
      pixel_points = pixel_points,
      labels = as.integer(labels),
      threshold = threshold,
      multi_object = TRUE
    )

  } else if (!is.null(exemplar)) {
    # Exemplar detection - find all similar objects
    pixel_box <- as.integer(exemplar)

    result <- module$detect_exemplar(
      img_array = img_array,
      pixel_box = pixel_box,
      threshold = threshold
    )
  }

  # Check if any detections
  if (result$count == 0) {
    cli::cli_alert_info("No objects detected.")
    return(NULL)
  }

  cli::cli_alert_success("Detected {result$count} object{?s}.")

  # Return geosam_image object
  new_geosam_image(
    image_path = image_path,
    masks = result$masks,
    scores = as.numeric(result$scores),
    prompt = prompt_info,
    dimensions = c(img_width, img_height),
    history = list()
  )
}


# --------------------------------------------------------------------------
# Helper functions for non-georeferenced images
# --------------------------------------------------------------------------

#' Read Image as Array using magick
#'
#' Internal function to read any image format and return as RGB array for Python.
#'
#' @param img A magick image object
#' @return Numeric array (height x width x 3)
#' @noRd
.read_image_array_magick <- function(img) {
  info <- magick::image_info(img)
  width <- info$width
  height <- info$height

  # Get raw pixel data as RGB
  # magick returns a 3D array with dims (channels=3, width, height)
  raw_data <- magick::image_data(img, channels = "rgb")

  # We need (height, width, channels) for Python/numpy
  # Use aperm directly on raw_data to preserve structure, then convert to integer
  arr <- aperm(raw_data, c(3, 2, 1))

  # Convert to integer array
  storage.mode(arr) <- "integer"

  arr
}


#' Normalize Points Input
#'
#' Convert various point formats to a consistent matrix format.
#'
#' @param points Matrix, data.frame, or list of point coordinates
#' @return Matrix with columns (x, y)
#' @noRd
.normalize_points <- function(points) {
  if (is.data.frame(points)) {
    points <- as.matrix(points[, 1:2])
  } else if (is.list(points) && !is.matrix(points)) {
    points <- do.call(rbind, lapply(points, function(p) c(p[[1]], p[[2]])))
  } else if (!is.matrix(points)) {
    points <- matrix(points, ncol = 2, byrow = TRUE)
  }

  # Ensure integer pixel coordinates
  storage.mode(points) <- "integer"

  # Return as list of [x, y] pairs for Python
  lapply(seq_len(nrow(points)), function(i) as.integer(points[i, ]))
}


#' Normalize Boxes Input
#'
#' Convert various box formats to a consistent matrix format.
#'
#' @param boxes Matrix, data.frame, or vector of box coordinates
#' @return List of [xmin, ymin, xmax, ymax] vectors for Python
#' @noRd
.normalize_boxes <- function(boxes) {
  if (is.data.frame(boxes)) {
    boxes <- as.matrix(boxes[, 1:4])
  } else if (is.vector(boxes) && length(boxes) == 4) {
    boxes <- matrix(boxes, nrow = 1)
  } else if (!is.matrix(boxes)) {
    boxes <- matrix(boxes, ncol = 4, byrow = TRUE)
  }

  # Ensure integer pixel coordinates
  storage.mode(boxes) <- "integer"

  # Return as list of [xmin, ymin, xmax, ymax] for Python
  lapply(seq_len(nrow(boxes)), function(i) as.integer(boxes[i, ]))
}


#' Create Blank MapLibre Style
#'
#' Returns a minimal MapLibre style with just a background color.
#'
#' @param bg_color Background color (default "white")
#' @return A list suitable for maplibre() style parameter
#' @noRd
.blank_maplibre_style <- function(bg_color = "white") {
  list(
    version = 8L,
    sources = structure(list(), .Names = character(0)),
    layers = list(
      list(
        id = "background",
        type = "background",
        paint = list(`background-color` = bg_color)
      )
    )
  )
}


#' Convert Image to Base64 Data URI
#'
#' Converts an image file to a base64-encoded data URI for use with
#' MapLibre's image source.
#'
#' @param image_path Path to image file
#' @return A base64-encoded data URI string
#' @noRd
.image_to_data_uri <- function(image_path) {
  img <- magick::image_read(image_path)

  # Convert to PNG format for consistency
  img_png <- magick::image_convert(img, format = "png")

  # Get raw bytes
  raw_data <- magick::image_write(img_png, format = "png")

 # Encode as base64
  base64_data <- base64enc::base64encode(raw_data)

  # Return data URI
 paste0("data:image/png;base64,", base64_data)
}


#' Get Fake Geographic Extent for Image
#'
#' Returns the fake WGS84 extent used for displaying an image in MapLibre.
#'
#' @param width Image width in pixels
#' @param height Image height in pixels
#' @return Named list with xmin, xmax, ymin, ymax
#' @noRd
.image_extent <- function(width, height) {
  scale <- 0.0001
  list(
    xmin = 0,
    xmax = width * scale,
    ymin = 0,
    ymax = height * scale
  )
}


#' Convert Pixel Coordinates to Fake Geographic
#'
#' @param x X pixel coordinate (from left)
#' @param y Y pixel coordinate (from top)
#' @param height Image height in pixels
#' @return Named vector with lng, lat
#' @noRd
.pixel_to_fake_geo <- function(x, y, height) {
  scale <- 0.0001
  c(
    lng = x * scale,
    lat = (height - y) * scale  # Flip Y axis
 )
}


#' Convert Fake Geographic to Pixel Coordinates
#'
#' @param lng Longitude (fake)
#' @param lat Latitude (fake)
#' @param height Image height in pixels
#' @return Named vector with x, y (pixel coords, y from top)
#' @noRd
.fake_geo_to_pixel <- function(lng, lat, height) {
  scale <- 0.0001
  c(
    x = as.integer(round(lng / scale)),
    y = as.integer(round(height - lat / scale))  # Flip Y axis
  )
}


# --------------------------------------------------------------------------
# Interactive image exploration
# --------------------------------------------------------------------------

#' Interactive Image Exploration and Detection
#'
#' Opens an interactive viewer to explore an image and run SAM detection
#' using text prompts, point clicks, or drawn boxes.
#'
#' @param image Path to image file (PNG, JPG, etc.). Required.
#'
#' @return A `geosam_image` object when the user clicks "Done", or NULL if cancelled.
#'
#' @details
#' The explorer provides a complete workflow:
#' 1. View and pan/zoom the image
#' 2. Select a prompt type (text, box, or points)
#' 3. Enter text or draw/click prompts on the image
#' 4. Click "Detect" to run SAM
#' 5. View results
#' 6. Click "Done" to return the geosam_image object
#'
#' The viewer uses MapLibre with a blank background and the image displayed
#' as a raster layer, enabling all standard map interactions.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Explore a photo
#' result <- sam_explore_image("photo.jpg")
#'
#' # Extract detected regions
#' if (!is.null(result)) {
#'   polygons <- sam_as_sf(result)
#' }
#' }
sam_explore_image <- function(image) {
  rlang::check_installed(c("shiny", "mapgl", "base64enc"), reason = "for interactive exploration")

  # Validate image exists
  if (!file.exists(image)) {
    cli::cli_abort("Image file not found: {image}")
  }

  image_path <- normalizePath(image)

  # Get image dimensions
  img <- magick::image_read(image_path)
  info <- magick::image_info(img)
  img_width <- info$width
  img_height <- info$height

  # Create base64 data URI for display
  img_data_uri <- .image_to_data_uri(image_path)
  img_ext <- .image_extent(img_width, img_height)

  # Calculate image coordinates for MapLibre (clockwise from top-left)
  img_coords <- list(
    c(img_ext$xmin, img_ext$ymax),  # top-left
    c(img_ext$xmax, img_ext$ymax),  # top-right
    c(img_ext$xmax, img_ext$ymin),  # bottom-right
    c(img_ext$xmin, img_ext$ymin)   # bottom-left
  )

  # Run gadget and return result
  result <- shiny::runGadget(
    app = shiny::shinyApp(
      ui = .explore_image_ui(),
      server = .explore_image_server(image_path, img_data_uri, img_coords, img_ext, img_width, img_height)
    ),
    viewer = shiny::dialogViewer("geosam - Image", width = 1200, height = 800)
  )

  result
}


#' @noRd
.explore_image_ui <- function() {
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
            btn.innerHTML = 'Detect';
            btn.classList.remove('detecting');
          }
        });
      "))
    ),

    mapgl::maplibreOutput("map", height = "100%"),

    shiny::div(
      class = "control-panel",

      shiny::h4("geosam"),
      shiny::div(class = "subtitle", "Image Object Detection"),

      # Image info
      shiny::div(class = "section-label", "Image"),
      shiny::div(
        class = "info-row",
        shiny::span(class = "info-label", "Size:"),
        shiny::span(class = "info-value", shiny::textOutput("image_size", inline = TRUE))
      ),

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

      # Detect button
      shiny::actionButton("detect", "Detect", class = "btn-primary"),

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
.explore_image_server <- function(image_path, img_data_uri, img_coords, img_ext, img_width, img_height) {
  function(input, output, session) {
    # Color palette for prompts
    prompt_colors <- c("#facc15", "#3b82f6", "#ef4444", "#22c55e", "#a855f7", "#f97316")

    # Reactive values
    rv <- shiny::reactiveValues(
      geosam_image = NULL,
      points = list(),
      status = "Select a mode and create prompts, then click 'Detect'.",
      status_type = "normal",
      prompts = list(list(id = 1, text = "", color = "#facc15")),
      next_prompt_id = 2,
      detecting = FALSE
    )

    # Get proxy helper
    get_proxy <- function() {
      mapgl::maplibre_proxy("map")
    }

    # Render map with image
    output$map <- mapgl::renderMaplibre({
      mapgl::maplibre(style = .blank_maplibre_style()) |>
        mapgl::add_image_source(
          id = "user-image",
          url = img_data_uri,
          coordinates = img_coords
        ) |>
        mapgl::add_raster_layer(
          id = "user-image-layer",
          source = "user-image"
        ) |>
        mapgl::fit_bounds(
          c(img_ext$xmin, img_ext$ymin, img_ext$xmax, img_ext$ymax)
        ) |>
        mapgl::add_navigation_control(position = "top-left")
    })

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
            placeholder = if (i == 1) "e.g., dog, car" else "another object type"
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

    # Add prompt button
    shiny::observeEvent(input$add_prompt, {
      if (length(rv$prompts) < 6) {
        new_color <- prompt_colors[min(length(rv$prompts) + 1, length(prompt_colors))]
        rv$prompts <- c(rv$prompts, list(list(
          id = rv$next_prompt_id,
          text = "",
          color = new_color
        )))
        rv$next_prompt_id <- rv$next_prompt_id + 1
      }
    })

    # Remove prompt observers
    shiny::observe({
      prompts <- rv$prompts
      lapply(prompts, function(p) {
        btn_id <- paste0("remove_prompt_", p$id)
        shiny::observeEvent(input[[btn_id]], {
          rv$prompts <- Filter(function(x) x$id != p$id, rv$prompts)
        }, ignoreInit = TRUE, once = TRUE)
      })
    })

    # Update prompt text when inputs change
    shiny::observe({
      prompts <- rv$prompts
      for (i in seq_along(prompts)) {
        p <- prompts[[i]]
        input_id <- paste0("prompt_text_", p$id)
        new_text <- input[[input_id]]
        if (!is.null(new_text) && new_text != p$text) {
          rv$prompts[[i]]$text <- new_text
        }
      }
    })

    # Image size display
    output$image_size <- shiny::renderText({
      sprintf("%d x %d px", img_width, img_height)
    })

    # Count displays
    output$n_detections <- shiny::renderText({
      result <- rv$geosam_image
      if (is.null(result)) {
        "0"
      } else if (inherits(result, "sf")) {
        as.character(nrow(result))
      } else {
        as.character(length(result$masks))
      }
    })

    output$n_points <- shiny::renderText({
      as.character(length(rv$points))
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
          # Convert fake geo to pixel
          pixel <- .fake_geo_to_pixel(click$lng, click$lat, img_height)

          # Check bounds
          if (pixel["x"] >= 0 && pixel["x"] < img_width &&
              pixel["y"] >= 0 && pixel["y"] < img_height) {

            mode <- input$point_mode %||% "positive"
            label <- if (mode == "positive") 1L else 0L
            rv$points <- c(rv$points, list(list(
              x = pixel["x"],
              y = pixel["y"],
              lng = click$lng,
              lat = click$lat,
              label = label
            )))

            # Add marker
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
      get_proxy() |>
        mapgl::clear_draw()
      rv$status <- "Box cleared."
      rv$status_type <- "normal"
    })

    # Clear results
    shiny::observeEvent(input$clear_results, {
      rv$geosam_image <- NULL
      tryCatch({
        get_proxy() |>
          mapgl::clear_layer("detections")
      }, error = function(e) NULL)
      rv$status <- "Results cleared."
      rv$status_type <- "normal"
    })

    # Run detection
    shiny::observeEvent(input$detect, {
      session$sendCustomMessage("setDetecting", TRUE)
      rv$status <- "Running SAM detection..."
      rv$status_type <- "normal"

      # Use on.exit to reset button
      on.exit(session$sendCustomMessage("setDetecting", FALSE))

      result <- tryCatch({
        if (input$prompt_type == "text") {
          # Get all prompts with non-empty text
          active_prompts <- Filter(function(p) nchar(p$text) > 0, rv$prompts)
          if (length(active_prompts) == 0) {
            rv$status <- "Enter at least one text description."
            rv$status_type <- "warning"
            return()
          }

          # Run detection for each prompt and combine results
          all_results <- list()
          for (p in active_prompts) {
            rv$status <- sprintf("Detecting '%s'...", p$text)
            det <- sam_image(
              image = image_path,
              text = p$text,
              threshold = input$threshold
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
          list(combined_sf = combined_sf, prompts = active_prompts)

        } else if (input$prompt_type == "exemplar") {
          drawn <- mapgl::get_drawn_features(get_proxy())
          if (is.null(drawn) || nrow(drawn) == 0) {
            rv$status <- "Draw a rectangle around an example object first."
            rv$status_type <- "warning"
            return()
          }
          # Convert drawn box to pixel coordinates for exemplar
          bbox <- sf::st_bbox(drawn[1, ])
          p1 <- .fake_geo_to_pixel(bbox["xmin"], bbox["ymin"], img_height)
          p2 <- .fake_geo_to_pixel(bbox["xmax"], bbox["ymax"], img_height)
          pixel_box <- c(
            min(p1["x"], p2["x"]),
            min(p1["y"], p2["y"]),
            max(p1["x"], p2["x"]),
            max(p1["y"], p2["y"])
          )
          sam_image(
            image = image_path,
            exemplar = pixel_box,
            threshold = input$threshold
          )

        } else if (input$prompt_type == "points") {
          if (length(rv$points) == 0) {
            rv$status <- "Click to add some points first."
            rv$status_type <- "warning"
            return()
          }
          pixel_points <- do.call(rbind, lapply(rv$points, function(p) c(p$x, p$y)))
          labels <- sapply(rv$points, function(p) p$label)
          sam_image(
            image = image_path,
            points = pixel_points,
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
        if (rv$status_type != "error" && rv$status_type != "warning") {
          rv$status <- "No objects detected."
          rv$status_type <- "normal"
        }
        rv$geosam_image <- NULL
        return()
      }

      # Clear existing layers and legend
      tryCatch({
        get_proxy() |>
          mapgl::clear_layer("detections") |>
          mapgl::clear_legend()
      }, error = function(e) NULL)

      # Handle multi-prompt results (list with combined_sf) vs single geosam_image
      if (is.list(result) && !is.null(result$combined_sf)) {
        # Multi-prompt text detection
        result_sf <- result$combined_sf
        active_prompts <- result$prompts
        rv$geosam_image <- result_sf  # Store sf for export

        if (nrow(result_sf) > 0) {
          # Convert pixel coords to fake geo for display
          result_sf_geo <- .convert_pixel_sf_to_geo(result_sf, img_height)

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
              source = result_sf_geo,
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
        # Single geosam_image result (box, points)
        rv$geosam_image <- result
        result_sf <- sam_as_sf(result)

        if (!is.null(result_sf) && nrow(result_sf) > 0) {
          # Convert pixel coords to fake geo for display
          result_sf_geo <- .convert_pixel_sf_to_geo(result_sf, img_height)

          get_proxy() |>
            mapgl::add_fill_layer(
              id = "detections",
              source = result_sf_geo,
              fill_color = "#facc15",
              fill_opacity = 0.5,
              fill_outline_color = "#eab308"
            )

          rv$status <- sprintf("Found %d object(s).", length(result$masks))
          rv$status_type <- "success"
        } else {
          rv$status <- "No objects detected."
          rv$status_type <- "normal"
        }
      }
    })

    # Done - return result
    shiny::observeEvent(input$done, {
      shiny::stopApp(rv$geosam_image)
    })

    # Handle window close
    session$onSessionEnded(function() {
      shiny::stopApp(rv$geosam_image)
    })
  }
}


#' Convert Pixel-Space SF to Fake Geographic
#'
#' Internal function to convert pixel-space polygons to fake WGS84
#' for display in MapLibre.
#'
#' @param sf_obj An sf object with pixel coordinates
#' @param img_height Image height in pixels
#' @return An sf object with fake WGS84 coordinates
#' @noRd
.convert_pixel_sf_to_geo <- function(sf_obj, img_height) {
  scale <- 0.0001

  # Transform each geometry individually to preserve structure
  geom <- sf::st_geometry(sf_obj)

  geom_transformed <- lapply(seq_along(geom), function(i) {
    g <- geom[[i]]
    # Apply affine transform: scale and flip Y
    # x' = x * scale, y' = (img_height - y) * scale
    g_scaled <- g * scale
    g_flipped <- g_scaled * c(1, -1) + c(0, img_height * scale)
    g_flipped
  })

  sf::st_geometry(sf_obj) <- sf::st_sfc(geom_transformed, crs = 4326)
  sf_obj
}


# --------------------------------------------------------------------------
# View results on image
# --------------------------------------------------------------------------

#' View Detection Results on Image
#'
#' Opens an interactive viewer to display detection results overlaid on the
#' source image.
#'
#' @param x A `geosam_image` object from `sam_image()` or `sam_explore_image()`.
#'
#' @return The `geosam_image` object when the user clicks "Done".
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- sam_image("photo.jpg", text = "dog")
#' sam_view_image(result)
#' }
sam_view_image <- function(x) {
  validate_geosam_image(x)
  rlang::check_installed(c("shiny", "mapgl", "base64enc"), reason = "for interactive viewing")

  # Get image info
  image_path <- x$image_path
  img_width <- x$dimensions[1]
  img_height <- x$dimensions[2]

  # Create base64 data URI for display
  img_data_uri <- .image_to_data_uri(image_path)
  img_ext <- .image_extent(img_width, img_height)

  # Calculate image coordinates for MapLibre (clockwise from top-left)
  img_coords <- list(
    c(img_ext$xmin, img_ext$ymax),  # top-left
    c(img_ext$xmax, img_ext$ymax),  # top-right
    c(img_ext$xmax, img_ext$ymin),  # bottom-right
    c(img_ext$xmin, img_ext$ymin)   # bottom-left
  )

  # Run the gadget
  shiny::runGadget(
    app = shiny::shinyApp(
      ui = .view_image_ui(),
      server = .view_image_server(x, img_data_uri, img_coords, img_ext, img_width, img_height)
    ),
    viewer = shiny::dialogViewer("geosam - View", width = 1000, height = 700)
  )
}


#' @noRd
.view_image_ui <- function() {
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

        .section-label {
          font-size: 10px;
          font-weight: 600;
          color: #999;
          text-transform: uppercase;
          letter-spacing: 0.5px;
          margin: 12px 0 6px 0;
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

        .divider { height: 1px; background: #eee; margin: 12px 0; }
      "))
    ),

    mapgl::maplibreOutput("map", height = "100%"),

    shiny::div(
      class = "control-panel",

      shiny::h4("View Detections"),

      # Image info
      shiny::div(class = "section-label", "Image"),
      shiny::div(
        class = "info-row",
        shiny::span(class = "info-label", "Size:"),
        shiny::span(class = "info-value", shiny::textOutput("image_size", inline = TRUE))
      ),

      shiny::div(class = "divider"),

      # Results stats
      shiny::div(class = "section-label", "Results"),
      shiny::div(
        class = "info-row",
        shiny::span(class = "info-label", "Detections:"),
        shiny::span(class = "info-value", shiny::textOutput("n_detections", inline = TRUE))
      ),

      shiny::div(class = "divider"),

      shiny::actionButton("done", "Done", class = "btn-success")
    )
  )
}


#' @noRd
.view_image_server <- function(geosam_img, img_data_uri, img_coords, img_ext, img_width, img_height) {
  function(input, output, session) {
    # Convert masks to sf for display
    shiny::showNotification("Converting masks to polygons...", id = "loading", duration = NULL)
    result_sf <- sam_as_sf(geosam_img)
    shiny::removeNotification("loading")

    # Convert to fake geo coords for display
    result_sf_geo <- NULL
    if (!is.null(result_sf) && nrow(result_sf) > 0) {
      result_sf_geo <- .convert_pixel_sf_to_geo(result_sf, img_height)
    }

    # Render map
    output$map <- mapgl::renderMaplibre({
      m <- mapgl::maplibre(style = .blank_maplibre_style()) |>
        mapgl::add_image_source(
          id = "user-image",
          url = img_data_uri,
          coordinates = img_coords
        ) |>
        mapgl::add_raster_layer(
          id = "user-image-layer",
          source = "user-image"
        ) |>
        mapgl::fit_bounds(
          c(img_ext$xmin, img_ext$ymin, img_ext$xmax, img_ext$ymax)
        ) |>
        mapgl::add_navigation_control(position = "top-left")

      # Add detection polygons
      if (!is.null(result_sf_geo) && nrow(result_sf_geo) > 0) {
        m <- m |>
          mapgl::add_fill_layer(
            id = "detections",
            source = result_sf_geo,
            fill_color = "#facc15",
            fill_opacity = 0.5,
            fill_outline_color = "#eab308"
          )
      }

      m
    })

    # Display info
    output$image_size <- shiny::renderText({
      sprintf("%d x %d px", img_width, img_height)
    })

    output$n_detections <- shiny::renderText({
      length(geosam_img$masks)
    })

    # Done - return result
    shiny::observeEvent(input$done, {
      shiny::stopApp(geosam_img)
    })

    # Handle window close
    session$onSessionEnded(function() {
      shiny::stopApp(geosam_img)
    })
  }
}

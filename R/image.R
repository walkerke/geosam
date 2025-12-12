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
    # Exemplar detection - use box as starting point
    pixel_boxes <- .normalize_boxes(matrix(exemplar, nrow = 1))

    result <- module$detect_boxes(
      img_array = img_array,
      pixel_boxes = pixel_boxes,
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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
#' @keywords internal
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


#' @keywords internal
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
        .control-panel .irs--shiny .irs-bar { background: #154733; border-top-color: #154733; border-bottom-color: #154733; }
        .control-panel .irs--shiny .irs-single { background: #154733; }
        .control-panel .irs--shiny .irs-handle { border-color: #154733; }
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
        choices = c("Text Prompt" = "text", "Draw Box" = "box", "Click Points" = "points"),
        selected = "text",
        inline = TRUE
      ),

      # Text mode panel
      shiny::conditionalPanel(
        condition = "input.prompt_type == 'text'",
        shiny::p(class = "help-text", "Describe objects to find in the image."),
        shiny::textInput("text_prompt", label = NULL, placeholder = "e.g., dog, car, person")
      ),

      # Box mode panel
      shiny::conditionalPanel(
        condition = "input.prompt_type == 'box'",
        shiny::p(class = "help-text", "Draw a rectangle around an object to detect."),
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


#' @keywords internal
.explore_image_server <- function(image_path, img_data_uri, img_coords, img_ext, img_width, img_height) {
  function(input, output, session) {
    # Reactive values
    rv <- shiny::reactiveValues(
      geosam_image = NULL,
      points = list(),
      status = "Select a mode and create prompts, then click 'Detect'.",
      status_type = "normal"
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
      shiny::req(input$map_loaded)
      get_proxy() |>
        mapgl::add_draw_control(
          position = "top-left",
          displayControlsDefault = FALSE,
          controls = list(polygon = TRUE, trash = TRUE),
          rectangle = TRUE
        )
    }) |> shiny::bindEvent(input$map_loaded, once = TRUE)

    # Image size display
    output$image_size <- shiny::renderText({
      sprintf("%d x %d px", img_width, img_height)
    })

    # Count displays
    output$n_detections <- shiny::renderText({
      if (is.null(rv$geosam_image)) "0" else as.character(length(rv$geosam_image$masks))
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
      rv$status <- "Running SAM detection..."
      rv$status_type <- "normal"

      result <- tryCatch({
        if (input$prompt_type == "text") {
          if (is.null(input$text_prompt) || nchar(input$text_prompt) == 0) {
            rv$status <- "Enter a text description."
            rv$status_type <- "warning"
            return()
          }
          sam_image(
            image = image_path,
            text = input$text_prompt,
            threshold = input$threshold
          )

        } else if (input$prompt_type == "box") {
          drawn <- mapgl::get_drawn_features(get_proxy())
          if (is.null(drawn) || nrow(drawn) == 0) {
            rv$status <- "Draw a rectangle first."
            rv$status_type <- "warning"
            return()
          }
          # Convert drawn box to pixel coordinates
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
            boxes = matrix(pixel_box, nrow = 1),
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

      rv$geosam_image <- result

      # Add results to map
      result_sf <- sam_as_sf(result)
      if (!is.null(result_sf) && nrow(result_sf) > 0) {
        # Convert pixel coords to fake geo for display
        result_sf_geo <- .convert_pixel_sf_to_geo(result_sf, img_height)

        tryCatch({
          get_proxy() |>
            mapgl::clear_layer("detections")
        }, error = function(e) NULL)

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
#' @keywords internal
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


#' @keywords internal
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


#' @keywords internal
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

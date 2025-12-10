#' Detect Objects in Geospatial Imagery Using SAM3
#'
#' Main function for object detection using Meta's SAM3 model. Supports text
#' prompts, point prompts, box prompts, and exemplar-based detection.
#'
#' @param image Path to a GeoTIFF image, or NULL to download imagery for bbox.
#' @param bbox Bounding box for the area of interest. Can be a numeric vector
#'   `c(xmin, ymin, xmax, ymax)` in WGS84, or an sf/sfc object.
#' @param text Text prompt describing objects to detect (e.g., "well pad",
#'   "swimming pool", "solar panel"). Uses SAM3's open-vocabulary detection.
#' @param boxes sf object with polygons/boxes to use as box prompts.
#' @param points sf object with points to use as point prompts.
#' @param labels Integer vector of labels for point prompts (1 = foreground,
#'   0 = background). If NULL, all points are treated as foreground.
#' @param exemplar sf polygon representing an example object. SAM3 will find
#'   all similar objects in the image.
#' @param source Imagery source if downloading: "mapbox", "esri", or "google".
#' @param zoom Tile zoom level for imagery download (17-19 recommended).
#' @param threshold Detection confidence threshold (0-1). Lower values return
#'   more detections.
#'
#' @return A `geosam` object containing detection masks and metadata.
#'   Use `sam_as_sf()` to extract polygons, `sam_filter()` to filter by area/score.
#'   Returns NULL if no objects are detected.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Text-based detection
#' result <- sam_detect(
#'   bbox = c(-102.5, 31.8, -102.4, 31.9),
#'   text = "well pad"
#' )
#' pads <- result |> sam_filter(min_area = 500) |> sam_as_sf()
#'
#' # Point prompts on existing image
#' result <- sam_detect(
#'   image = "satellite.tif",
#'   points = my_points_sf
#' )
#'
#' # Box prompts
#' result <- sam_detect(
#'   image = "satellite.tif",
#'   boxes = my_boxes_sf
#' )
#'
#' # Exemplar-based detection
#' result <- sam_detect(
#'   image = "satellite.tif",
#'   exemplar = my_example_polygon
#' )
#' }
sam_detect <- function(
    image = NULL,
    bbox = NULL,
    text = NULL,
    boxes = NULL,
    points = NULL,
    labels = NULL,
    exemplar = NULL,
    source = "mapbox",
    zoom = 17,
    threshold = 0.5
) {
  # Validate inputs
  .ensure_python()

  # Must have either image or bbox
  if (is.null(image) && is.null(bbox)) {
    cli::cli_abort("Must provide either {.arg image} or {.arg bbox}.")
  }

  # Must have exactly one prompt type
  prompt_count <- sum(!is.null(text), !is.null(boxes), !is.null(points), !is.null(exemplar))
  if (prompt_count == 0) {
    cli::cli_abort("Must provide a prompt: {.arg text}, {.arg boxes}, {.arg points}, or {.arg exemplar}.")
  }
  if (prompt_count > 1) {
    cli::cli_abort("Provide only one prompt type at a time.")
  }

  # Convert bbox if sf object
  if (!is.null(bbox)) {
    if (inherits(bbox, c("sf", "sfc"))) {
      bbox <- .sf_to_coords(bbox, "bbox")
    } else if (is.numeric(bbox) && length(bbox) == 4) {
      # Already in correct format
    } else {
      cli::cli_abort("{.arg bbox} must be a numeric vector of length 4 or an sf object.")
    }
  }

  # Download imagery if needed
  if (is.null(image)) {
    cli::cli_alert_info("Downloading satellite imagery...")
    image <- get_imagery(
      bbox = bbox,
      source = source,
      zoom = zoom
    )
  }

  # Verify image exists
  if (!file.exists(image)) {
    cli::cli_abort("Image file not found: {image}")
  }

  # Get Python module
  module <- .get_module()

  # Read image as array for Python (R handles all geospatial)
  image_path <- normalizePath(image)
  cli::cli_alert_info("Reading image...")
  img_array <- .read_image_array(image_path)

  # Get image metadata for geosam object
  template <- terra::rast(image_path)
  img_extent <- as.vector(terra::ext(template))
  img_crs <- terra::crs(template, proj = TRUE)

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
    # Box prompt detection - convert geographic coords to pixels in R
    box_coords <- .sf_to_coords(boxes, "boxes")
    pixel_boxes <- .geo_boxes_to_pixel(box_coords, image_path)

    result <- module$detect_boxes(
      img_array = img_array,
      pixel_boxes = pixel_boxes,
      threshold = threshold
    )

  } else if (!is.null(points)) {
    # Point prompt detection - convert geographic coords to pixels in R
    point_coords <- .sf_to_coords(points, "points")
    pixel_points <- .geo_to_pixel(point_coords, image_path)

    # Default labels to foreground
    if (is.null(labels)) {
      labels <- rep(1L, length(pixel_points))
    }

    result <- module$detect_points(
      img_array = img_array,
      pixel_points = pixel_points,
      labels = as.integer(labels),
      threshold = threshold
    )

  } else if (!is.null(exemplar)) {
    # Exemplar-based detection
    exemplar_box <- .sf_to_coords(exemplar, "bbox")
    pixel_boxes <- .geo_boxes_to_pixel(list(exemplar_box), image_path)

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

  # Return geosam object (masks stay as arrays for refinement)
  new_geosam(
    image_path = image_path,
    masks = result$masks,
    scores = as.numeric(result$scores),
    prompt = prompt_info,
    extent = img_extent,
    crs = img_crs,
    history = list()
  )
}


#' Load SAM3 Model
#'
#' Loads the SAM3 model into memory. Useful for interactive use or Shiny apps
#' where you want to keep the model warm between calls.
#'
#' @param device Device to load model on: "auto", "mps", "cuda", or "cpu".
#'
#' @return Invisibly returns TRUE on success.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Load model before making multiple detections
#' sam_load()
#'
#' # Now detections are faster
#' result1 <- sam_detect(image = "img1.tif", text = "building")
#' result2 <- sam_detect(image = "img2.tif", text = "building")
#'
#' # Unload when done
#' sam_unload()
#' }
sam_load <- function(device = "auto") {
  .ensure_python()

  module <- .get_module()

  if (device == "auto") {
    device <- NULL
  }

  cli::cli_alert_info("Loading SAM3 model...")
  module$load_model(device = device)

  .geosam_env$model_loaded <- TRUE
  cli::cli_alert_success("SAM3 model loaded.")

  invisible(TRUE)
}


#' Unload SAM3 Model
#'
#' Unloads the SAM3 model from memory to free GPU/CPU resources.
#'
#' @return Invisibly returns TRUE.
#'
#' @export
sam_unload <- function() {
  module <- .get_module()
  module$unload_model()

  .geosam_env$model_loaded <- FALSE
  cli::cli_alert_success("SAM3 model unloaded.")

  invisible(TRUE)
}


#' Check if Model is Loaded
#'
#' Checks whether the SAM3 model is currently loaded in memory.
#'
#' @return Logical indicating whether the model is loaded.
#'
#' @export
sam_is_loaded <- function() {
  if (is.null(.geosam_env$python_module)) {
    return(FALSE)
  }

  module <- .get_module()
  module$is_model_loaded()
}

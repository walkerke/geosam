#' Filter Detections by Area or Score
#'
#' Filters detections in a geosam object based on area and/or confidence score.
#'
#' @param x A geosam object.
#' @param min_area Minimum area in square meters.
#' @param max_area Maximum area in square meters.
#' @param min_score Minimum confidence score (0-1).
#'
#' @return A new geosam object with filtered detections.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- sam_detect(image = "satellite.tif", text = "building")
#' filtered <- result |> sam_filter(min_area = 100, min_score = 0.7)
#' }
sam_filter <- function(x, min_area = NULL, max_area = NULL, min_score = NULL) {
  validate_geosam(x)

  if (length(x$masks) == 0) {
    return(x)
  }

  # Calculate areas if needed
  if (!is.null(min_area) || !is.null(max_area)) {
    sf_result <- .masks_to_sf(
      masks = x$masks,
      scores = x$scores,
      template_raster = x$image_path,
      min_area_m2 = 0,
      max_area_m2 = Inf
    )

    if (is.null(sf_result)) {
      return(new_geosam(
        image_path = x$image_path,
        masks = list(),
        scores = numeric(),
        prompt = x$prompt,
        extent = x$extent,
        crs = x$crs,
        history = c(x$history, list(list(action = "filter", kept = 0)))
      ))
    }

    areas <- sf_result$area_m2
  } else {
    areas <- rep(Inf, length(x$masks))
  }

  # Build keep index
  keep <- rep(TRUE, length(x$masks))

  if (!is.null(min_area)) {
    keep <- keep & (areas >= min_area)
  }
  if (!is.null(max_area)) {
    keep <- keep & (areas <= max_area)
  }
  if (!is.null(min_score)) {
    keep <- keep & (x$scores >= min_score)
  }

  # Return filtered geosam
  new_geosam(
    image_path = x$image_path,
    masks = x$masks[keep],
    scores = x$scores[keep],
    prompt = x$prompt,
    extent = x$extent,
    crs = x$crs,
    history = c(x$history, list(list(
      action = "filter",
      min_area = min_area,
      max_area = max_area,
      min_score = min_score,
      kept = sum(keep)
    )))
  )
}


#' Select Specific Detections by Index
#'
#' Subsets a geosam object to include only specific detections.
#'
#' @param x A geosam object.
#' @param index Integer vector of indices to keep.
#'
#' @return A new geosam object with only the selected detections.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- sam_detect(image = "satellite.tif", text = "building")
#' # Keep only the top 3 scoring detections
#' top3 <- sam_select(result, order(sam_scores(result), decreasing = TRUE)[1:3])
#' }
sam_select <- function(x, index) {
  validate_geosam(x)

  if (length(x$masks) == 0) {
    cli::cli_warn("No detections to select from.")
    return(x)
  }

  # Validate indices
  if (any(index < 1) || any(index > length(x$masks))) {
    cli::cli_abort("Index out of bounds. Valid range: 1 to {length(x$masks)}.")
  }

  new_geosam(
    image_path = x$image_path,
    masks = x$masks[index],
    scores = x$scores[index],
    prompt = x$prompt,
    extent = x$extent,
    crs = x$crs,
    history = c(x$history, list(list(action = "select", index = index)))
  )
}


#' Refine Detection with Additional Point Prompts
#'
#' Re-runs detection with additional positive or negative point prompts to refine results.
#'
#' @param x A geosam object.
#' @param points sf object with point geometries, or a matrix/data frame with columns x, y.
#' @param labels Integer vector of labels (1 = foreground/positive, 0 = background/negative).
#'   If NULL, all points are treated as positive.
#'
#' @return A new geosam object with refined detections.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- sam_detect(image = "satellite.tif", text = "building")
#' # Add positive and negative points to refine
#' refined <- sam_refine(result,
#'   points = data.frame(x = c(-102.5, -102.51), y = c(31.8, 31.81)),
#'   labels = c(1, 0)  # first is positive, second is negative
#' )
#' }
sam_refine <- function(x, points, labels = NULL) {
  validate_geosam(x)

  .ensure_python()

  # Convert points to sf if needed
  if (inherits(points, c("matrix", "data.frame"))) {
    if (ncol(points) < 2) {
      cli::cli_abort("Points must have at least 2 columns (x, y).")
    }
    points <- sf::st_as_sf(
      as.data.frame(points[, 1:2]),
      coords = c(1, 2),
      crs = 4326
    )
  }

  # Default labels to positive
  if (is.null(labels)) {
    labels <- rep(1L, nrow(points))
  }

  # Convert to pixel coordinates
  point_coords <- .sf_to_coords(points, "points")
  pixel_points <- .geo_to_pixel(point_coords, x$image_path)

  # Read image array
  img_array <- .read_image_array(x$image_path)

  # Run point detection
  module <- .get_module()
  result <- module$detect_points(
    img_array = img_array,
    pixel_points = pixel_points,
    labels = as.integer(labels),
    threshold = 0.5
  )

  if (result$count == 0) {
    cli::cli_alert_info("Refinement produced no detections.")
    return(NULL)
  }

  cli::cli_alert_success("Refined to {result$count} detection{?s}.")

  new_geosam(
    image_path = x$image_path,
    masks = result$masks,
    scores = as.numeric(result$scores),
    prompt = list(type = "points", value = points, labels = labels),
    extent = x$extent,
    crs = x$crs,
    history = c(x$history, list(list(action = "refine", n_points = nrow(points))))
  )
}


#' Find Similar Objects Using Selected Detection as Exemplar
#'
#' Uses a single selected detection as an exemplar to find all similar objects
#' in the image.
#'
#' @param x A geosam object with exactly one detection (use `sam_select()` first).
#'
#' @return A new geosam object with all detected similar objects.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- sam_detect(image = "satellite.tif", text = "well pad")
#' # View results, pick the best one, find all similar
#' similar <- result |>
#'   sam_select(3) |>
#'   sam_find_similar()
#' }
sam_find_similar <- function(x) {
  validate_geosam(x)

  if (length(x$masks) != 1) {
    cli::cli_abort(
      "Must have exactly one detection selected. Use {.fn sam_select} first.",
      "i" = "Current number of detections: {length(x$masks)}"
    )
  }

  .ensure_python()

  # Get bounding box of the selected mask
  sf_mask <- .masks_to_sf(
    masks = x$masks,
    scores = x$scores,
    template_raster = x$image_path,
    min_area_m2 = 0,
    max_area_m2 = Inf
  )

  if (is.null(sf_mask)) {
    cli::cli_abort("Could not extract bounding box from selected detection.")
  }

  bbox <- sf::st_bbox(sf_mask)
  exemplar_box <- c(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"])

  # Convert to pixel coordinates
  pixel_boxes <- .geo_boxes_to_pixel(list(as.numeric(exemplar_box)), x$image_path)

  # Read image and run detection
  img_array <- .read_image_array(x$image_path)
  module <- .get_module()

  result <- module$detect_boxes(
    img_array = img_array,
    pixel_boxes = pixel_boxes,
    threshold = 0.5
  )

  if (result$count == 0) {
    cli::cli_alert_info("No similar objects found.")
    return(NULL)
  }

  cli::cli_alert_success("Found {result$count} similar object{?s}.")

  new_geosam(
    image_path = x$image_path,
    masks = result$masks,
    scores = as.numeric(result$scores),
    prompt = list(type = "exemplar", value = sf_mask),
    extent = x$extent,
    crs = x$crs,
    history = c(x$history, list(list(action = "find_similar")))
  )
}

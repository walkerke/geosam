#' Create a geosam Object
#'
#' Internal constructor for geosam class objects. Users typically get geosam
#' objects from `sam_detect()` or `sam_explore()`, not by calling this directly.
#'
#' @param image_path Path to the source GeoTIFF image.
#' @param masks List of binary mask arrays from SAM detection.
#' @param scores Numeric vector of confidence scores per mask.
#' @param prompt List with `type` and `value` describing the prompt used.
#' @param extent Numeric vector of length 4: c(xmin, ymin, xmax, ymax).
#' @param crs CRS string (typically "EPSG:3857").
#' @param history List tracking refinement steps.
#'
#' @return A geosam object.
#' @keywords internal
new_geosam <- function(
    image_path = character(),
    masks = list(),
    scores = numeric(),
    prompt = list(),
    extent = numeric(4),
    crs = character(),
    history = list()
) {
  structure(
    list(
      image_path = image_path,
      masks = masks,
      scores = scores,
      prompt = prompt,
      extent = extent,
      crs = crs,
      history = history
    ),
    class = "geosam"
  )
}


#' Print Method for geosam Objects
#'
#' @param x A geosam object.
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns `x`.
#' @export
print.geosam <- function(x, ...) {
  n_detections <- length(x$masks)

  cli::cli_text("{.cls geosam} object")
  cli::cli_text("
 Detections: {n_detections}")

  if (!is.null(x$prompt$type)) {
    prompt_desc <- switch(x$prompt$type,
      text = sprintf("text (\"%s\")", x$prompt$value),
      boxes = sprintf("boxes (%d)", length(x$prompt$value)),
      points = sprintf("points (%d)", length(x$prompt$value)),
      exemplar = "exemplar",
      x$prompt$type
    )
    cli::cli_text("  Prompt: {prompt_desc}")
  }

  if (length(x$image_path) > 0 && nchar(x$image_path) > 0) {
    cli::cli_text("  Image: {basename(x$image_path)}")
  }

  if (length(x$history) > 0) {
    cli::cli_text("  History: {length(x$history)} refinement step{?s}")
  }

  if (n_detections > 0) {
    cli::cli_text("")
    cli::cli_text("
 Use {.fn sam_as_sf} to extract polygons")
  }

  invisible(x)
}


#' Check if Object is a geosam
#'
#' @param x Object to test.
#' @return Logical indicating if `x` is a geosam object.
#' @export
is_geosam <- function(x) {
  inherits(x, "geosam")
}


#' Validate geosam Object
#'
#' Internal function to validate geosam object structure.
#'
#' @param x A geosam object.
#' @return The validated geosam object (invisibly).
#' @keywords internal
validate_geosam <- function(x) {
  if (!inherits(x, "geosam")) {
    cli::cli_abort("{.arg x} must be a {.cls geosam} object.")
  }

  if (length(x$masks) != length(x$scores)) {
    cli::cli_abort("Number of masks ({length(x$masks)}) must equal number of scores ({length(x$scores)}).")
  }

  invisible(x)
}

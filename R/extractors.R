#' Extract sf Polygons from geosam Object
#'
#' Converts detection masks from a geosam object to sf polygon geometries.
#'
#' @param x A geosam object from `sam_detect()` or `sam_explore()`.
#' @param min_area Minimum object area in square meters. Objects smaller are filtered out.
#' @param max_area Maximum object area in square meters. Objects larger are filtered out.
#'
#' @return An sf data frame with polygon geometries, scores, and area.
#'   Returns NULL if no polygons remain after filtering.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- sam_detect(image = "satellite.tif", text = "building")
#' buildings <- sam_as_sf(result, min_area = 100)
#' }
sam_as_sf <- function(x, min_area = NULL, max_area = NULL) {
  validate_geosam(x)

  if (length(x$masks) == 0) {
    cli::cli_alert_warning("No masks to convert.")
    return(NULL)
  }

  min_area_m2 <- min_area %||% 0
  max_area_m2 <- max_area %||% Inf

  .masks_to_sf(
    masks = x$masks,
    scores = x$scores,
    template_raster = x$image_path,
    min_area_m2 = min_area_m2,
    max_area_m2 = max_area_m2
  )
}


#' Extract Raster Masks from geosam Object
#'
#' Converts detection masks from a geosam object to a terra SpatRaster.
#'
#' @param x A geosam object from `sam_detect()` or `sam_explore()`.
#' @param combined If TRUE, combine all masks into a single layer (union).
#'   If FALSE (default), return a multi-layer raster with one layer per detection.
#'
#' @return A terra SpatRaster with mask values (1 = detected, 0 = background).
#'   Returns NULL if no masks.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- sam_detect(image = "satellite.tif", text = "building")
#' mask_raster <- sam_as_raster(result, combined = TRUE)
#' terra::plot(mask_raster)
#' }
sam_as_raster <- function(x, combined = FALSE) {
  validate_geosam(x)

  if (length(x$masks) == 0) {
    cli::cli_alert_warning("No masks to convert.")
    return(NULL)
  }

  # Read template for extent/crs
  template <- terra::rast(x$image_path)
  tmpl_nrow <- terra::nrow(template)
  tmpl_ncol <- terra::ncol(template)

  # Create rasters from each mask
  rast_list <- lapply(seq_along(x$masks), function(i) {
    mask_array <- x$masks[[i]]

    # Handle dimension mismatch (transpose if needed)
    if (nrow(mask_array) == tmpl_ncol && ncol(mask_array) == tmpl_nrow) {
      mask_array <- t(mask_array)
    }

    mask_rast <- terra::rast(
      nrows = tmpl_nrow,
      ncols = tmpl_ncol,
      extent = terra::ext(template),
      crs = terra::crs(template)
    )
    terra::values(mask_rast) <- c(t(mask_array))
    names(mask_rast) <- paste0("mask_", i)
    mask_rast
  })

  if (combined) {
    # Union all masks into single layer
    result <- Reduce(function(a, b) terra::max(a, b), rast_list)
    names(result) <- "combined_mask"
    result
  } else {
    # Stack as multi-layer raster
    terra::rast(rast_list)
  }
}


#' Get Bounding Box of Detections
#'
#' Returns the bounding box encompassing all detected objects.
#'
#' @param x A geosam object from `sam_detect()` or `sam_explore()`.
#'
#' @return An sf bbox object, or NULL if no detections.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- sam_detect(image = "satellite.tif", text = "building")
#' bbox <- sam_bbox(result)
#' }
sam_bbox <- function(x) {
  validate_geosam(x)

  sf_result <- sam_as_sf(x)
  if (is.null(sf_result)) {
    return(NULL)
  }

  sf::st_bbox(sf_result)
}


#' Get Detection Scores
#'
#' Returns confidence scores for all detections.
#'
#' @param x A geosam object.
#'
#' @return Numeric vector of scores.
#'
#' @export
sam_scores <- function(x) {
  validate_geosam(x)
  x$scores
}


#' Get Number of Detections
#'
#' Returns the number of detected objects.
#'
#' @param x A geosam object.
#'
#' @return Integer count of detections.
#'
#' @export
sam_count <- function(x) {
  validate_geosam(x)
  length(x$masks)
}

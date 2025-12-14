#' Extract sf Polygons from geosam or geosam_image Object
#'
#' Converts detection masks to sf polygon geometries.
#'
#' For `geosam` objects (georeferenced imagery), returns polygons in WGS84
#' coordinates with area in square meters.
#'
#' For `geosam_image` objects (non-georeferenced images), returns polygons
#' in pixel coordinates where x = column (from left) and y = row (from top).
#'
#' @param x A geosam or geosam_image object.
#' @param min_area Minimum object area. For geosam, in square meters.
#'   For geosam_image, in square pixels. Objects smaller are filtered out.
#' @param max_area Maximum object area. For geosam, in square meters.
#'   For geosam_image, in square pixels. Objects larger are filtered out.
#'
#' @return An sf data frame with polygon geometries, scores, and area.
#'   Returns NULL if no polygons remain after filtering.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Georeferenced imagery
#' result <- sam_detect(image = "satellite.tif", text = "building")
#' buildings <- sam_as_sf(result, min_area = 100)
#'
#' # Non-georeferenced image
#' result <- sam_image("photo.jpg", text = "dog")
#' dogs <- sam_as_sf(result)  # coordinates in pixels
#' }
sam_as_sf <- function(x, min_area = NULL, max_area = NULL) {
  # Dispatch based on class
  if (inherits(x, "geosam_image")) {
    return(.sam_as_sf_image(x, min_area, max_area))
  }

  # Check for pre-computed sf (from tiled detection) BEFORE validation

  # Tiled detection doesn't store masks (different sizes per chunk)
  if (!is.null(x$sf_result)) {
    result <- x$sf_result

    # Apply area filters if specified
    if (!is.null(min_area)) {
      result <- result[result$area_m2 >= min_area, ]
    }
    if (!is.null(max_area)) {
      result <- result[result$area_m2 <= max_area, ]
    }

    if (nrow(result) == 0) {
      return(NULL)
    }

    return(result)
  }

  # Normal path - validate geosam object
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


#' Convert geosam_image masks to sf (pixel coordinates)
#' @noRd
.sam_as_sf_image <- function(x, min_area = NULL, max_area = NULL) {
  validate_geosam_image(x)

  if (length(x$masks) == 0) {
    cli::cli_alert_warning("No masks to convert.")
    return(NULL)
  }

  img_width <- x$dimensions[1]
  img_height <- x$dimensions[2]

  min_area_px <- min_area %||% 0
  max_area_px <- max_area %||% Inf

  # Convert each mask to polygon in pixel space
  polygons <- list()

  for (i in seq_along(x$masks)) {
    mask_array <- x$masks[[i]]

    # Ensure correct orientation
    mask_nrow <- nrow(mask_array)
    mask_ncol <- ncol(mask_array)

    if (mask_nrow == img_width && mask_ncol == img_height) {
      mask_array <- t(mask_array)
    }

    # Create a simple raster in pixel coordinates
    # Origin at top-left (0,0), x increases right, y increases down
    mask_rast <- terra::rast(
      nrows = img_height,
      ncols = img_width,
      extent = terra::ext(0, img_width, 0, img_height),
      crs = ""  # No CRS for pixel space
    )

    # Fill with mask values - need to flip Y since terra expects bottom-to-top
    terra::values(mask_rast) <- as.vector(t(mask_array[nrow(mask_array):1, ]))

    # Convert to polygon
    tryCatch({
      polys <- terra::as.polygons(mask_rast, dissolve = TRUE)
      polys <- polys[polys[[1]] == 1, ]  # Keep only mask=1

      if (nrow(polys) > 0) {
        sf_poly <- sf::st_as_sf(polys)

        # Calculate area in pixels
        area_px <- as.numeric(sf::st_area(sf_poly))

        # Filter by area
        if (area_px >= min_area_px && area_px <= max_area_px) {
          sf_poly$score <- x$scores[i]
          sf_poly$area_px <- area_px
          sf_poly$mask_id <- i
          sf_poly <- sf_poly[, c("mask_id", "score", "area_px", "geometry")]
          polygons[[length(polygons) + 1]] <- sf_poly
        }
      }
    }, error = function(e) {
      # Skip problematic masks
    })
  }

  if (length(polygons) == 0) {
    return(NULL)
  }

  result <- do.call(rbind, polygons)

  # The polygons from terra have y=0 at bottom (geographic convention)
  # For pixel coordinates, we want y=0 at top
  # But .convert_pixel_sf_to_geo() will flip when converting to fake geographic coords
  # So we leave these in terra's bottom-up convention and let the display conversion handle it

  result
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
#' @param x A geosam or geosam_image object.
#'
#' @return Numeric vector of scores.
#'
#' @export
sam_scores <- function(x) {
  if (!inherits(x, c("geosam", "geosam_image"))) {
    cli::cli_abort("{.arg x} must be a {.cls geosam} or {.cls geosam_image} object.")
  }
  x$scores
}


#' Get Number of Detections
#'
#' Returns the number of detected objects.
#'
#' @param x A geosam or geosam_image object.
#'
#' @return Integer count of detections.
#'
#' @export
sam_count <- function(x) {
  if (!inherits(x, c("geosam", "geosam_image"))) {
    cli::cli_abort("{.arg x} must be a {.cls geosam} or {.cls geosam_image} object.")
  }

  # For tiled results, count from pre-computed sf
  if (!is.null(x$sf_result)) {
    return(nrow(x$sf_result))
  }

  length(x$masks)
}


#' Extract Masks as Matrices
#'
#' Returns detection masks as a list of R matrices.
#'
#' @param x A geosam or geosam_image object.
#'
#' @return A list of binary matrices (1 = detected, 0 = background).
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- sam_image("photo.jpg", text = "dog")
#' masks <- sam_as_matrix(result)
#' image(masks[[1]])  # Display first mask
#' }
sam_as_matrix <- function(x) {
  if (!inherits(x, c("geosam", "geosam_image"))) {
    cli::cli_abort("{.arg x} must be a {.cls geosam} or {.cls geosam_image} object.")
  }

  if (length(x$masks) == 0) {
    cli::cli_alert_warning("No masks to extract.")
    return(list())
  }

  # Return masks as-is (they're already matrices)
  x$masks
}

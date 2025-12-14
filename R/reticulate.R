#' Initialize Python Bridge
#'
#' Internal function to set up the Python environment and load the geosam module.
#'
#' @return The Python geosam_core module, invisibly.
#' @noRd
.init_python <- function() {
 # Return cached module if available
  if (!is.null(.geosam_env$python_module)) {
    return(invisible(.geosam_env$python_module))
  }

  # Try to activate saved environment
  config <- .load_env_config()
  if (!is.null(config)) {
    .activate_env(config)
  }

  # Check Python availability
  if (!reticulate::py_available(initialize = TRUE)) {
    cli::cli_abort(c(
      "Python is not available.",
      "i" = "Run {.code geosam_install()} to set up the environment."
    ))
  }

  # Get path to bundled Python module
  module_path <- system.file("python", package = "geosam")

  if (module_path == "") {
    cli::cli_abort("Could not find Python module in package.")
  }

  # Add module path to Python path
  reticulate::py_run_string(sprintf(
    "import sys; sys.path.insert(0, '%s')",
    module_path
  ))

  # Import the module
  tryCatch({
    module <- reticulate::import("geosam_core")
    .geosam_env$python_module <- module
    invisible(module)
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to load geosam Python module.",
      "x" = e$message,
      "i" = "Run {.code geosam_install()} to set up dependencies."
    ))
  })
}


#' Get Python Module
#'
#' Internal function to get the initialized Python module.
#'
#' @return The Python geosam_core module.
#' @noRd
.get_module <- function() {
  if (is.null(.geosam_env$python_module)) {
    .init_python()
  }
  .geosam_env$python_module
}


#' Ensure Python Environment
#'
#' Internal function to check Python is ready and transformers is available.
#'
#' @return TRUE if ready, otherwise aborts with error.
#' @noRd
.ensure_python <- function() {
  module <- .get_module()

  # Check transformers availability (SAM3 via HuggingFace)
  env_info <- module$check_environment()

  if (!isTRUE(env_info$transformers_available)) {
    cli::cli_abort(c(
      "transformers is not installed (required for SAM3).",
      "i" = "Run {.code geosam_install()} to install dependencies."
    ))
  }

  TRUE
}


#' Convert sf Object to Coordinate List
#'
#' Internal function to extract coordinates from sf objects for Python.
#'
#' @param x An sf object (points, polygons, or bbox)
#' @param type Type of geometry to extract: "points", "boxes", or "bbox"
#' @return List of coordinates suitable for Python
#' @noRd
.sf_to_coords <- function(x, type = c("points", "boxes", "bbox")) {
  type <- match.arg(type)

  if (inherits(x, "bbox")) {
    # Already a bbox
    return(as.numeric(x[c("xmin", "ymin", "xmax", "ymax")]))
  }

  if (!inherits(x, c("sf", "sfc"))) {
    cli::cli_abort("Input must be an sf object or bbox.")
  }

  # Transform to WGS84 if needed
  if (!is.na(sf::st_crs(x)) && sf::st_crs(x)$epsg != 4326) {
    x <- sf::st_transform(x, 4326)
  }

  geom_type <- unique(sf::st_geometry_type(x))

  if (type == "points") {
    if (!all(geom_type %in% c("POINT", "MULTIPOINT"))) {
      cli::cli_abort("Expected point geometry for point prompts.")
    }
    coords <- sf::st_coordinates(x)
    lapply(seq_len(nrow(coords)), function(i) {
      as.numeric(coords[i, c("X", "Y")])
    })

  } else if (type == "boxes") {
    # Extract bbox of each feature
    lapply(seq_len(nrow(x)), function(i) {
      bb <- sf::st_bbox(x[i, ])
      as.numeric(bb[c("xmin", "ymin", "xmax", "ymax")])
    })

  } else if (type == "bbox") {
    bb <- sf::st_bbox(x)
    as.numeric(bb[c("xmin", "ymin", "xmax", "ymax")])
  }
}


#' Read GeoJSON Result
#'
#' Internal function to read detection results from GeoJSON.
#'
#' @param path Path to GeoJSON file
#' @return sf object or NULL if empty/error
#' @noRd
.read_geojson <- function(path) {
  if (!file.exists(path)) {
    return(NULL)
  }

  tryCatch({
    result <- sf::st_read(path, quiet = TRUE)
    if (nrow(result) == 0) {
      return(NULL)
    }
    result
  }, error = function(e) {
    cli::cli_alert_warning("Failed to read results: {e$message}")
    NULL
  })
}


#' Convert Masks to sf Polygons
#'
#' Internal function to convert binary mask arrays to sf polygons using terra.
#' All geospatial handling is done in R to ensure correct georeferencing.
#'
#' @param masks List of binary mask arrays (from Python)
#' @param scores List of detection scores
#' @param template_raster Path to the GeoTIFF that masks correspond to
#' @param min_area_m2 Minimum area filter in square meters
#' @param max_area_m2 Maximum area filter in square meters
#' @return sf object with polygons, or NULL if no valid polygons
#' @noRd
.masks_to_sf <- function(masks, scores, template_raster, min_area_m2 = 10, max_area_m2 = 100000) {
  # Read template to get extent and CRS
  template <- terra::rast(template_raster)
  tmpl_nrow <- terra::nrow(template)
  tmpl_ncol <- terra::ncol(template)

  all_polys <- list()

  for (i in seq_along(masks)) {
    mask_array <- masks[[i]]
    score <- scores[[i]]

    # reticulate converts numpy arrays to R matrices
    # numpy uses (height, width) = (rows, cols) which matches R convention
    # But we need to verify dimensions match the template
    mask_nrow <- nrow(mask_array)
    mask_ncol <- ncol(mask_array)

    # Only transpose if dimensions are actually swapped (not just equal for square images)
    if (mask_nrow != tmpl_nrow || mask_ncol != tmpl_ncol) {
      if (mask_nrow == tmpl_ncol && mask_ncol == tmpl_nrow) {
        mask_array <- t(mask_array)
      } else {
        cli::cli_alert_warning(
          "Mask {i} dimensions ({mask_nrow}x{mask_ncol}) don't match template ({tmpl_nrow}x{tmpl_ncol}), skipping"
        )
        next
      }
    }

    # Create a new raster with same properties as template
    mask_rast <- terra::rast(
      nrows = tmpl_nrow,
      ncols = tmpl_ncol,
      extent = terra::ext(template),
      crs = terra::crs(template)
    )

    # terra::values() fills cells in row-major order (row 1 left-to-right, then row 2, etc.)
    terra::values(mask_rast) <- as.vector(t(mask_array))

    # Convert to polygons (only where mask == 1)
    polys <- tryCatch({
      terra::as.polygons(mask_rast, dissolve = TRUE, values = TRUE)
    }, error = function(e) NULL)

    if (is.null(polys) || nrow(polys) == 0) next

    # Filter to only mask value 1 (foreground)
    polys <- polys[terra::values(polys)[, 1] == 1, ]

    if (nrow(polys) == 0) next

    # Convert to sf
    polys_sf <- sf::st_as_sf(polys)
    polys_sf$score <- score
    polys_sf$mask_id <- i

    # Remove the raster value column if present
    if ("lyr.1" %in% names(polys_sf)) {
      polys_sf$lyr.1 <- NULL
    }

    all_polys[[length(all_polys) + 1]] <- polys_sf
  }

  if (length(all_polys) == 0) {
    return(NULL)
  }

  # Combine all polygons
  result <- do.call(rbind, all_polys)


  # Calculate area using sf::st_area (works with any CRS, returns units)
  result$area_m2 <- as.numeric(sf::st_area(result))

  # Filter by area
  result <- result[result$area_m2 >= min_area_m2 & result$area_m2 <= max_area_m2, ]

  if (nrow(result) == 0) {
    return(NULL)
  }

  # Transform to WGS84 for output

  result <- sf::st_transform(result, 4326)

  result
}


#' Read Image as Array for Python
#'
#' Internal function to read a GeoTIFF and return as RGB array for Python.
#'
#' @param image_path Path to GeoTIFF
#' @return Numeric array (height x width x 3)
#' @noRd
.read_image_array <- function(image_path) {
  r <- terra::rast(image_path)

  nrow_img <- terra::nrow(r)
  ncol_img <- terra::ncol(r)

  # Get values as matrix for each band using terra::as.matrix
  # This returns a proper row x col matrix with correct orientation
  if (terra::nlyr(r) >= 3) {
    red_mat <- terra::as.matrix(r[[1]], wide = TRUE)
    green_mat <- terra::as.matrix(r[[2]], wide = TRUE)
    blue_mat <- terra::as.matrix(r[[3]], wide = TRUE)
  } else {
    gray_mat <- terra::as.matrix(r[[1]], wide = TRUE)
    red_mat <- green_mat <- blue_mat <- gray_mat
  }

  # Stack into 3D array (height, width, channels) for Python/numpy
  array(c(red_mat, green_mat, blue_mat), dim = c(nrow_img, ncol_img, 3))
}


#' Convert Geographic Coordinates to Pixel Coordinates
#'
#' Internal function to convert geographic coordinates to pixel coordinates.
#'
#' @param coords List of coordinate pairs (x, y in geographic CRS)
#' @param image_path Path to the reference GeoTIFF
#' @param input_crs CRS of input coordinates (default WGS84)
#' @return List of pixel coordinate pairs (col, row)
#' @noRd
.geo_to_pixel <- function(coords, image_path, input_crs = 4326) {
  r <- terra::rast(image_path)
  img_crs <- terra::crs(r)
  ext <- terra::ext(r)

  nrow_img <- terra::nrow(r)
  ncol_img <- terra::ncol(r)

  # Resolution
  res_x <- (ext$xmax - ext$xmin) / ncol_img
  res_y <- (ext$ymax - ext$ymin) / nrow_img

  pixel_coords <- lapply(coords, function(pt) {
    # Create point and transform to image CRS
    pt_sf <- sf::st_sfc(sf::st_point(c(pt[1], pt[2])), crs = input_crs)
    pt_transformed <- sf::st_transform(pt_sf, sf::st_crs(img_crs))
    xy <- sf::st_coordinates(pt_transformed)

    # Convert to pixel coordinates
    col <- floor((xy[1, "X"] - ext$xmin) / res_x)
    row <- floor((ext$ymax - xy[1, "Y"]) / res_y)

    # Clamp to valid range
    col <- max(0, min(col, ncol_img - 1))
    row <- max(0, min(row, nrow_img - 1))

    c(as.integer(col), as.integer(row))
  })

  pixel_coords
}


#' Convert Geographic Boxes to Pixel Boxes
#'
#' Internal function to convert geographic bounding boxes to pixel coordinates.
#'
#' @param boxes List of bounding boxes (xmin, ymin, xmax, ymax in geographic CRS)
#' @param image_path Path to the reference GeoTIFF
#' @param input_crs CRS of input coordinates (default WGS84)
#' @return List of pixel boxes (xmin, ymin, xmax, ymax in pixels)
#' @noRd
.geo_boxes_to_pixel <- function(boxes, image_path, input_crs = 4326) {
  r <- terra::rast(image_path)
  img_crs <- terra::crs(r)
  ext <- terra::ext(r)

  nrow_img <- terra::nrow(r)
  ncol_img <- terra::ncol(r)

  # Resolution
  res_x <- (ext$xmax - ext$xmin) / ncol_img
  res_y <- (ext$ymax - ext$ymin) / nrow_img

  pixel_boxes <- lapply(boxes, function(box) {
    xmin_geo <- box[1]
    ymin_geo <- box[2]
    xmax_geo <- box[3]
    ymax_geo <- box[4]

    # Transform corners to image CRS
    corners <- sf::st_sfc(
      sf::st_point(c(xmin_geo, ymin_geo)),
      sf::st_point(c(xmax_geo, ymax_geo)),
      crs = input_crs
    )
    corners_transformed <- sf::st_transform(corners, sf::st_crs(img_crs))
    coords <- sf::st_coordinates(corners_transformed)

    # Convert to pixel coordinates
    col_min <- floor((coords[1, "X"] - ext$xmin) / res_x)
    col_max <- floor((coords[2, "X"] - ext$xmin) / res_x)
    row_max <- floor((ext$ymax - coords[1, "Y"]) / res_y)  # ymin -> row_max
    row_min <- floor((ext$ymax - coords[2, "Y"]) / res_y)  # ymax -> row_min

    # Clamp to valid range
    col_min <- max(0, min(col_min, ncol_img - 1))
    col_max <- max(0, min(col_max, ncol_img - 1))
    row_min <- max(0, min(row_min, nrow_img - 1))
    row_max <- max(0, min(row_max, nrow_img - 1))

    # Return as xmin, ymin, xmax, ymax in pixel space
    c(as.integer(min(col_min, col_max)),
      as.integer(min(row_min, row_max)),
      as.integer(max(col_min, col_max)),
      as.integer(max(row_min, row_max)))
  })

  pixel_boxes
}

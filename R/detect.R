#' Detect Objects in Geospatial Imagery Using SAM3
#'
#' Main function for object detection using Meta's SAM3 model. Supports text
#' prompts, point prompts, box prompts, and exemplar-based detection.
#'
#' For large areas or images, detection is automatically chunked to maintain
#' accuracy and avoid memory issues. This means you can pass a large bounding
#' box (e.g., a census tract) or a large GeoTIFF file (e.g., an orthoimage)
#' and get reliable detection without running out of memory.
#'
#' @param image Path to a GeoTIFF image, or NULL to download imagery for bbox.
#'   Large images (>2000 pixels on longest dimension) are automatically
#'   processed in chunks when using text prompts.
#' @param bbox Bounding box for the area of interest. Can be a numeric vector
#'   `c(xmin, ymin, xmax, ymax)` in WGS84, or an sf/sfc object.
#' @param text Text prompt describing objects to detect (e.g., "swimming pool",
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
#' @param chunked Control chunking for large areas/images:
#'   - `NULL` (default): Auto-chunk when image >2000px or bbox requires multiple tiles
#'   - `TRUE`: Force chunked detection
#'   - `FALSE`: Disable chunking (may cause memory issues for large images)
#'
#' @param min_area Minimum object area in square meters. Objects smaller than
#'   this are filtered out. For chunked detection, filtering happens during
#'   processing (better performance). Default NULL (no minimum).
#' @param max_area Maximum object area in square meters. Objects larger than
#'   this are filtered out. Default NULL (no maximum).
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
#'   text = "swimming pool"
#' )
#' pads <- result |> sam_filter(min_area = 500) |> sam_as_sf()
#'
#' # Large area - automatically chunked
#' result <- sam_detect(
#'   bbox = c(-118.45, 34.08, -118.40, 34.12),  # ~5km area
#'   text = "swimming pool",
#'   zoom = 18
#' )
#'
#' # Large user-provided image - automatically chunked
#' result <- sam_detect(
#'   image = "large_orthoimage.tif",
#'   text = "trees",
#'   threshold = 0.3
#' )
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
    threshold = 0.5,
    chunked = NULL,
    min_area = NULL,
    max_area = NULL
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

  # Check if chunked detection should be used
  # Only applies when: downloading imagery + text prompt + chunked not disabled
  use_chunked <- FALSE
  if (is.null(image) && !is.null(text) && !is.null(bbox)) {
    if (isTRUE(chunked)) {
      use_chunked <- TRUE
    } else if (is.null(chunked)) {
      # Auto: check if bbox is large enough to need chunking
      tile_bboxes <- .split_bbox_for_detection(bbox, zoom, source)
      use_chunked <- length(tile_bboxes) > 1
    }
    # chunked = FALSE disables chunking
  }

  # Route to chunked detection if needed
  if (use_chunked) {
    return(.sam_detect_chunked(
      bbox = bbox,
      text = text,
      source = source,
      zoom = zoom,
      threshold = threshold,
      min_area = min_area,
      max_area = max_area
    ))
  }

  # Download imagery if needed (single tile path)
  # Track source for later use in sam_view()
  used_source <- NULL
  if (is.null(image)) {
    image <- get_imagery(
      bbox = bbox,
      source = source,
      zoom = zoom
    )
    used_source <- source
  }

  # Verify image exists
  if (!file.exists(image)) {
    cli::cli_abort("Image file not found: {image}")
  }

  # Check if chunked detection should be used for user-provided images
  # Only applies to text prompts currently
  if (!is.null(text) && !isFALSE(chunked)) {
    img_rast <- terra::rast(image)
    img_width <- terra::ncol(img_rast)
    img_height <- terra::nrow(img_rast)
    max_dim <- max(img_width, img_height)

    # Use chunked detection for large images
    if (isTRUE(chunked) || (is.null(chunked) && max_dim > 2000)) {
      return(.sam_detect_chunked_file(
        image_path = image,
        text = text,
        threshold = threshold,
        min_area = min_area,
        max_area = max_area
      ))
    }
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
    # Exemplar-based detection - uses main SAM3 model to find similar objects
    exemplar_box <- .sf_to_coords(exemplar, "bbox")
    pixel_box <- .geo_boxes_to_pixel(list(exemplar_box), image_path)[[1]]

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

  # Return geosam object (masks stay as arrays for refinement)
  new_geosam(
    image_path = image_path,
    masks = result$masks,
    scores = as.numeric(result$scores),
    prompt = prompt_info,
    extent = img_extent,
    crs = img_crs,
    source = used_source,
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


#' Chunked detection for large areas
#'
#' Internal function that downloads imagery once, then runs detection
#' on image chunks to maintain accuracy for large areas.
#'
#' @param bbox Numeric bbox vector
#' @param text Text prompt
#' @param source Imagery source
#' @param zoom Zoom level
#' @param threshold Detection threshold
#' @param min_area Minimum area in square meters (filters during detection)
#' @param max_area Maximum area in square meters (filters during detection)
#' @return A geosam object with merged results
#' @noRd
.sam_detect_chunked <- function(bbox, text, source, zoom, threshold,
                                 min_area = NULL, max_area = NULL) {
 .ensure_python()

  # Step 1: Download ALL imagery at once
  cli::cli_alert_info("Downloading imagery...")
  image_path <- get_imagery(
    bbox = bbox,
    source = source,
    zoom = zoom
  )

  # Get image dimensions
  img_rast <- terra::rast(image_path)
  img_width <- terra::ncol(img_rast)
  img_height <- terra::nrow(img_rast)

  # Step 2: Calculate detection grid based on image size
  # Target ~1000-1200 pixels per detection tile
  target_size <- 1000
  tiles_x <- ceiling(img_width / target_size)
  tiles_y <- ceiling(img_height / target_size)
  n_tiles <- tiles_x * tiles_y

  # If only 1 tile needed, just run normal detection
 if (n_tiles == 1) {
    cli::cli_alert_info("Running SAM3 detection...")
    module <- .get_module()
    img_array <- .read_image_array(image_path)

    result <- module$detect_text(
      img_array = img_array,
      text_prompt = text,
      threshold = threshold
    )

    if (result$count == 0) {
      cli::cli_alert_info("No objects detected.")
      unlink(image_path)
      return(NULL)
    }

    cli::cli_alert_success("Detected {result$count} object{?s}.")

    return(new_geosam(
      image_path = image_path,
      masks = result$masks,
      scores = as.numeric(result$scores),
      prompt = list(type = "text", value = text),
      extent = as.vector(terra::ext(img_rast)),
      crs = terra::crs(img_rast, proj = TRUE),
      source = source,
      history = list()
    ))
  }

  cli::cli_alert_info(
    "Running detection in {n_tiles} chunks ({tiles_x}x{tiles_y})..."
  )

  # Pre-load model
  if (!sam_is_loaded()) {
    sam_load()
  }

  module <- .get_module()

  # Calculate chunk boundaries in pixels
  chunk_width <- ceiling(img_width / tiles_x)
  chunk_height <- ceiling(img_height / tiles_y)

  # Process each chunk with progress bar
  all_polygons <- list()
  tile_num <- 0

  cli::cli_progress_bar(
    total = n_tiles,
    format = "Detecting objects [{cli::pb_current}/{cli::pb_total}] {cli::pb_percent}"
  )

  for (j in seq_len(tiles_y)) {
    for (i in seq_len(tiles_x)) {
      tile_num <- tile_num + 1
      cli::cli_progress_update()

      # Calculate pixel bounds for this chunk (1-indexed)
      x_start <- (i - 1) * chunk_width + 1
      x_end <- min(i * chunk_width, img_width)
      y_start <- (j - 1) * chunk_height + 1
      y_end <- min(j * chunk_height, img_height)

      tryCatch({
        # Convert pixel bounds to geographic extent
        full_ext <- terra::ext(img_rast)
        xres <- terra::xres(img_rast)
        yres <- terra::yres(img_rast)

        chunk_xmin <- full_ext[1] + (x_start - 1) * xres
        chunk_xmax <- full_ext[1] + x_end * xres
        chunk_ymax <- full_ext[4] - (y_start - 1) * yres
        chunk_ymin <- full_ext[4] - y_end * yres

        chunk_ext <- terra::ext(chunk_xmin, chunk_xmax, chunk_ymin, chunk_ymax)

        # Extract chunk from full image
        chunk_rast <- terra::crop(img_rast, chunk_ext)

        # Write chunk to temp file for coordinate reference
        chunk_path <- tempfile(fileext = ".tif")
        terra::writeRaster(chunk_rast, chunk_path, datatype = "INT1U")

        # Read as array for detection
        img_array <- .read_image_array(chunk_path)

        # Run detection on chunk
        result <- module$detect_text(
          img_array = img_array,
          text_prompt = text,
          threshold = threshold
        )

        if (result$count > 0) {
          # Convert masks to sf with correct georeferencing
          # Apply size filtering during conversion (early filtering)
          chunk_sf <- .masks_to_sf(
            masks = result$masks,
            scores = as.numeric(result$scores),
            template_raster = chunk_path,
            min_area_m2 = min_area %||% 0,
            max_area_m2 = max_area %||% Inf
          )

          if (!is.null(chunk_sf) && nrow(chunk_sf) > 0) {
            # Ensure consistent columns for combining
            chunk_sf <- chunk_sf[, c("score", "area_m2", "geometry")]
            all_polygons[[length(all_polygons) + 1]] <- chunk_sf
          }
        }

        # Clean up chunk file
        unlink(chunk_path)

        # Clear GPU/MPS cache after each tile to prevent memory buildup
        module$clear_cache()

      }, error = function(e) {
        cli::cli_alert_warning("Chunk {tile_num} failed: {e$message}")
      })
    }
  }

  cli::cli_progress_done()

  # Merge results
  if (length(all_polygons) == 0) {
    cli::cli_alert_info("No objects detected.")
    unlink(image_path)
    return(NULL)
  }

  combined_sf <- do.call(rbind, all_polygons)
  combined_sf <- .deduplicate_tile_boundaries(combined_sf)

  n_total <- nrow(combined_sf)
  cli::cli_alert_success("Detected {n_total} object{?s} across {n_tiles} chunks.")

  # Create geosam object - keep the full image for plotting
  result <- new_geosam(
    image_path = image_path,
    masks = list(),  # Masks not stored for chunked (different sizes per chunk)
    scores = combined_sf$score,
    prompt = list(type = "text", value = text),
    extent = as.vector(terra::ext(img_rast)),
    crs = terra::crs(img_rast, proj = TRUE),
    source = source,
    history = list(list(
      action = "chunked_detection",
      n_tiles = n_tiles,
      grid = c(tiles_x, tiles_y)
    ))
  )

  result$sf_result <- combined_sf
  result
}


#' Chunked detection for user-provided large images
#'
#' Internal function that splits a large user-provided image into chunks
#' and runs detection on each chunk to avoid memory issues. Uses overlap
#' to reduce boundary artifacts.
#'
#' @param image_path Path to the GeoTIFF image
#' @param text Text prompt
#' @param threshold Detection threshold
#' @param target_size Target size for each chunk in pixels (before overlap)
#' @param overlap Overlap in pixels on each side to reduce boundary artifacts
#' @param min_area Minimum area in square meters (filters during detection)
#' @param max_area Maximum area in square meters (filters during detection)
#' @return A geosam object with merged results
#' @noRd
.sam_detect_chunked_file <- function(image_path, text, threshold,
                                      target_size = 1000, overlap = 128,
                                      min_area = NULL, max_area = NULL) {
  .ensure_python()

  image_path <- normalizePath(image_path)

  # Get image dimensions without loading full raster into memory
  img_info <- terra::rast(image_path)
  img_width <- terra::ncol(img_info)
  img_height <- terra::nrow(img_info)
  full_ext <- terra::ext(img_info)
  img_crs <- terra::crs(img_info)
  xres <- terra::xres(img_info)
  yres <- terra::yres(img_info)

  # Calculate detection grid based on image size
  tiles_x <- ceiling(img_width / target_size)
  tiles_y <- ceiling(img_height / target_size)
  n_tiles <- tiles_x * tiles_y

  # If only 1 tile needed, just run normal detection
  if (n_tiles == 1) {
    cli::cli_alert_info("Running SAM3 detection...")
    module <- .get_module()
    img_array <- .read_image_array(image_path)

    result <- module$detect_text(
      img_array = img_array,
      text_prompt = text,
      threshold = threshold
    )

    if (result$count == 0) {
      cli::cli_alert_info("No objects detected.")
      return(NULL)
    }

    cli::cli_alert_success("Detected {result$count} object{?s}.")

    return(new_geosam(
      image_path = image_path,
      masks = result$masks,
      scores = as.numeric(result$scores),
      prompt = list(type = "text", value = text),
      extent = as.vector(full_ext),
      crs = terra::crs(img_info, proj = TRUE),
      source = NULL,
      history = list()
    ))
  }

  cli::cli_alert_info(
    "Large image ({img_width}x{img_height}). Processing in {n_tiles} chunks..."
  )

  # Pre-load model
  if (!sam_is_loaded()) {
    sam_load()
  }

  module <- .get_module()

  # Calculate chunk boundaries in pixels (core area without overlap)
  chunk_width <- ceiling(img_width / tiles_x)
  chunk_height <- ceiling(img_height / tiles_y)

  # Process each chunk with progress bar
  all_polygons <- list()
  tile_num <- 0

  cli::cli_progress_bar(
    total = n_tiles,
    format = "Detecting objects [{cli::pb_current}/{cli::pb_total}] {cli::pb_percent}"
  )

  for (j in seq_len(tiles_y)) {
    for (i in seq_len(tiles_x)) {
      tile_num <- tile_num + 1
      cli::cli_progress_update()

      # Calculate CORE pixel bounds for this chunk (1-indexed)
      # This is the area we'll keep polygons from
      core_x_start <- (i - 1) * chunk_width + 1
      core_x_end <- min(i * chunk_width, img_width)
      core_y_start <- (j - 1) * chunk_height + 1
      core_y_end <- min(j * chunk_height, img_height)

      # Calculate EXTENDED pixel bounds with overlap (for detection)
      ext_x_start <- max(1, core_x_start - overlap)
      ext_x_end <- min(img_width, core_x_end + overlap)
      ext_y_start <- max(1, core_y_start - overlap)
      ext_y_end <- min(img_height, core_y_end + overlap)

      tryCatch({
        # Convert EXTENDED pixel bounds to geographic extent for detection
        chunk_xmin <- as.numeric(full_ext[1]) + (ext_x_start - 1) * xres
        chunk_xmax <- as.numeric(full_ext[1]) + ext_x_end * xres
        chunk_ymax <- as.numeric(full_ext[4]) - (ext_y_start - 1) * yres
        chunk_ymin <- as.numeric(full_ext[4]) - ext_y_end * yres

        chunk_ext <- terra::ext(chunk_xmin, chunk_xmax, chunk_ymin, chunk_ymax)

        # Convert CORE pixel bounds to geographic extent for filtering
        core_xmin <- as.numeric(full_ext[1]) + (core_x_start - 1) * xres
        core_xmax <- as.numeric(full_ext[1]) + core_x_end * xres
        core_ymax <- as.numeric(full_ext[4]) - (core_y_start - 1) * yres
        core_ymin <- as.numeric(full_ext[4]) - core_y_end * yres

        # Crop to chunk extent
        # terra::crop() is memory-efficient for GeoTIFFs (reads only needed data)
        chunk_rast <- terra::crop(img_info, chunk_ext)

        # Write chunk to temp file for coordinate reference
        chunk_path <- tempfile(fileext = ".tif")
        terra::writeRaster(chunk_rast, chunk_path, datatype = "INT1U")

        # Read as array for detection
        img_array <- .read_image_array(chunk_path)

        # Run detection on chunk (includes overlap area)
        result <- module$detect_text(
          img_array = img_array,
          text_prompt = text,
          threshold = threshold
        )

        if (result$count > 0) {
          # Convert masks to sf with correct georeferencing
          # Apply size filtering during conversion (early filtering)
          chunk_sf <- .masks_to_sf(
            masks = result$masks,
            scores = as.numeric(result$scores),
            template_raster = chunk_path,
            min_area_m2 = min_area %||% 0,
            max_area_m2 = max_area %||% Inf
          )

          if (!is.null(chunk_sf) && nrow(chunk_sf) > 0) {
            # Filter to keep only polygons whose centroid is in the CORE area
            # This ensures each object is only counted once
            # Note: chunk_sf is already in WGS84 (transformed by .masks_to_sf)
            # so we need to transform core bounds to WGS84 as well
            core_coords <- c(xmin = core_xmin, ymin = core_ymin,
                             xmax = core_xmax, ymax = core_ymax)
            core_poly <- sf::st_as_sfc(sf::st_bbox(core_coords))
            sf::st_crs(core_poly) <- sf::st_crs(img_crs)
            core_poly_wgs84 <- sf::st_transform(core_poly, 4326)
            core_bbox_wgs84 <- sf::st_bbox(core_poly_wgs84)

            centroids <- suppressWarnings(sf::st_centroid(chunk_sf))
            centroid_coords <- sf::st_coordinates(centroids)

            in_core <- centroid_coords[, "X"] >= core_bbox_wgs84["xmin"] &
                       centroid_coords[, "X"] <= core_bbox_wgs84["xmax"] &
                       centroid_coords[, "Y"] >= core_bbox_wgs84["ymin"] &
                       centroid_coords[, "Y"] <= core_bbox_wgs84["ymax"]

            chunk_sf <- chunk_sf[in_core, ]

            if (nrow(chunk_sf) > 0) {
              # Ensure consistent columns for combining
              chunk_sf <- chunk_sf[, c("score", "area_m2", "geometry")]
              all_polygons[[length(all_polygons) + 1]] <- chunk_sf
            }
          }
        }

        # Clean up chunk file
        unlink(chunk_path)

        # Clear GPU/MPS cache after each tile to prevent memory buildup
        module$clear_cache()

      }, error = function(e) {
        cli::cli_alert_warning("Chunk {tile_num} failed: {e$message}")
      })
    }
  }

  cli::cli_progress_done()

  # Merge results
  if (length(all_polygons) == 0) {
    cli::cli_alert_info("No objects detected.")
    return(NULL)
  }

  combined_sf <- do.call(rbind, all_polygons)

  # Merge polygons that touch at tile boundaries
  n_before <- nrow(combined_sf)
  combined_sf <- .deduplicate_tile_boundaries(combined_sf)
  n_after <- nrow(combined_sf)

  if (n_before > n_after) {
    cli::cli_alert_info("Merged {n_before - n_after} boundary polygons.")
  }

  cli::cli_alert_success("Detected {n_after} object{?s} across {n_tiles} chunks.")

  # Create geosam object
  result <- new_geosam(
    image_path = image_path,
    masks = list(),  # Masks not stored for chunked (different sizes per chunk)
    scores = combined_sf$score,
    prompt = list(type = "text", value = text),
    extent = as.vector(full_ext),
    crs = terra::crs(img_info, proj = TRUE),
    source = NULL,
    history = list(list(
      action = "chunked_detection",
      n_tiles = n_tiles,
      grid = c(tiles_x, tiles_y),
      overlap = overlap
    ))
  )

  result$sf_result <- combined_sf
  result
}


#' Deduplicate polygons at tile boundaries
#'
#' Identifies and merges polygons that touch or overlap at tile boundaries.
#'
#' @param sf_obj An sf object with polygons
#' @return An sf object with deduplicated polygons
#' @noRd
.deduplicate_tile_boundaries <- function(sf_obj) {
  if (nrow(sf_obj) <= 1) {
    return(sf_obj)
  }

  # Transform to a projected CRS for accurate distance calculations
  sf_proj <- sf::st_transform(sf_obj, 3857)

  # Find polygons that touch or nearly touch (within 1m buffer)
  buffered <- sf::st_buffer(sf_proj, 1)

  # Find which polygons intersect after buffering
  intersects_mat <- sf::st_intersects(buffered, sparse = FALSE)

  # Build groups of connected polygons
  n <- nrow(sf_obj)
  group <- rep(NA_integer_, n)
  current_group <- 0L

  for (i in seq_len(n)) {
    if (is.na(group[i])) {
      current_group <- current_group + 1L
      # BFS to find all connected polygons
      to_visit <- i
      while (length(to_visit) > 0) {
        current <- to_visit[1]
        to_visit <- to_visit[-1]

        if (is.na(group[current])) {
          group[current] <- current_group
          # Find neighbors
          neighbors <- which(intersects_mat[current, ] & is.na(group))
          to_visit <- c(to_visit, neighbors)
        }
      }
    }
  }

  # If no merging needed (all singletons), return as-is
  if (max(group) == n) {
    return(sf_obj)
  }

  # Merge polygons in each group
  result_list <- list()

  for (g in seq_len(max(group))) {
    members <- which(group == g)

    if (length(members) == 1) {
      # Single polygon, keep as-is
      result_list[[length(result_list) + 1]] <- sf_obj[members, ]
    } else {
      # Multiple polygons - union them
      group_sf <- sf_proj[members, ]
      merged_geom <- sf::st_union(sf::st_geometry(group_sf))

      # Take the max score from the group
      max_score <- max(sf_obj$score[members])

      # Calculate new area
      new_area <- as.numeric(sf::st_area(merged_geom))

      # Create new sf row
      merged_sf <- sf::st_sf(
        score = max_score,
        area_m2 = new_area,
        geometry = sf::st_transform(merged_geom, 4326)
      )

      result_list[[length(result_list) + 1]] <- merged_sf
    }
  }

  # Combine results
  result <- do.call(rbind, result_list)

  # Re-order by score descending
  result <- result[order(result$score, decreasing = TRUE), ]
  rownames(result) <- NULL

  result
}

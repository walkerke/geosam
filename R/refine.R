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
        source = x$source,
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
    source = x$source,
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
    source = x$source,
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
#' @noRd
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


  # Convert points to sf if needed (check sf first since it inherits from data.frame)
  if (!inherits(points, c("sf", "sfc"))) {
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


  # Run point detection in refinement mode (all points = one object)
  module <- .get_module()
  result <- module$detect_points(
    img_array = img_array,
    pixel_points = pixel_points,
    labels = as.integer(labels),
    threshold = 0.5,
    multi_object = FALSE
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
    source = x$source,
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
#' result <- sam_detect(image = "satellite.tif", text = "swimming pool")
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
  pixel_box <- .geo_boxes_to_pixel(list(as.numeric(exemplar_box)), x$image_path)[[1]]

  # Read image and run detection using exemplar mode (finds similar objects)
  img_array <- .read_image_array(x$image_path)
  module <- .get_module()

  result <- module$detect_exemplar(
    img_array = img_array,
    pixel_box = pixel_box,
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
    source = x$source,
    history = c(x$history, list(list(action = "find_similar")))
  )
}


#' Merge Polygons Split at Tile Boundaries
#'
#' When detecting objects over large areas, geosam uses tiled processing which
#' can split objects that span tile boundaries into multiple polygons. This
#' function merges polygons that are close together, healing those splits.
#'
#' @param x A geosam object or sf object with detection results.
#' @param buffer Distance in meters to buffer polygons before checking for
#'   overlap. Larger values merge polygons that are further apart. Default is 2.
#' @param by_prompt If TRUE and a `prompt` column exists, only merge polygons
#'   with the same prompt value. Default is TRUE.
#'
#' @return An sf object with merged polygons. Scores are aggregated by taking
#'   the maximum score from merged polygons.
#'
#' @details
#' This function is useful when you notice objects being split at regular
#' intervals (tile boundaries). The default buffer of 2 meters catches most
#' boundary splits without merging truly separate objects.
#'
#' For aggressive merging of nearby objects (not just boundary splits), use
#' a larger buffer value, but be aware this may merge objects that should
#' remain separate.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Detect buildings over a large area (uses tiling internally)
#' buildings <- sam_detect(
#'   bbox = c(-118.5, 34.0, -118.4, 34.1),
#'   text = "building",
#'   zoom = 18
#' )
#'
#' # Merge any buildings split at tile boundaries
#' merged <- sam_merge_edges(buildings)
#'
#' # More aggressive merging (5m buffer)
#' merged <- sam_merge_edges(buildings, buffer = 5)
#' }
sam_merge_edges <- function(x, buffer = 2, by_prompt = TRUE) {
  # Handle both geosam objects and sf objects

  if (inherits(x, "geosam")) {
    if (!is.null(x$sf_result)) {
      sf_obj <- x$sf_result
    } else {
      sf_obj <- sam_as_sf(x)
    }
  } else if (inherits(x, "sf")) {
    sf_obj <- x
  } else {
    cli::cli_abort("x must be a geosam object or sf object.")
  }

  if (is.null(sf_obj) || nrow(sf_obj) <= 1) {
    return(sf_obj)
  }

  # Check for prompt column
  has_prompt <- "prompt" %in% names(sf_obj) && by_prompt

  if (has_prompt) {
    # Process each prompt group separately
    prompts <- unique(sf_obj$prompt)
    result_list <- lapply(prompts, function(p) {
      subset_sf <- sf_obj[sf_obj$prompt == p, ]
      merged <- .merge_nearby_polygons(subset_sf, buffer)
      if (nrow(merged) > 0) {
        merged$prompt <- p
        # Preserve prompt_color if it exists
        if ("prompt_color" %in% names(subset_sf)) {
          merged$prompt_color <- subset_sf$prompt_color[1]
        }
      }
      merged
    })
    result <- do.call(rbind, result_list)
  } else {
    result <- .merge_nearby_polygons(sf_obj, buffer)
  }

  # Re-order by score
  if ("score" %in% names(result)) {
    result <- result[order(result$score, decreasing = TRUE), ]
  }
  rownames(result) <- NULL

  cli::cli_alert_success(
    "Merged {nrow(sf_obj)} polygons into {nrow(result)} ({nrow(sf_obj) - nrow(result)} merged)"
  )

  result
}


#' Merge nearby polygons
#'
#' Internal function to merge polygons within buffer distance.
#'
#' @param sf_obj An sf object
#' @param buffer Buffer distance in meters
#' @return An sf object with merged polygons
#' @noRd
.merge_nearby_polygons <- function(sf_obj, buffer) {
  if (nrow(sf_obj) <= 1) {
    return(sf_obj)
  }

  # Transform to projected CRS for accurate distance calculations
  sf_proj <- sf::st_transform(sf_obj, 3857)

  # Buffer polygons to find nearby ones

  buffered <- sf::st_buffer(sf_proj, buffer)

  # Find which polygons intersect after buffering

  intersects_mat <- sf::st_intersects(buffered, sparse = FALSE)

  # Build groups of connected polygons using BFS

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

  # If no merging needed, return as-is
  if (max(group) == n) {
    return(sf_obj)
  }

  # Merge polygons in each group
  result_list <- list()
  has_area <- "area_m2" %in% names(sf_obj)

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
      max_score <- if ("score" %in% names(sf_obj)) {
        max(sf_obj$score[members])
      } else {
        NA_real_
      }

      # Calculate new area
      new_area <- as.numeric(sf::st_area(merged_geom))

      # Create new sf row
      new_row <- sf::st_sf(
        geometry = sf::st_transform(merged_geom, sf::st_crs(sf_obj))
      )

      if ("score" %in% names(sf_obj)) {
        new_row$score <- max_score
      }
      if (has_area) {
        new_row$area_m2 <- new_area
      }

      result_list[[length(result_list) + 1]] <- new_row
    }
  }

  # Combine results
  do.call(rbind, result_list)
}

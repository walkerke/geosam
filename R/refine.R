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

  if (!is.null(x$sf_result)) {
    sf_result <- x$sf_result
    keep <- rep(TRUE, nrow(sf_result))

    if (!is.null(min_area) && "area_m2" %in% names(sf_result)) {
      keep <- keep & sf_result$area_m2 >= min_area
    }
    if (!is.null(max_area) && "area_m2" %in% names(sf_result)) {
      keep <- keep & sf_result$area_m2 <= max_area
    }
    if (!is.null(min_score) && "score" %in% names(sf_result)) {
      keep <- keep & sf_result$score >= min_score
    }

    result <- x
    result$sf_result <- sf_result[keep, ]
    result$scores <- result$sf_result$score %||% numeric()
    result$history <- c(x$history, list(list(
      action = "filter",
      min_area = min_area,
      max_area = max_area,
      min_score = min_score,
      kept = nrow(result$sf_result)
    )))
    return(result)
  }

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
        crs = "+proj=longlat +datum=WGS84 +no_defs"
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
#' @param seam_aware If TRUE (default) and `x` is a geosam object from
#'   chunked detection, only merge polygons that straddle a chunk seam.
#'   This prevents welding together distinct objects that happen to sit
#'   close together (e.g. truck trailers parked side by side) while still
#'   healing objects genuinely split at tile boundaries. Set to FALSE to
#'   merge purely by proximity (legacy behavior). Ignored when seam
#'   geometry is unavailable (plain sf input or unchunked detection).
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
#' a larger buffer value with `seam_aware = FALSE`, but be aware this may
#' merge objects that should remain separate.
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
sam_merge_edges <- function(x, buffer = 2, by_prompt = TRUE, seam_aware = TRUE) {
  # Handle both geosam objects and sf objects

  seams <- NULL

  if (inherits(x, "geosam")) {
    if (!is.null(x$sf_result)) {
      sf_obj <- x$sf_result
    } else {
      sf_obj <- sam_as_sf(x)
    }
    if (isTRUE(seam_aware)) {
      seams <- x$seams
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
      merged <- .merge_nearby_polygons(subset_sf, buffer, seams = seams)
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
    result <- .merge_nearby_polygons(sf_obj, buffer, seams = seams)
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
#' @param seams Optional sfc of seam LINESTRINGs (WGS84). When provided,
#'   only polygons straddling a seam are merged.
#' @return An sf object with merged polygons
#' @noRd
.merge_nearby_polygons <- function(sf_obj, buffer, seams = NULL) {
  if (nrow(sf_obj) <= 1) {
    return(sf_obj)
  }

  # Transform to projected CRS for accurate distance calculations
  merc <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +wktext +no_defs"
  sf_proj <- sf::st_transform(sf_obj, merc)

  # Buffer polygons to find nearby ones

  buffered <- sf::st_buffer(sf_proj, buffer)

  # Find which polygons intersect after buffering

  intersects_mat <- sf::st_intersects(buffered, sparse = FALSE)

  # Restrict merging to seam-straddling pairs when seam geometry is known
  if (!is.null(seams) && length(seams) > 0) {
    seams_proj <- sf::st_transform(seams, merc)
    intersects_mat <- .seam_filter_adjacency(
      intersects_mat, sf_proj, buffered, seams_proj
    )
  }

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


#' Suppress Overlapping Duplicate Detections
#'
#' SAM3 can return multiple overlapping masks for the same object: nested
#' partial masks and offset duplicates. This function performs score-ranked
#' non-maximum suppression: detections are visited in descending score
#' order, and a detection is dropped when it overlaps an already-kept
#' detection beyond the IoU threshold, or when most of its own area falls
#' inside one (the containment test catches nested partial masks that IoU
#' alone misses).
#'
#' Chunked [sam_detect()] results have this suppression applied
#' automatically; use this function to clean up other results (e.g.
#' unchunked detections or externally loaded sf data), to re-run with
#' different thresholds, or to trim residual partial overlaps with
#' `trim = TRUE`.
#'
#' @param x A geosam object or sf object with detection results.
#' @param iou Intersection-over-union threshold (0-1) above which the
#'   lower-scored detection is dropped. Default 0.5.
#' @param containment Fraction (0-1) of a detection's own area that must be
#'   covered by a higher-scored detection for it to be dropped. Default 0.8.
#' @param trim If TRUE, residual partial overlaps among the surviving
#'   detections are removed by subtracting each higher-scored geometry from
#'   lower-scored ones, so no two output polygons overlap. Pieces smaller
#'   than `min_sliver` square meters after trimming are dropped. Default
#'   FALSE.
#' @param min_sliver Minimum area in square meters for a trimmed geometry
#'   to be kept. Only used when `trim = TRUE`. Default 5.
#'
#' @return An sf object with duplicate detections removed.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- sam_detect(
#'   bbox = c(-97.33, 32.98, -97.32, 32.99),
#'   text = "white trailer",
#'   zoom = 18
#' )
#'
#' clean <- sam_dedupe(result)
#'
#' # Strictly non-overlapping output polygons
#' clean <- sam_dedupe(result, trim = TRUE)
#' }
sam_dedupe <- function(x, iou = 0.5, containment = 0.8, trim = FALSE,
                       min_sliver = 5) {
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

  result <- .suppress_overlaps(sf_obj, iou = iou, containment = containment)

  if (isTRUE(trim) && nrow(result) > 1) {
    result <- .trim_overlaps(result, min_sliver = min_sliver)
  }

  result
}


#' Trim residual overlaps between detections
#'
#' Subtracts each higher-scored geometry from lower-scored ones so that no
#' two polygons overlap. Trimmed pieces smaller than `min_sliver` square
#' meters are dropped.
#'
#' @param sf_obj An sf object with a `score` column.
#' @param min_sliver Minimum area in square meters to keep after trimming.
#' @return An sf object with non-overlapping polygons.
#' @noRd
.trim_overlaps <- function(sf_obj, min_sliver = 5) {
  merc <- "+proj=merc +a=6378137 +b=6378137 +lat_ts=0 +lon_0=0 +x_0=0 +y_0=0 +k=1 +units=m +nadgrids=@null +wktext +no_defs"
  proj <- sf::st_transform(sf_obj, merc)
  geoms <- sf::st_geometry(proj)
  n <- nrow(sf_obj)
  scores <- if ("score" %in% names(sf_obj)) sf_obj$score else rep(1, n)

  ord <- order(scores, decreasing = TRUE)
  rank <- integer(n)
  rank[ord] <- seq_len(n)

  # Each polygon is trimmed only against intersecting higher-scored
  # neighbors (via the sparse spatial index), not a global accumulator.
  # Processing in score order means those neighbors are already final.
  neighbors <- sf::st_intersects(geoms)
  keep <- rep(TRUE, n)

  for (idx in ord) {
    higher <- neighbors[[idx]]
    higher <- higher[higher != idx & keep[higher] & rank[higher] < rank[idx]]
    if (length(higher) == 0) next

    g <- suppressWarnings(
      sf::st_difference(geoms[idx], sf::st_union(geoms[higher]))
    )

    if (length(g) == 0) {
      keep[idx] <- FALSE
      next
    }
    # st_difference can return collections; keep polygonal parts only
    if (!all(sf::st_geometry_type(g) %in% c("POLYGON", "MULTIPOLYGON"))) {
      g <- suppressWarnings(sf::st_collection_extract(g, "POLYGON"))
      if (length(g) == 0) {
        keep[idx] <- FALSE
        next
      }
      g <- sf::st_combine(g)
    }
    if (sum(as.numeric(sf::st_area(g))) < min_sliver) {
      keep[idx] <- FALSE
      next
    }
    geoms[idx] <- g
  }

  out <- proj
  sf::st_geometry(out) <- geoms
  out <- out[keep, ]
  if ("area_m2" %in% names(out)) {
    out$area_m2 <- as.numeric(sf::st_area(out))
  }
  sf::st_transform(out, sf::st_crs(sf_obj))
}

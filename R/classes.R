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
#' @param source Imagery source used ("mapbox", "esri", "maptiler", or NULL).
#' @param history List tracking refinement steps.
#'
#' @return A geosam object.
#' @noRd
new_geosam <- function(
    image_path = character(),
    masks = list(),
    scores = numeric(),
    prompt = list(),
    extent = numeric(4),
    crs = character(),
    source = NULL,
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
      source = source,
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
  # Handle chunked results
  is_chunked <- !is.null(x$sf_result)
  n_detections <- if (is_chunked) nrow(x$sf_result) else length(x$masks)

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

  if (is_chunked) {
    # Show chunked detection info
    if (length(x$history) > 0 && !is.null(x$history[[1]]$n_tiles)) {
      grid <- x$history[[1]]$grid
      n_chunks <- x$history[[1]]$n_tiles
      cli::cli_text("  Mode: chunked ({grid[1]}x{grid[2]} = {n_chunks} chunks)")
    } else {
      cli::cli_text("  Mode: chunked")
    }
  } else if (length(x$image_path) > 0 && nchar(x$image_path) > 0) {
    cli::cli_text("  Image: {basename(x$image_path)}")
  }

  if (length(x$history) > 0 && is.null(x$history[[1]]$n_tiles)) {
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
#' @noRd
validate_geosam <- function(x) {
  if (!inherits(x, "geosam")) {
    cli::cli_abort("{.arg x} must be a {.cls geosam} object.")
  }

  if (length(x$masks) != length(x$scores)) {
    cli::cli_abort("Number of masks ({length(x$masks)}) must equal number of scores ({length(x$scores)}).")
  }

  invisible(x)
}


# --------------------------------------------------------------------------
# geosam_image class - for non-georeferenced images
# --------------------------------------------------------------------------

#' Create a geosam_image Object
#'
#' Internal constructor for geosam_image class objects (non-georeferenced images).
#' Users typically get geosam_image objects from `sam_image()` or
#' `sam_explore_image()`, not by calling this directly.
#'
#' @param image_path Path to the source image (PNG, JPG, etc.).
#' @param masks List of binary mask arrays from SAM detection.
#' @param scores Numeric vector of confidence scores per mask.
#' @param prompt List with `type` and `value` describing the prompt used.
#' @param dimensions Integer vector of length 2: c(width, height) in pixels.
#' @param history List tracking refinement steps.
#'
#' @return A geosam_image object.
#' @noRd
new_geosam_image <- function(
    image_path = character(),
    masks = list(),
    scores = numeric(),
    prompt = list(),
    dimensions = integer(2),
    history = list()
) {
  structure(
    list(
      image_path = image_path,
      masks = masks,
      scores = scores,
      prompt = prompt,
      dimensions = dimensions,
      history = history
    ),
    class = "geosam_image"
  )
}


#' Print Method for geosam_image Objects
#'
#' @param x A geosam_image object.
#' @param ... Additional arguments (ignored).
#'
#' @return Invisibly returns `x`.
#' @export
print.geosam_image <- function(x, ...) {
  n_detections <- length(x$masks)

  cli::cli_text("{.cls geosam_image} object")
  cli::cli_text("
  Detections: {n_detections}")

  if (length(x$dimensions) == 2 && all(x$dimensions > 0)) {
    cli::cli_text("  Dimensions: {x$dimensions[1]} x {x$dimensions[2]} pixels")
  }

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
  Use {.fn sam_as_sf} to extract polygons (pixel coordinates)")
  }

  invisible(x)
}


#' Check if Object is a geosam_image
#'
#' @param x Object to test.
#' @return Logical indicating if `x` is a geosam_image object.
#' @export
is_geosam_image <- function(x) {
  inherits(x, "geosam_image")
}


#' Validate geosam_image Object
#'
#' Internal function to validate geosam_image object structure.
#'
#' @param x A geosam_image object.
#' @return The validated geosam_image object (invisibly).
#' @noRd
validate_geosam_image <- function(x) {

  if (!inherits(x, "geosam_image")) {
    cli::cli_abort("{.arg x} must be a {.cls geosam_image} object.")
  }

  if (length(x$masks) != length(x$scores)) {
    cli::cli_abort("Number of masks ({length(x$masks)}) must equal number of scores ({length(x$scores)}).")
  }

  if (length(x$dimensions) != 2) {
    cli::cli_abort("Dimensions must be a vector of length 2 (width, height).")
  }

  invisible(x)
}


# --------------------------------------------------------------------------
# Plot methods
# --------------------------------------------------------------------------

#' Plot geosam Detection Results
#'
#' Displays satellite imagery with detection polygons overlaid using terra.
#'
#' @param x A geosam object from `sam_detect()` or `sam_explore()`.
#' @param fill Fill color for detection polygons. Default is semi-transparent yellow.
#'   Ignored if `palette` is specified.
#' @param border Border color for polygon outlines. If NULL (default), automatically
#'   darkens the fill color. Set to NA to disable borders.
#' @param palette Optional color palette for distinct colors per detection. Can be:
#'
#'   - A character vector of colors (recycled if needed)
#'   - A palette name from RColorBrewer (e.g., "Set1", "Dark2", "Paired")
#'   - A palette function that takes n and returns colors
#' @param opacity Opacity for fill colors (0-1). Default is 0.5. Applied to all fills.
#' @param lwd Line width for polygon borders. Default is 1.
#' @param main Plot title. Default is NULL (no title).
#' @param add If TRUE, add to existing plot instead of creating new one.
#'   Default is FALSE.
#' @param ... Additional arguments passed to `terra::plotRGB()` when `add = FALSE`.
#'
#' @return Invisibly returns the sf polygons that were plotted.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- sam_detect(
#'   bbox = c(-102.63, 31.81, -102.62, 31.83),
#'   text = "swimming pool"
#' )
#' plot(result)
#'
#' # Distinct colors per detection
#' plot(result, palette = "Set1")
#'
#' # Custom colors
#' plot(result, fill = "red")  # border auto-darkens
#'
#' # Layer multiple results
#' plot(result1)
#' plot(result2, add = TRUE, fill = "blue")
#' }
plot.geosam <- function(x, fill = "#FACC15", border = NULL, palette = NULL,
                        opacity = 0.5, lwd = 1, main = NULL, add = FALSE, ...) {
  validate_geosam(x)

  # Get detection polygons
  polys <- sam_as_sf(x)

  if (is.null(polys) || nrow(polys) == 0) {
    cli::cli_alert_warning("No detections to plot.")
    return(invisible(NULL))
  }

  # Check if we have an image to display
  has_image <- length(x$image_path) > 0 &&
               nchar(x$image_path) > 0 &&
               file.exists(x$image_path)

  # Load imagery with terra and plot base if not adding
  if (!add) {
    if (has_image) {
      img <- terra::rast(x$image_path)
      terra::plotRGB(img, main = main, ...)

      # Transform polygons to image CRS
      img_crs <- terra::crs(img)
      if (!is.na(img_crs) && img_crs != "") {
        polys <- sf::st_transform(polys, img_crs)
      }
    } else {
      # No image available, just plot polygons
      if (is.null(main)) {
        main <- "Detection Results"
      }
      plot(sf::st_geometry(polys), col = NA, border = NA, main = main, ...)
    }
  }

  # Generate colors
  n <- nrow(polys)
  colors <- .get_plot_colors(n, fill, palette, opacity)

  # Generate border colors
  if (is.null(border)) {
    borders <- .darken_colors(colors)
  } else if (identical(border, NA)) {
    borders <- NA
  } else {
    borders <- rep_len(border, n)
  }

  # Plot each polygon
  for (i in seq_len(n)) {
    plot(sf::st_geometry(polys[i, ]), add = TRUE,
         col = colors[i], border = borders[i], lwd = lwd)
  }

  invisible(polys)
}


#' Plot geosam_image Detection Results
#'
#' Displays an image with detection masks overlaid using magick.
#'
#' @param x A geosam_image object from `sam_image()` or `sam_explore_image()`.
#' @param fill Fill color for detection masks. Default is yellow.
#'   Ignored if `palette` is specified.
#' @param border Border color for detection outlines. If NULL (default), automatically
#'   darkens the fill color. Set to NA to disable borders.
#' @param palette Optional color palette for distinct colors per detection. Can be:
#'
#'   - A character vector of colors (recycled if needed)
#'   - A palette name from RColorBrewer (e.g., "Set1", "Dark2", "Paired")
#'   - A palette function that takes n and returns colors
#' @param opacity Opacity of the mask overlay (0-1). Default is 0.4.
#' @param border_width Width of border lines in pixels. Default is 2.
#' @param add If TRUE, add to an existing magick image instead of loading from file.
#'   Pass the image as the `x` parameter's `image_path` will be ignored and `base_img`
#'   must be provided.
#' @param base_img A magick image to add overlays to. Required when `add = TRUE`.
#' @param ... Additional arguments (currently ignored).
#'
#' @return Invisibly returns the composite magick image, which can be saved
#'   with `magick::image_write()` or further manipulated.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' result <- sam_image("photo.jpg", text = "dog")
#' plot(result)
#'
#' # Distinct colors per detection
#' plot(result, palette = "Set2")
#'
#' # Custom color (border auto-matches)
#' plot(result, fill = "red")
#'
#' # Layer multiple detections
#' img <- plot(result1)
#' img <- plot(result2, add = TRUE, base_img = img, fill = "blue")
#' img <- plot(result3, add = TRUE, base_img = img, fill = "red")
#'
#' # Save the result
#' magick::image_write(img, "annotated.png")
#' }
plot.geosam_image <- function(x, fill = "#FACC15", border = NULL, palette = NULL,
                               opacity = 0.4, border_width = 2,
                               add = FALSE, base_img = NULL, ...) {
  validate_geosam_image(x)

  # Handle base image
  if (add) {
    if (is.null(base_img)) {
      cli::cli_abort("Must provide {.arg base_img} when {.code add = TRUE}.")
    }
    img <- base_img
  } else {
    if (!file.exists(x$image_path)) {
      cli::cli_abort("Image file not found: {x$image_path}")
    }
    img <- magick::image_read(x$image_path)
  }

  img_info <- magick::image_info(img)
  width <- img_info$width
  height <- img_info$height

  if (length(x$masks) == 0) {
    cli::cli_alert_warning("No detections to plot.")
    if (!add) print(img)
    return(invisible(img))
  }

  # Generate colors for each detection
  n <- length(x$masks)
  colors <- .get_plot_colors(n, fill, palette, opacity = 1)  # opacity applied separately

  # Generate border colors
  if (is.null(border)) {
    borders <- .darken_colors(colors)
  } else if (identical(border, NA)) {
    borders <- rep(NA_character_, n)
  } else {
    borders <- rep_len(border, n)
  }

  result <- img

  # Process each mask with its own color
  for (i in seq_along(x$masks)) {
    mask <- x$masks[[i]]

    # Handle potential dimension mismatch
    mask_h <- nrow(mask)
    mask_w <- ncol(mask)

    if (mask_h == width && mask_w == height) {
      mask <- t(mask)
    }

    # Get color components for this mask
    fill_rgb <- grDevices::col2rgb(colors[i])
    fill_r <- fill_rgb[1, 1]
    fill_g <- fill_rgb[2, 1]
    fill_b <- fill_rgb[3, 1]

    # Build overlay for this mask using vectorized operations
    mask_indices <- which(mask == 1)

    if (length(mask_indices) > 0) {
      # Create overlay array
      overlay_array <- array(as.raw(0), dim = c(4, width, height))

      # Convert linear indices to row, col
      rows <- ((mask_indices - 1) %% height) + 1
      cols <- ((mask_indices - 1) %/% height) + 1

      # Fill overlay at mask positions
      for (j in seq_along(mask_indices)) {
        overlay_array[1, cols[j], rows[j]] <- as.raw(fill_r)
        overlay_array[2, cols[j], rows[j]] <- as.raw(fill_g)
        overlay_array[3, cols[j], rows[j]] <- as.raw(fill_b)
        overlay_array[4, cols[j], rows[j]] <- as.raw(as.integer(255 * opacity))
      }

      overlay_img <- magick::image_read(overlay_array)
      result <- magick::image_composite(result, overlay_img, operator = "over")
    }

    # Draw border for this mask if requested
    if (!is.na(borders[i]) && border_width > 0) {
      # Get polygon for this specific mask
      single_result <- x
      single_result$masks <- list(x$masks[[i]])
      single_result$scores <- x$scores[i]

      polys <- sam_as_sf(single_result)

      if (!is.null(polys) && nrow(polys) > 0) {
        border_canvas <- magick::image_blank(width, height, color = "transparent")

        for (p in seq_len(nrow(polys))) {
          geom <- sf::st_geometry(polys[p, ])[[1]]
          coords <- .extract_polygon_coords(geom, height)

          if (length(coords) > 0 && length(coords$x) > 0) {
            border_canvas <- magick::image_draw(border_canvas)
            graphics::polygon(
              coords$x, coords$y,
              border = borders[i],
              lwd = border_width,
              col = NA
            )
            grDevices::dev.off()
          }
        }

        result <- magick::image_composite(result, border_canvas, operator = "over")
      }
    }
  }

  # Display unless adding
  if (!add) {
    print(result)
  }

  invisible(result)
}


#' Get colors for plotting
#' @noRd
.get_plot_colors <- function(n, fill, palette, opacity = 1) {
  if (!is.null(palette)) {
    colors <- if (is.function(palette)) {
      palette(n)
    } else if (length(palette) == 1 && requireNamespace("RColorBrewer", quietly = TRUE) &&
               palette %in% rownames(RColorBrewer::brewer.pal.info)) {
      max_colors <- RColorBrewer::brewer.pal.info[palette, "maxcolors"]
      RColorBrewer::brewer.pal(min(n, max_colors), palette)
    } else {
      # Treat as vector of colors
      palette
    }
    colors <- rep_len(colors, n)
  } else {
    colors <- rep_len(fill, n)
  }

  # Apply opacity
  if (opacity < 1) {
    colors <- .add_alpha(colors, opacity)
  }

  colors
}


#' Add alpha to colors
#' @noRd
.add_alpha <- function(colors, alpha) {
  rgb_vals <- grDevices::col2rgb(colors)
  grDevices::rgb(
    rgb_vals[1, ], rgb_vals[2, ], rgb_vals[3, ],
    alpha = alpha * 255,
    maxColorValue = 255
  )
}


#' Darken colors for borders
#' @noRd
.darken_colors <- function(colors, factor = 0.7) {
  sapply(colors, function(col) {
    if (is.na(col)) return(NA_character_)
    rgb_vals <- grDevices::col2rgb(col, alpha = TRUE)
    grDevices::rgb(
      rgb_vals[1, 1] * factor,
      rgb_vals[2, 1] * factor,
      rgb_vals[3, 1] * factor,
      maxColorValue = 255
    )
  }, USE.NAMES = FALSE)
}


#' Extract polygon coordinates for plotting
#' @noRd
.extract_polygon_coords <- function(geom, img_height) {
  # Handle different geometry types
  if (inherits(geom, "POLYGON")) {
    coords_mat <- sf::st_coordinates(geom)[, 1:2, drop = FALSE]
  } else if (inherits(geom, "MULTIPOLYGON")) {
    # Get coordinates from all parts
    coords_mat <- sf::st_coordinates(geom)[, 1:2, drop = FALSE]
  } else {
    return(list())
  }

  if (nrow(coords_mat) == 0) {
    return(list())
  }

  # Coordinates from .sam_as_sf_image use terra's coordinate system
  # with extent (0, width, 0, height) where y=0 is at bottom.
  # For magick/image_draw, y=0 is at top.
  # Use coordinates directly - the y values from terra are already
  # in the range [0, height] and work as image coordinates.
  list(
    x = coords_mat[, 1],
    y = coords_mat[, 2]
  )
}


#' Convert coordinates to SVG path string
#' @noRd
.coords_to_svg_path <- function(coords) {
  if (length(coords$x) == 0) return("")

  # Build path: M x0,y0 L x1,y1 L x2,y2 ... Z
  parts <- sprintf("%.1f,%.1f", coords$x, coords$y)
  paste0("M ", parts[1], " L ", paste(parts[-1], collapse = " L "), " Z")
}

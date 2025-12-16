#' Download Satellite Imagery
#'
#' Downloads satellite imagery tiles for a bounding box and saves as a GeoTIFF.
#' Uses R-native tools (httr2, terra) for reliable tile fetching and georeferencing.
#'
#' @param bbox Bounding box for the area. Can be a numeric vector
#'   `c(xmin, ymin, xmax, ymax)` in WGS84, or an sf/sfc object.
#' @param output Path for output GeoTIFF. If NULL, creates a temp file.
#' @param source Imagery source: "mapbox", "esri", or "maptiler".
#'   - "mapbox": Requires `MAPBOX_PUBLIC_TOKEN` environment variable
#'   - "esri": Free Esri World Imagery (no API key required)
#'   - "maptiler": Requires `MAPTILER_API_KEY` environment variable
#' @param zoom Tile zoom level (15-19). Higher values = more detail.
#'   Recommended: 17-18 for objects like buildings, swimming pools.
#' @param api_key API key for the imagery source. For Mapbox, uses
#'   MAPBOX_PUBLIC_TOKEN environment variable by default. For MapTiler, uses
#'   MAPTILER_API_KEY environment variable by default.
#'
#' @return Path to the downloaded GeoTIFF file.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Download imagery for an area
#' img <- get_imagery(
#'   bbox = c(-102.5, 31.8, -102.4, 31.9),
#'   source = "mapbox",
#'   zoom = 17
#' )
#'
#' # Use with detection
#' pads <- sam_detect(image = img, text = "swimming pool")
#' }
get_imagery <- function(
    bbox,
    output = NULL,
    source = c("mapbox", "esri", "maptiler"),
    zoom = 17,
    api_key = NULL
) {
  source <- match.arg(source)

  # Convert bbox if sf object
 if (inherits(bbox, c("sf", "sfc"))) {
    bbox <- sf::st_bbox(sf::st_transform(bbox, 4326))
    bbox <- as.numeric(bbox[c("xmin", "ymin", "xmax", "ymax")])
  } else if (inherits(bbox, "bbox")) {
    bbox <- as.numeric(bbox[c("xmin", "ymin", "xmax", "ymax")])
  } else if (!is.numeric(bbox) || length(bbox) != 4) {
    cli::cli_abort("{.arg bbox} must be a numeric vector of length 4 or an sf object.")
  }

  # Get API key for Mapbox
  if (source == "mapbox") {
    if (is.null(api_key)) {
      api_key <- .get_mapbox_token()
    }
    if (is.null(api_key)) {
      cli::cli_abort(c(
        "Mapbox API key required.",
        "i" = "Set environment variable: {.code MAPBOX_PUBLIC_TOKEN}",
        "i" = "Or via {.code geosam_configure(mapbox_token = 'pk.xxx')}"
      ))
    }
  }

  # Get API key for MapTiler
  if (source == "maptiler") {
    if (is.null(api_key)) {
      api_key <- Sys.getenv("MAPTILER_API_KEY", unset = NA)
      if (is.na(api_key) || api_key == "") {
        api_key <- NULL
      }
    }
    if (is.null(api_key)) {
      cli::cli_abort(c(
        "MapTiler API key required.",
        "i" = "Set environment variable: {.code MAPTILER_API_KEY}",
        "i" = "Get a free key at: {.url https://www.maptiler.com/}"
      ))
    }
  }

  # Create output path if not provided
  if (is.null(output)) {
    output <- tempfile(fileext = ".tif")
  }

  # Ensure output directory exists
  dir.create(dirname(output), showWarnings = FALSE, recursive = TRUE)

  # Show ToS notice (once per session per provider)
  .notify_tos(source)

  # Get display name for provider
 source_name <- .source_display_name(source)

  cli::cli_alert_info("Downloading {source_name} imagery at zoom {zoom}...")

  # Download tiles using R-native implementation
  .download_tiles(
    bbox = bbox,
    output = output,
    source = source,
    zoom = zoom,
    api_key = api_key
  )

  if (!file.exists(output)) {
    cli::cli_abort("Failed to download imagery.")
  }

  cli::cli_alert_success("Saved imagery to: {output}")

  output
}


#' Convert lon/lat to tile coordinates
#' @noRd
.lonlat_to_tile <- function(lon, lat, zoom) {
  n <- 2^zoom
  x <- floor((lon + 180) / 360 * n)
  lat_rad <- lat * pi / 180
  y <- floor((1 - asinh(tan(lat_rad)) / pi) / 2 * n)
  c(x = x, y = y)
}


#' Convert tile coordinates to lon/lat (NW corner)
#' @noRd
.tile_to_lonlat <- function(x, y, zoom) {
  n <- 2^zoom
  lon <- x / n * 360 - 180
  lat_rad <- atan(sinh(pi * (1 - 2 * y / n)))
  lat <- lat_rad * 180 / pi
  c(lon = lon, lat = lat)
}


#' Convert lon/lat to Web Mercator
#' @noRd
.lonlat_to_mercator <- function(lon, lat) {
  x <- lon * 20037508.34 / 180
  y <- log(tan((90 + lat) * pi / 360)) / (pi / 180)
  y <- y * 20037508.34 / 180
  c(x = x, y = y)
}


#' Build tile URL
#' @noRd
.build_tile_url <- function(x, y, zoom, source, api_key) {
  switch(source,
    mapbox = sprintf(
      "https://api.mapbox.com/v4/mapbox.satellite/%d/%d/%d@2x.jpg?access_token=%s",
      zoom, x, y, api_key
    ),
    esri = sprintf(
      "https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/%d/%d/%d",
      zoom, y, x
    ),
    maptiler = sprintf(
      "https://api.maptiler.com/tiles/satellite-v2/%d/%d/%d.jpg?key=%s",
      zoom, x, y, api_key
    )
  )
}


#' Download a single tile
#' @noRd
.download_tile <- function(x, y, zoom, source, api_key) {
  url <- .build_tile_url(x, y, zoom, source, api_key)

  tryCatch({
    resp <- httr2::request(url) |>
      httr2::req_timeout(30) |>
      httr2::req_perform()

    if (httr2::resp_status(resp) == 200) {
      httr2::resp_body_raw(resp)
    } else {
      NULL
    }
  }, error = function(e) {
    NULL
  })
}


#' Download and merge tiles into GeoTIFF
#' @noRd
.download_tiles <- function(bbox, output, source, zoom, api_key) {
  west <- bbox[1]
  south <- bbox[2]
  east <- bbox[3]
  north <- bbox[4]

 # Get tile range
  nw_tile <- .lonlat_to_tile(west, north, zoom)
  se_tile <- .lonlat_to_tile(east, south, zoom)

  x_min <- min(nw_tile[1], se_tile[1])
  x_max <- max(nw_tile[1], se_tile[1])
  y_min <- min(nw_tile[2], se_tile[2])
  y_max <- max(nw_tile[2], se_tile[2])

  n_cols <- x_max - x_min + 1
  n_rows <- y_max - y_min + 1
  n_tiles <- n_cols * n_rows
  cli::cli_alert_info("Fetching {n_tiles} tiles ({n_cols}x{n_rows})...")

  # Tile size (Mapbox @2x = 512, others = 256)
  tile_size <- if (source == "mapbox") 512L else 256L

  # Download tiles and store in list organized by row
  tile_rows <- list()

  for (y in y_min:y_max) {
    row_tiles <- list()
    for (x in x_min:x_max) {
      tile_data <- .download_tile(x, y, zoom, source, api_key)

      if (!is.null(tile_data)) {
        tile_img <- tryCatch({
          magick::image_read(tile_data)
        }, error = function(e) NULL)

        if (!is.null(tile_img)) {
          # Resize if needed
          tile_info <- magick::image_info(tile_img)
          if (tile_info$width != tile_size || tile_info$height != tile_size) {
            tile_img <- magick::image_resize(tile_img, paste0(tile_size, "x", tile_size, "!"))
          }
          row_tiles[[length(row_tiles) + 1]] <- tile_img
        } else {
          # Create blank tile
          row_tiles[[length(row_tiles) + 1]] <- magick::image_blank(tile_size, tile_size, "black")
        }
      } else {
        # Create blank tile
        row_tiles[[length(row_tiles) + 1]] <- magick::image_blank(tile_size, tile_size, "black")
      }
    }
    # Append tiles horizontally into a row
    if (length(row_tiles) > 0) {
      row_img <- magick::image_append(do.call(c, row_tiles), stack = FALSE)
      tile_rows[[length(tile_rows) + 1]] <- row_img
    }
  }

  # Stack rows vertically
  merged_img <- magick::image_append(do.call(c, tile_rows), stack = TRUE)

  # Flip image vertically - terra expects rows from south to north,
  # but web tiles are arranged north to south
  merged_img <- magick::image_flip(merged_img)

  # Save as temp PNG
  tmp_png <- tempfile(fileext = ".png")
  magick::image_write(merged_img, tmp_png, format = "png")

  # Read with terra (suppress extent warning for raw PNG)
  r <- suppressWarnings(terra::rast(tmp_png))

  # Calculate bounds in Web Mercator
  nw_lonlat <- .tile_to_lonlat(x_min, y_min, zoom)
  se_lonlat <- .tile_to_lonlat(x_max + 1, y_max + 1, zoom)

  nw_merc <- .lonlat_to_mercator(nw_lonlat[1], nw_lonlat[2])
  se_merc <- .lonlat_to_mercator(se_lonlat[1], se_lonlat[2])

  # Set extent and CRS
  terra::ext(r) <- terra::ext(nw_merc[1], se_merc[1], se_merc[2], nw_merc[2])
  terra::crs(r) <- "EPSG:3857"

  # Write GeoTIFF
  terra::writeRaster(r, output, overwrite = TRUE, datatype = "INT1U")

  # Clean up
  unlink(tmp_png)

  output
}


#' Calculate pixel dimensions of a bbox at a given zoom
#' @param bbox Numeric vector c(west, south, east, north)
#' @param zoom Zoom level
#' @param source Imagery source (affects tile size)
#' @return Named list with width, height, n_tiles_x, n_tiles_y
#' @noRd
.calc_bbox_pixels <- function(bbox, zoom, source = "mapbox") {
  west <- bbox[1]
  south <- bbox[2]
  east <- bbox[3]
  north <- bbox[4]

  # Get tile range
  nw_tile <- .lonlat_to_tile(west, north, zoom)
  se_tile <- .lonlat_to_tile(east, south, zoom)

  n_tiles_x <- abs(se_tile[1] - nw_tile[1]) + 1
  n_tiles_y <- abs(se_tile[2] - nw_tile[2]) + 1

  # Tile size: Mapbox @2x = 512, others = 256
 tile_size <- if (source == "mapbox") 512L else 256L

  list(
    width = n_tiles_x * tile_size,
    height = n_tiles_y * tile_size,
    n_tiles_x = n_tiles_x,
    n_tiles_y = n_tiles_y,
    tile_size = tile_size
  )
}


#' Split a bbox into optimal detection tiles
#'
#' Splits a large bbox into smaller tiles suitable for SAM detection.
#' Target tile size is approximately 1000-1500 pixels.
#'
#' @param bbox Numeric vector c(west, south, east, north)
#' @param zoom Zoom level
#' @param source Imagery source
#' @param max_pixels Maximum pixels in either dimension before chunking (default 2500)
#' @return List of bbox vectors, or original bbox if no chunking needed
#' @noRd
.split_bbox_for_detection <- function(bbox, zoom, source = "mapbox", max_pixels = 2500) {
  dims <- .calc_bbox_pixels(bbox, zoom, source)

  # Check if chunking is needed
  if (dims$width <= max_pixels && dims$height <= max_pixels) {
    return(list(bbox))
  }

  # Calculate how many tiles we need in each direction
  # Target ~1000-1200 pixels per tile
  target_pixels <- 1000

  tiles_x <- ceiling(dims$width / target_pixels)
  tiles_y <- ceiling(dims$height / target_pixels)

  west <- bbox[1]
  south <- bbox[2]
  east <- bbox[3]
  north <- bbox[4]

  width <- east - west
  height <- north - south

  tile_width <- width / tiles_x
  tile_height <- height / tiles_y

  # Generate tile bboxes
  tile_bboxes <- list()

  for (i in seq_len(tiles_x)) {
    for (j in seq_len(tiles_y)) {
      tile_west <- west + (i - 1) * tile_width
      tile_east <- tile_west + tile_width
      tile_south <- south + (j - 1) * tile_height
      tile_north <- tile_south + tile_height

      tile_bboxes[[length(tile_bboxes) + 1]] <- c(tile_west, tile_south, tile_east, tile_north)
    }
  }

  attr(tile_bboxes, "grid") <- c(tiles_x, tiles_y)
  tile_bboxes
}


#' Clear Imagery Cache
#'
#' Clears cached satellite imagery tiles.
#'
#' @return Invisibly returns TRUE.
#'
#' @export
geosam_clear_cache <- function() {
  cache_dir <- rappdirs::user_cache_dir("geosam")

  if (dir.exists(cache_dir)) {
    unlink(cache_dir, recursive = TRUE)
    cli::cli_alert_success("Cache cleared: {cache_dir}")
  } else {
    cli::cli_alert_info("No cache to clear.")
  }

  invisible(TRUE)
}


#' Get display name for imagery source
#' @noRd
.source_display_name <- function(source) {
  switch(source,
    mapbox = "Mapbox",
    esri = "Esri",
    maptiler = "MapTiler",
    source
  )
}


#' Notify user of Terms of Service (once per session per provider)
#' @noRd
.notify_tos <- function(source) {
  # Check if already shown this session
  if (source %in% .geosam_env$tos_shown) {
    return(invisible())
  }

  # Mark as shown
  .geosam_env$tos_shown <- c(.geosam_env$tos_shown, source)

  # Provider-specific ToS messages (using cli for proper formatting)
  switch(source,
    mapbox = cli::cli_inform(c(
      "i" = "Mapbox imagery is governed by the Mapbox Terms of Service.",
      " " = "See: {.url https://www.mapbox.com/legal/tos/}"
    )),
    esri = cli::cli_inform(c(
      "i" = "Esri World Imagery is governed by the Esri Master License Agreement.",
      " " = "See: {.url https://www.esri.com/en-us/legal/terms/full-master-agreement}"
    )),
    maptiler = cli::cli_inform(c(
      "i" = "MapTiler imagery is governed by the MapTiler Terms of Service.",
      " " = "See: {.url https://www.maptiler.com/terms/}"
    ))
  )

  invisible()
}

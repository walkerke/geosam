#' Download Satellite Imagery
#'
#' Downloads satellite imagery tiles for a bounding box and saves as a GeoTIFF.
#' Uses R-native tools (httr2, terra) for reliable tile fetching and georeferencing.
#'
#' @param bbox Bounding box for the area. Can be a numeric vector
#'   `c(xmin, ymin, xmax, ymax)` in WGS84, or an sf/sfc object.
#' @param output Path for output GeoTIFF. If NULL, creates a temp file.
#' @param source Imagery source: "mapbox", "esri", "maptiler", or "google".
#'   - "mapbox": Requires `MAPBOX_PUBLIC_TOKEN` environment variable
#'   - "esri": Free Esri World Imagery (no API key required)
#'   - "maptiler": Requires `MAPTILER_API_KEY` environment variable
#'   - "google": Google satellite tiles (no API key required)
#' @param zoom Tile zoom level (15-19). Higher values = more detail.
#'   Recommended: 17-18 for objects like buildings, well pads.
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
#' pads <- sam_detect(image = img, text = "well pad")
#' }
get_imagery <- function(
    bbox,
    output = NULL,
    source = c("mapbox", "esri", "maptiler", "google"),
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

  cli::cli_alert_info("Downloading {source} imagery at zoom {zoom}...")

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
#' @keywords internal
.lonlat_to_tile <- function(lon, lat, zoom) {
  n <- 2^zoom
  x <- floor((lon + 180) / 360 * n)
  lat_rad <- lat * pi / 180
  y <- floor((1 - asinh(tan(lat_rad)) / pi) / 2 * n)
  c(x = x, y = y)
}


#' Convert tile coordinates to lon/lat (NW corner)
#' @keywords internal
.tile_to_lonlat <- function(x, y, zoom) {
  n <- 2^zoom
  lon <- x / n * 360 - 180
  lat_rad <- atan(sinh(pi * (1 - 2 * y / n)))
  lat <- lat_rad * 180 / pi
  c(lon = lon, lat = lat)
}


#' Convert lon/lat to Web Mercator
#' @keywords internal
.lonlat_to_mercator <- function(lon, lat) {
  x <- lon * 20037508.34 / 180
  y <- log(tan((90 + lat) * pi / 360)) / (pi / 180)
  y <- y * 20037508.34 / 180
  c(x = x, y = y)
}


#' Build tile URL
#' @keywords internal
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
    ),
    google = sprintf(
      "https://mt1.google.com/vt/lyrs=s&x=%d&y=%d&z=%d",
      x, y, zoom
    )
  )
}


#' Download a single tile
#' @keywords internal
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
#' @keywords internal
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

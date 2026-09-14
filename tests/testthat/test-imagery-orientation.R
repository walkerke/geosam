# Regression tests for the tile stitcher (.download_tiles). Fetched mosaics
# were once vertically mirrored on some terra/GDAL builds because the mosaic
# was flipped and round-tripped through an ungeoreferenced PNG. These tests
# use an asymmetric synthetic tile fixture so any row flip, column flip or
# transpose is caught, and check geographic placement of the mosaic.

# Each synthetic tile has a base colour encoding its (x, y) tile index and a
# bright marker in its top-left quadrant. Tile (5, 11) is reported missing.
.fixture_tile_colour <- function(x, y) {
  c((x * 20L) %% 250L, (y * 20L) %% 250L, 100L)
}

.fixture_download_tile <- function(x, y, zoom, source, api_key) {
  ts <- if (source == "mapbox") 512L else 256L
  if (x == 5L && y == 11L) return(NULL)
  rgb <- .fixture_tile_colour(x, y)
  img <- magick::image_blank(ts, ts, sprintf("#%02x%02x%02x", rgb[1], rgb[2], rgb[3]))
  marker <- magick::image_blank(ts %/% 2L, ts %/% 2L, "#f0f0f0")
  img <- magick::image_composite(img, marker, offset = "+0+0")
  magick::image_write(img, format = "png")
}

# bbox covering tiles x 4..6, y 10..11 at zoom 4 (a rectangular 3x2 mosaic)
.fixture_bbox <- function() {
  nw <- geosam:::.tile_to_lonlat(4, 10, 4)
  se <- geosam:::.tile_to_lonlat(7, 12, 4)
  c(nw[1] + 1e-6, se[2] + 1e-6, se[1] - 1e-6, nw[2] - 1e-6)
}

.stitch_fixture <- function(source) {
  out <- tempfile(fileext = ".tif")
  testthat::local_mocked_bindings(
    .download_tile = .fixture_download_tile,
    .package = "geosam"
  )
  suppressMessages(suppressWarnings(
    geosam:::.download_tiles(.fixture_bbox(), out, source = source, zoom = 4, api_key = "x")
  ))
  suppressWarnings(terra::rast(out))
}

.px <- function(r, row, col) unname(as.integer(r[row, col][1, ]))

for (source in c("mapbox", "esri", "maptiler")) {
  ts <- if (source == "mapbox") 512L else 256L

  test_that(paste0("stitched mosaic is north-up and not mirrored (", source, ")"), {
    r <- .stitch_fixture(source)
    expect_equal(c(terra::nrow(r), terra::ncol(r), terra::nlyr(r)), c(2L * ts, 3L * ts, 3L))

    # Raster row 1 / col 1 is the top-left of the NW tile (4, 10): the marker
    expect_equal(.px(r, 1, 1), c(240, 240, 240))
    # Bottom-right of the NW tile is its base colour
    expect_equal(.px(r, ts, ts), .fixture_tile_colour(4L, 10L))
    # Marker of the NW tile does not extend beyond its quadrant (catches transposes)
    expect_equal(.px(r, 1, ts %/% 2L + 1L), .fixture_tile_colour(4L, 10L))
    expect_equal(.px(r, ts %/% 2L + 1L, 1), .fixture_tile_colour(4L, 10L))
    # Last pixel of the mosaic belongs to the SE tile (6, 11); its marker sits
    # at the tile's own top-left. A vertical flip would put row 2*ts at tile row 10.
    expect_equal(.px(r, 2L * ts, 3L * ts), .fixture_tile_colour(6L, 11L))
    expect_equal(.px(r, ts + 1L, 2L * ts + 1L), c(240, 240, 240))
    # NE tile (6, 10) base colour lands top-right
    expect_equal(.px(r, ts, 3L * ts), .fixture_tile_colour(6L, 10L))
  })

  test_that(paste0("missing tiles are filled with black (", source, ")"), {
    r <- .stitch_fixture(source)
    expect_equal(.px(r, 2L * ts, 2L * ts), c(0, 0, 0))
    expect_equal(.px(r, ts + 1L, ts + 1L), c(0, 0, 0))
  })

  test_that(paste0("mosaic extent matches the tile grid in Web Mercator (", source, ")"), {
    r <- .stitch_fixture(source)
    nw <- geosam:::.tile_to_lonlat(4, 10, 4)
    se <- geosam:::.tile_to_lonlat(7, 12, 4)
    nw_m <- geosam:::.lonlat_to_mercator(nw[1], nw[2])
    se_m <- geosam:::.lonlat_to_mercator(se[1], se[2])
    e <- as.vector(terra::ext(r))
    expect_equal(unname(e), unname(c(nw_m[1], se_m[1], se_m[2], nw_m[2])), tolerance = 1e-6)
    expect_true(grepl("merc", terra::crs(r, proj = TRUE)))
    # A point just inside the NW corner of tile (4, 10) must fall in cell (1, 1)
    cell <- terra::cellFromXY(r, cbind(nw_m[1] + 0.1, nw_m[2] - 0.1))
    expect_equal(unname(cell), 1)
  })
}

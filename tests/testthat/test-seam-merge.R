.sq <- function(xmin, ymin, xmax, ymax) {
  sf::st_polygon(list(matrix(
    c(xmin, ymin, xmax, ymin, xmax, ymax, xmin, ymax, xmin, ymin),
    ncol = 2,
    byrow = TRUE
  )))
}

.mk_detections <- function(...) {
  geoms <- sf::st_sfc(list(...), crs = 3857)
  sf::st_transform(
    sf::st_sf(
      score = rep(0.9, length(geoms)),
      area_m2 = as.numeric(sf::st_area(geoms)),
      geometry = geoms
    ),
    4326
  )
}

# Vertical seam at x = 0 (EPSG:3857 meters)
.mk_seam <- function() {
  sf::st_transform(
    sf::st_sfc(
      sf::st_linestring(matrix(c(0, -1000, 0, 1000), ncol = 2, byrow = TRUE)),
      crs = 3857
    ),
    4326
  )
}

test_that("legacy behavior without seams merges nearby polygons", {
  # Two trailer-sized rectangles 1 m apart
  dets <- .mk_detections(
    .sq(20, 0, 23, 16),
    .sq(24, 0, 27, 16)
  )
  merged <- geosam:::.deduplicate_tile_boundaries(dets)
  expect_equal(nrow(merged), 1)
})

test_that("seam-aware merging leaves nearby distinct objects alone", {
  # Same two rectangles, nowhere near the seam at x = 0
  dets <- .mk_detections(
    .sq(20, 0, 23, 16),
    .sq(24, 0, 27, 16)
  )
  merged <- geosam:::.deduplicate_tile_boundaries(dets, seams = .mk_seam())
  expect_equal(nrow(merged), 2)
})

test_that("seam-aware merging heals fragments split at a seam", {
  # Two fragments of one object straddling x = 0, overlapping in the
  # chunk-overlap band (as chunked detection produces)
  dets <- .mk_detections(
    .sq(-10, 0, 2, 8),
    .sq(-2, 0, 10, 8)
  )
  merged <- geosam:::.deduplicate_tile_boundaries(dets, seams = .mk_seam())
  expect_equal(nrow(merged), 1)
})

test_that("seam-aware merging heals non-overlapping fragments across a seam", {
  # Fragments that meet at the seam without an overlap band; centroids on
  # opposite sides of the seam
  dets <- .mk_detections(
    .sq(-10, 0, 0.2, 8),
    .sq(0.2, 0, 10, 8)
  )
  merged <- geosam:::.deduplicate_tile_boundaries(dets, seams = .mk_seam())
  expect_equal(nrow(merged), 1)
})

test_that("objects parked in a row alongside a seam stay separate", {
  # Two objects near (but not crossing) the seam, both on the same side,
  # within buffer distance of each other and of the seam
  dets <- .mk_detections(
    .sq(0.5, 0, 4, 4),
    .sq(0.5, 5, 4, 9)
  )
  merged <- geosam:::.deduplicate_tile_boundaries(dets, seams = .mk_seam())
  expect_equal(nrow(merged), 2)
})

test_that("sam_merge_edges uses stored seams from chunked results", {
  dets <- .mk_detections(
    .sq(20, 0, 23, 16),   # two distinct objects near each other
    .sq(24, 0, 27, 16),
    .sq(-10, 20, 2, 28),  # one object split at the seam
    .sq(-2, 20, 10, 28)
  )
  x <- structure(
    list(sf_result = dets, seams = .mk_seam()),
    class = "geosam"
  )

  merged <- sam_merge_edges(x)
  expect_equal(nrow(merged), 3)

  legacy <- sam_merge_edges(x, seam_aware = FALSE)
  expect_equal(nrow(legacy), 2)
})

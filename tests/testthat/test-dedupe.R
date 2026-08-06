.sq2 <- function(xmin, ymin, xmax, ymax) {
  sf::st_polygon(list(matrix(
    c(xmin, ymin, xmax, ymin, xmax, ymax, xmin, ymax, xmin, ymin),
    ncol = 2,
    byrow = TRUE
  )))
}

.mk_dets2 <- function(polys, scores) {
  geoms <- sf::st_sfc(polys, crs = 3857)
  sf::st_transform(
    sf::st_sf(
      score = scores,
      area_m2 = as.numeric(sf::st_area(geoms)),
      geometry = geoms
    ),
    4326
  )
}

test_that("nested partial masks are suppressed by containment", {
  # A trailer mask plus a lower-scored partial mask nested inside it
  dets <- .mk_dets2(
    list(
      .sq2(0, 0, 3, 16),
      .sq2(0.2, 1, 2.8, 14)
    ),
    scores = c(0.9, 0.6)
  )
  out <- geosam:::.suppress_overlaps(dets)
  expect_equal(nrow(out), 1)
  expect_equal(out$score, 0.9)
})

test_that("offset duplicates are suppressed by IoU", {
  # Two masks of the same object shifted slightly (IoU ~ 0.68)
  dets <- .mk_dets2(
    list(
      .sq2(0, 0, 3, 16),
      .sq2(0.5, 1.5, 3.5, 17.5)
    ),
    scores = c(0.9, 0.7)
  )
  out <- geosam:::.suppress_overlaps(dets)
  expect_equal(nrow(out), 1)
})

test_that("adjacent distinct objects are not suppressed", {
  # Trailers parked 1 m apart: disjoint, must both survive
  dets <- .mk_dets2(
    list(
      .sq2(0, 0, 3, 16),
      .sq2(4, 0, 7, 16)
    ),
    scores = c(0.9, 0.85)
  )
  out <- geosam:::.suppress_overlaps(dets)
  expect_equal(nrow(out), 2)
})

test_that("seam fragments survive suppression and still heal", {
  # Fragments of one object split at a seam at x = 0, sharing only the
  # chunk-overlap band: low IoU, low containment
  seam <- sf::st_transform(
    sf::st_sfc(
      sf::st_linestring(matrix(c(0, -1000, 0, 1000), ncol = 2, byrow = TRUE)),
      crs = 3857
    ),
    4326
  )
  dets <- .mk_dets2(
    list(
      .sq2(-10, 0, 2, 8),
      .sq2(-2, 0, 10, 8)
    ),
    scores = c(0.9, 0.88)
  )
  suppressed <- geosam:::.suppress_overlaps(dets)
  expect_equal(nrow(suppressed), 2)

  healed <- geosam:::.deduplicate_tile_boundaries(suppressed, seams = seam)
  expect_equal(nrow(healed), 1)
})

test_that("sam_dedupe trims residual overlaps to non-overlapping polygons", {
  # Overlap below suppression thresholds (IoU ~ 0.23) survives dedupe but
  # is trimmed away with trim = TRUE
  dets <- .mk_dets2(
    list(
      .sq2(0, 0, 10, 10),
      .sq2(7, 0, 17, 10)
    ),
    scores = c(0.9, 0.8)
  )
  out <- sam_dedupe(dets, trim = TRUE)
  expect_equal(nrow(out), 2)

  merc <- sf::st_transform(out, 3857)
  inter <- suppressWarnings(
    sf::st_intersection(
      sf::st_geometry(merc)[1],
      sf::st_geometry(merc)[2]
    )
  )
  inter_area <- if (length(inter) > 0) {
    sum(as.numeric(sf::st_area(inter)))
  } else {
    0
  }
  expect_lt(inter_area, 1e-6)

  # Higher-scored polygon keeps its full area; lower-scored one lost the
  # overlap strip
  expect_equal(round(merc$area_m2[merc$score == 0.9]), 100)
  expect_equal(round(merc$area_m2[merc$score == 0.8]), 70)
})

test_that("sam_dedupe works on geosam-shaped objects", {
  dets <- .mk_dets2(
    list(
      .sq2(0, 0, 3, 16),
      .sq2(0.2, 1, 2.8, 14),
      .sq2(4, 0, 7, 16)
    ),
    scores = c(0.9, 0.6, 0.85)
  )
  x <- structure(list(sf_result = dets), class = "geosam")
  out <- sam_dedupe(x)
  expect_equal(nrow(out), 2)
})

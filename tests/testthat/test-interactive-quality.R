test_that("interactive quality policies are stable", {
  fast <- geosam:::.explore_detection_policy("fast")
  balanced <- geosam:::.explore_detection_policy("balanced")
  accurate <- geosam:::.explore_detection_policy("accurate")

  expect_false(fast$chunked)
  expect_true(balanced$chunked)
  expect_true(accurate$chunked)
  expect_gt(balanced$chunk_size, accurate$chunk_size)
  expect_lt(balanced$chunk_overlap, accurate$chunk_overlap)
})

test_that("interactive chunk estimates respect quality mode", {
  bbox <- c(-97.372, 32.707, -97.366, 32.712)

  fast <- geosam:::.explore_chunk_estimate(bbox, 17, "mapbox", "fast")
  accurate <- geosam:::.explore_chunk_estimate(bbox, 17, "mapbox", "accurate")

  expect_equal(fast$n_chunks, 1L)
  expect_gte(accurate$n_chunks, 1L)
  expect_named(fast$dims, c("width", "height", "n_tiles_x", "n_tiles_y", "tile_size"))
})

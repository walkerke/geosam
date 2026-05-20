test_that("sam_filter handles chunked sf_result objects", {
  geom <- sf::st_sfc(
    sf::st_polygon(list(matrix(
      c(0, 0, 1, 0, 1, 1, 0, 1, 0, 0),
      ncol = 2,
      byrow = TRUE
    ))),
    sf::st_polygon(list(matrix(
      c(2, 2, 3, 2, 3, 3, 2, 3, 2, 2),
      ncol = 2,
      byrow = TRUE
    ))),
    crs = 4326
  )
  detections <- sf::st_sf(
    score = c(0.95, 0.25),
    area_m2 = c(120, 20),
    geometry = geom
  )

  x <- geosam:::new_geosam(
    image_path = "dummy.tif",
    masks = list(),
    scores = detections$score,
    prompt = list(type = "text", value = "building"),
    extent = c(0, 3, 0, 3),
    crs = "EPSG:4326",
    history = list()
  )
  x$sf_result <- detections

  filtered <- sam_filter(x, min_area = 50, min_score = 0.5)

  expect_equal(nrow(filtered$sf_result), 1L)
  expect_equal(filtered$scores, 0.95)
  expect_equal(filtered$history[[1]]$kept, 1L)
})

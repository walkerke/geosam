library(geosam)
library(mapgl)

# 1. Basic programmatic detection (well pads in Permian Basin)
result <- sam_detect(
  bbox = c(-102.63, 31.81, -102.62, 31.83),
  text = "white or tan cleared area",
  source = "mapbox",
  zoom = 17
)
result
sam_count(result)

wells_sf <- sam_as_sf(result)
plot(wells_sf$geometry)

# 3. Filter by area
filtered <- result |>

  sam_filter(min_area = 500, max_area = 50000)
sam_count(filtered)

refined <- sam_view(result)

# 4. Interactive exploration (opens Shiny gadget)
# Start centered on Midland, TX oil fields
explore_result <- sam_explore(
  source = "mapbox",
  center = c(-97.365, 32.705),
  zoom = 16
)

explore_result <- sam_explore(
  source = "maptiler",
  center = c(-97.365, 32.705),
  zoom = 16
)

# 5. Interactive refinement of existing result
sam_view(result)

# 6. Building detection at TCU
buildings <- sam_detect(
  bbox = c(-97.365, 32.705, -97.355, 32.712),
  text = "building",
  source = "mapbox",
  zoom = 17
)
buildings
sam_as_sf(buildings) |> plot()

buildings_sf <- sam_as_sf(buildings)

mapboxgl_view(buildings_sf, style = mapbox_style("satellite"))

# 7. Get imagery separately, then detect
img <- get_imagery(
  bbox = c(-102.08, 31.95, -102.05, 31.98),
  source = "mapbox",
  zoom = 17
)
result2 <- sam_detect(image = img, text = "well pad")

# 8. Check scores
sam_scores(result)

# 9. Select specific detections
top3 <- sam_select(result, 1:3)
sam_as_sf(top3)


sam_image("~/Downloads/mavs.jpeg", "basketball")

library(geosam)

sam_explore_image("~/Downloads/mavs.jpeg")

library(geosam)

pads <- sam_detect(
  bbox = c(-103.625120, 32.070631, -103.298105, 32.211333),
  text = "white or tan cleared area",
  source = "mapbox",
  zoom = 16
)

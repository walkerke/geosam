# Batch Process Multiple Areas

Runs SAM3 detection on multiple areas (polygons) and returns combined
results.

## Usage

``` r
sam_batch(
  areas,
  text,
  source = "mapbox",
  zoom = 17,
  threshold = 0.5,
  min_area = NULL,
  max_area = NULL,
  .progress = TRUE
)
```

## Arguments

- areas:

  An sf object with polygon geometries defining areas to process.

- text:

  Text prompt for detection.

- source:

  Imagery source: "mapbox", "esri", or "maptiler".

- zoom:

  Tile zoom level for imagery download.

- threshold:

  Detection confidence threshold.

- min_area:

  Minimum object area in square meters.

- max_area:

  Maximum object area in square meters.

- .progress:

  Logical. If TRUE, shows a progress bar.

## Value

An sf data frame with detection results. Includes an `area_id` column
linking detections to input areas.

## Examples

``` r
if (FALSE) { # \dontrun{
library(tigris)

# Process multiple counties
counties <- counties("TX") |>
  filter(NAME %in% c("Midland", "Ector"))

pads <- sam_batch(
  areas = counties,
  text = "swimming pool",
  min_area = 500
)
} # }
```

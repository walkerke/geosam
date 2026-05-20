# Download Satellite Imagery

Downloads satellite imagery tiles for a bounding box and saves as a
GeoTIFF. Uses R-native tools (httr2, terra) for reliable tile fetching
and georeferencing.

## Usage

``` r
get_imagery(
  bbox,
  output = NULL,
  source = c("mapbox", "esri", "maptiler"),
  zoom = 17,
  api_key = NULL
)
```

## Arguments

- bbox:

  Bounding box for the area. Can be a numeric vector
  `c(xmin, ymin, xmax, ymax)` in WGS84, or an sf/sfc object.

- output:

  Path for output GeoTIFF. If NULL, creates a temp file.

- source:

  Imagery source: "mapbox", "esri", or "maptiler".

  - "mapbox": Requires `MAPBOX_PUBLIC_TOKEN` environment variable

  - "esri": Free Esri World Imagery (no API key required)

  - "maptiler": Requires `MAPTILER_API_KEY` environment variable

- zoom:

  Tile zoom level (15-19). Higher values = more detail. Recommended:
  17-18 for objects like buildings, swimming pools.

- api_key:

  API key for the imagery source. For Mapbox, uses MAPBOX_PUBLIC_TOKEN
  environment variable by default. For MapTiler, uses MAPTILER_API_KEY
  environment variable by default.

## Value

Path to the downloaded GeoTIFF file.

## Examples

``` r
if (FALSE) { # \dontrun{
# Download imagery for an area
img <- get_imagery(
  bbox = c(-102.5, 31.8, -102.4, 31.9),
  source = "mapbox",
  zoom = 17
)

# Use with detection
pads <- sam_detect(image = img, text = "swimming pool")
} # }
```

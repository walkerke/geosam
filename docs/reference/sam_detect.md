# Detect Objects in Geospatial Imagery Using SAM3

Main function for object detection using Meta's SAM3 model. Supports
text prompts, point prompts, box prompts, and exemplar-based detection.

## Usage

``` r
sam_detect(
  image = NULL,
  bbox = NULL,
  text = NULL,
  boxes = NULL,
  points = NULL,
  labels = NULL,
  exemplar = NULL,
  source = "mapbox",
  zoom = 17,
  threshold = 0.5,
  chunked = NULL,
  chunk_size = NULL,
  chunk_overlap = NULL,
  min_area = NULL,
  max_area = NULL
)
```

## Arguments

- image:

  Path to a GeoTIFF image, or NULL to download imagery for bbox. Large
  images (\>2000 pixels on longest dimension) are automatically
  processed in chunks when using text prompts.

- bbox:

  Bounding box for the area of interest. Can be a numeric vector
  `c(xmin, ymin, xmax, ymax)` in WGS84, or an sf/sfc object.

- text:

  Text prompt describing objects to detect (e.g., "swimming pool",
  "swimming pool", "solar panel"). Uses SAM3's open-vocabulary
  detection.

- boxes:

  sf object with polygons/boxes to use as box prompts.

- points:

  sf object with points to use as point prompts.

- labels:

  Integer vector of labels for point prompts (1 = foreground, 0 =
  background). If NULL, all points are treated as foreground.

- exemplar:

  sf polygon representing an example object. SAM3 will find all similar
  objects in the image.

- source:

  Imagery source if downloading: "mapbox", "esri", or "maptiler".

- zoom:

  Tile zoom level for imagery download (17-19 recommended).

- threshold:

  Detection confidence threshold (0-1). Lower values return more
  detections.

- chunked:

  Control chunking for large areas/images:

  - `NULL` (default): Auto-chunk when image \>2000px or bbox requires
    multiple tiles

  - `TRUE`: Force chunked detection

  - `FALSE`: Disable chunking (may cause memory issues for large images)

- chunk_size:

  Target chunk size in pixels when chunking. Defaults to an internal
  value tuned for SAM3.

- chunk_overlap:

  Overlap in pixels when chunking. Defaults to an internal value that
  reduces tile-boundary artifacts.

- min_area:

  Minimum object area in square meters. Objects smaller than this are
  filtered out. For chunked detection, filtering happens during
  processing (better performance). Default NULL (no minimum).

- max_area:

  Maximum object area in square meters. Objects larger than this are
  filtered out. Default NULL (no maximum).

## Value

A `geosam` object containing detection masks and metadata. Use
[`sam_as_sf()`](https://walker-data.com/geosam/reference/sam_as_sf.md)
to extract polygons,
[`sam_filter()`](https://walker-data.com/geosam/reference/sam_filter.md)
to filter by area/score. Returns NULL if no objects are detected.

## Details

For large areas or images, detection is automatically chunked to
maintain accuracy and avoid memory issues. This means you can pass a
large bounding box (e.g., a census tract) or a large GeoTIFF file (e.g.,
an orthoimage) and get reliable detection without running out of memory.

## Examples

``` r
if (FALSE) { # \dontrun{
# Text-based detection
result <- sam_detect(
  bbox = c(-102.5, 31.8, -102.4, 31.9),
  text = "swimming pool"
)
pads <- result |> sam_filter(min_area = 500) |> sam_as_sf()

# Large area - automatically chunked
result <- sam_detect(
  bbox = c(-118.45, 34.08, -118.40, 34.12),  # ~5km area
  text = "swimming pool",
  zoom = 18
)

# Large user-provided image - automatically chunked
result <- sam_detect(
  image = "large_orthoimage.tif",
  text = "trees",
  threshold = 0.3
)

# Point prompts on existing image
result <- sam_detect(
  image = "satellite.tif",
  points = my_points_sf
)

# Box prompts
result <- sam_detect(
  image = "satellite.tif",
  boxes = my_boxes_sf
)

# Exemplar-based detection
result <- sam_detect(
  image = "satellite.tif",
  exemplar = my_example_polygon
)
} # }
```

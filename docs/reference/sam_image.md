# Detect Objects in a Local Image

Run SAM3 object detection on a non-georeferenced image (PNG, JPG, etc.).
For georeferenced satellite imagery, use
[`sam_detect()`](https://walker-data.com/geosam/reference/sam_detect.md)
instead.

## Usage

``` r
sam_image(
  image,
  text = NULL,
  points = NULL,
  labels = NULL,
  boxes = NULL,
  exemplar = NULL,
  threshold = 0.5
)
```

## Arguments

- image:

  Path to image file (PNG, JPG, TIFF, etc.).

- text:

  Text prompt describing objects to detect (e.g., "car", "person").

- points:

  Matrix or data.frame of point coordinates in pixels (x, y). Column 1
  is x (horizontal), column 2 is y (vertical, from top).

- labels:

  Integer vector of labels for point prompts (1 = foreground, 0 =
  background). If NULL, all points are treated as foreground.

- boxes:

  Matrix or data.frame of box coordinates in pixels. Each row: (xmin,
  ymin, xmax, ymax).

- exemplar:

  Vector of box coordinates for an exemplar (xmin, ymin, xmax, ymax).

- threshold:

  Detection confidence threshold (0-1).

## Value

A `geosam_image` object containing detection masks and metadata. Use
[`sam_as_sf()`](https://walker-data.com/geosam/reference/sam_as_sf.md)
to extract polygons (in pixel coordinates). Returns NULL if no objects
are detected.

## Examples

``` r
if (FALSE) { # \dontrun{
# Text-based detection on a photo
result <- sam_image("photo.jpg", text = "dog")

# Point prompts (click locations in pixels)
result <- sam_image("photo.jpg", points = matrix(c(100, 200, 150, 250), ncol = 2, byrow = TRUE))

# Extract polygons
polys <- sam_as_sf(result)
} # }
```

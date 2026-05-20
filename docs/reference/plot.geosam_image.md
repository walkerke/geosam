# Plot geosam_image Detection Results

Displays an image with detection masks overlaid using magick.

## Usage

``` r
# S3 method for class 'geosam_image'
plot(
  x,
  fill = "#FACC15",
  border = NULL,
  palette = NULL,
  opacity = 0.4,
  border_width = 2,
  add = FALSE,
  base_img = NULL,
  ...
)
```

## Arguments

- x:

  A geosam_image object from
  [`sam_image()`](https://walker-data.com/geosam/reference/sam_image.md)
  or
  [`sam_explore_image()`](https://walker-data.com/geosam/reference/sam_explore_image.md).

- fill:

  Fill color for detection masks. Default is yellow. Ignored if
  `palette` is specified.

- border:

  Border color for detection outlines. If NULL (default), automatically
  darkens the fill color. Set to NA to disable borders.

- palette:

  Optional color palette for distinct colors per detection. Can be:

  - A character vector of colors (recycled if needed)

  - A palette name from RColorBrewer (e.g., "Set1", "Dark2", "Paired")

  - A palette function that takes n and returns colors

- opacity:

  Opacity of the mask overlay (0-1). Default is 0.4.

- border_width:

  Width of border lines in pixels. Default is 2.

- add:

  If TRUE, add to an existing magick image instead of loading from file.
  Pass the image as the `x` parameter's `image_path` will be ignored and
  `base_img` must be provided.

- base_img:

  A magick image to add overlays to. Required when `add = TRUE`.

- ...:

  Additional arguments (currently ignored).

## Value

Invisibly returns the composite magick image, which can be saved with
[`magick::image_write()`](https://docs.ropensci.org/magick/reference/editing.html)
or further manipulated.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- sam_image("photo.jpg", text = "dog")
plot(result)

# Distinct colors per detection
plot(result, palette = "Set2")

# Custom color (border auto-matches)
plot(result, fill = "red")

# Layer multiple detections
img <- plot(result1)
img <- plot(result2, add = TRUE, base_img = img, fill = "blue")
img <- plot(result3, add = TRUE, base_img = img, fill = "red")

# Save the result
magick::image_write(img, "annotated.png")
} # }
```

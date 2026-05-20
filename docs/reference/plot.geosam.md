# Plot geosam Detection Results

Displays satellite imagery with detection polygons overlaid using terra.

## Usage

``` r
# S3 method for class 'geosam'
plot(
  x,
  fill = "#FACC15",
  border = NULL,
  palette = NULL,
  opacity = 0.5,
  lwd = 1,
  main = NULL,
  add = FALSE,
  ...
)
```

## Arguments

- x:

  A geosam object from
  [`sam_detect()`](https://walker-data.com/geosam/reference/sam_detect.md)
  or
  [`sam_explore()`](https://walker-data.com/geosam/reference/sam_explore.md).

- fill:

  Fill color for detection polygons. Default is semi-transparent yellow.
  Ignored if `palette` is specified.

- border:

  Border color for polygon outlines. If NULL (default), automatically
  darkens the fill color. Set to NA to disable borders.

- palette:

  Optional color palette for distinct colors per detection. Can be:

  - A character vector of colors (recycled if needed)

  - A palette name from RColorBrewer (e.g., "Set1", "Dark2", "Paired")

  - A palette function that takes n and returns colors

- opacity:

  Opacity for fill colors (0-1). Default is 0.5. Applied to all fills.

- lwd:

  Line width for polygon borders. Default is 1.

- main:

  Plot title. Default is NULL (no title).

- add:

  If TRUE, add to existing plot instead of creating new one. Default is
  FALSE.

- ...:

  Additional arguments passed to
  [`terra::plotRGB()`](https://rspatial.github.io/terra/reference/plotRGB.html)
  when `add = FALSE`.

## Value

Invisibly returns the sf polygons that were plotted.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- sam_detect(
  bbox = c(-102.63, 31.81, -102.62, 31.83),
  text = "swimming pool"
)
plot(result)

# Distinct colors per detection
plot(result, palette = "Set1")

# Custom colors
plot(result, fill = "red")  # border auto-darkens

# Layer multiple results
plot(result1)
plot(result2, add = TRUE, fill = "blue")
} # }
```

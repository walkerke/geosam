# Interactive Image Exploration and Detection

Opens an interactive viewer to explore an image and run SAM detection
using text prompts, point clicks, or drawn boxes.

## Usage

``` r
sam_explore_image(image)
```

## Arguments

- image:

  Path to image file (PNG, JPG, etc.). Required.

## Value

A `geosam_image` object when the user clicks "Done", or NULL if
cancelled.

## Details

The explorer provides a complete workflow:

1.  View and pan/zoom the image

2.  Select a prompt type (text, box, or points)

3.  Enter text or draw/click prompts on the image

4.  Click "Detect" to run SAM

5.  View results

6.  Click "Done" to return the geosam_image object

The viewer uses MapLibre with a blank background and the image displayed
as a raster layer, enabling all standard map interactions.

## Examples

``` r
if (FALSE) { # \dontrun{
# Explore a photo
result <- sam_explore_image("photo.jpg")

# Extract detected regions
if (!is.null(result)) {
  polygons <- sam_as_sf(result)
}
} # }
```

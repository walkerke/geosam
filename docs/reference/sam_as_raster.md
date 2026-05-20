# Extract Raster Masks from geosam Object

Converts detection masks from a geosam object to a terra SpatRaster.

## Usage

``` r
sam_as_raster(x, combined = FALSE)
```

## Arguments

- x:

  A geosam object from
  [`sam_detect()`](https://walker-data.com/geosam/reference/sam_detect.md)
  or
  [`sam_explore()`](https://walker-data.com/geosam/reference/sam_explore.md).

- combined:

  If TRUE, combine all masks into a single layer (union). If FALSE
  (default), return a multi-layer raster with one layer per detection.

## Value

A terra SpatRaster with mask values (1 = detected, 0 = background).
Returns NULL if no masks.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- sam_detect(image = "satellite.tif", text = "building")
mask_raster <- sam_as_raster(result, combined = TRUE)
terra::plot(mask_raster)
} # }
```

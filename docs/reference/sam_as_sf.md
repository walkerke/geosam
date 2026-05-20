# Extract sf Polygons from geosam or geosam_image Object

Converts detection masks to sf polygon geometries.

## Usage

``` r
sam_as_sf(x, min_area = NULL, max_area = NULL)
```

## Arguments

- x:

  A geosam or geosam_image object.

- min_area:

  Minimum object area. For geosam, in square meters. For geosam_image,
  in square pixels. Objects smaller are filtered out.

- max_area:

  Maximum object area. For geosam, in square meters. For geosam_image,
  in square pixels. Objects larger are filtered out.

## Value

An sf data frame with polygon geometries, scores, and area. Returns NULL
if no polygons remain after filtering.

## Details

For `geosam` objects (georeferenced imagery), returns polygons in WGS84
coordinates with area in square meters.

For `geosam_image` objects (non-georeferenced images), returns polygons
in pixel coordinates where x = column (from left) and y = row (from
top).

## Examples

``` r
if (FALSE) { # \dontrun{
# Georeferenced imagery
result <- sam_detect(image = "satellite.tif", text = "building")
buildings <- sam_as_sf(result, min_area = 100)

# Non-georeferenced image
result <- sam_image("photo.jpg", text = "dog")
dogs <- sam_as_sf(result)  # coordinates in pixels
} # }
```

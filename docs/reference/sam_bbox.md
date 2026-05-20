# Get Bounding Box of Detections

Returns the bounding box encompassing all detected objects.

## Usage

``` r
sam_bbox(x)
```

## Arguments

- x:

  A geosam object from
  [`sam_detect()`](https://walker-data.com/geosam/reference/sam_detect.md)
  or
  [`sam_explore()`](https://walker-data.com/geosam/reference/sam_explore.md).

## Value

An sf bbox object, or NULL if no detections.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- sam_detect(image = "satellite.tif", text = "building")
bbox <- sam_bbox(result)
} # }
```

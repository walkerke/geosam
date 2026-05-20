# Filter Detections by Area or Score

Filters detections in a geosam object based on area and/or confidence
score.

## Usage

``` r
sam_filter(x, min_area = NULL, max_area = NULL, min_score = NULL)
```

## Arguments

- x:

  A geosam object.

- min_area:

  Minimum area in square meters.

- max_area:

  Maximum area in square meters.

- min_score:

  Minimum confidence score (0-1).

## Value

A new geosam object with filtered detections.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- sam_detect(image = "satellite.tif", text = "building")
filtered <- result |> sam_filter(min_area = 100, min_score = 0.7)
} # }
```

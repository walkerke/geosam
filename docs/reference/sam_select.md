# Select Specific Detections by Index

Subsets a geosam object to include only specific detections.

## Usage

``` r
sam_select(x, index)
```

## Arguments

- x:

  A geosam object.

- index:

  Integer vector of indices to keep.

## Value

A new geosam object with only the selected detections.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- sam_detect(image = "satellite.tif", text = "building")
# Keep only the top 3 scoring detections
top3 <- sam_select(result, order(sam_scores(result), decreasing = TRUE)[1:3])
} # }
```

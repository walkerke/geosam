# Find Similar Objects Using Selected Detection as Exemplar

Uses a single selected detection as an exemplar to find all similar
objects in the image.

## Usage

``` r
sam_find_similar(x)
```

## Arguments

- x:

  A geosam object with exactly one detection (use
  [`sam_select()`](https://walker-data.com/geosam/reference/sam_select.md)
  first).

## Value

A new geosam object with all detected similar objects.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- sam_detect(image = "satellite.tif", text = "swimming pool")
# View results, pick the best one, find all similar
similar <- result |>
  sam_select(3) |>
  sam_find_similar()
} # }
```

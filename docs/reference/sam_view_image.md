# View Detection Results on Image

Opens an interactive viewer to display detection results overlaid on the
source image.

## Usage

``` r
sam_view_image(x)
```

## Arguments

- x:

  A `geosam_image` object from
  [`sam_image()`](https://walker-data.com/geosam/reference/sam_image.md)
  or
  [`sam_explore_image()`](https://walker-data.com/geosam/reference/sam_explore_image.md).

## Value

The `geosam_image` object when the user clicks "Done".

## Examples

``` r
if (FALSE) { # \dontrun{
result <- sam_image("photo.jpg", text = "dog")
sam_view_image(result)
} # }
```

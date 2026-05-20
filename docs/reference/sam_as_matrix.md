# Extract Masks as Matrices

Returns detection masks as a list of R matrices.

## Usage

``` r
sam_as_matrix(x)
```

## Arguments

- x:

  A geosam or geosam_image object.

## Value

A list of binary matrices (1 = detected, 0 = background).

## Examples

``` r
if (FALSE) { # \dontrun{
result <- sam_image("photo.jpg", text = "dog")
masks <- sam_as_matrix(result)
image(masks[[1]])  # Display first mask
} # }
```

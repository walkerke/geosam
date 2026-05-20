# Check geosam Installation Status

Checks the Python environment and reports which features are available.

## Usage

``` r
geosam_status(check_access = TRUE)
```

## Arguments

- check_access:

  Logical. If TRUE, checks whether the configured HuggingFace token can
  access the gated `facebook/sam3` model metadata. This does not
  download model weights.

## Value

A list with installation status information, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
geosam_status()
} # }
```

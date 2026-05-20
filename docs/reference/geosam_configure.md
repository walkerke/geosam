# Configure geosam Options

Set package-wide configuration options for model selection and device.

## Usage

``` r
geosam_configure(model = NULL, device = NULL, mapbox_token = NULL)
```

## Arguments

- model:

  Model to use: "sam2" (default, latest) or "sam" (original).

- device:

  Computing device: "auto" (default), "mps", "cuda", or "cpu". "auto"
  selects the best available device.

- mapbox_token:

  Mapbox access token for satellite imagery. Can also be set via the
  MAPBOX_ACCESS_TOKEN environment variable.

## Value

Invisibly returns the previous configuration.

## Examples

``` r
if (FALSE) { # \dontrun{
# Use SAM 2 on CPU
geosam_configure(model = "sam2", device = "cpu")

# Set Mapbox token
geosam_configure(mapbox_token = "pk.xxx")
} # }
```

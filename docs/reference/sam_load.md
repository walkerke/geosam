# Load SAM3 Model

Loads the SAM3 model into memory. Useful for interactive use or Shiny
apps where you want to keep the model warm between calls.

## Usage

``` r
sam_load(device = "auto")
```

## Arguments

- device:

  Device to load model on: "auto", "mps", "cuda", or "cpu".

## Value

Invisibly returns TRUE on success.

## Examples

``` r
if (FALSE) { # \dontrun{
# Load model before making multiple detections
sam_load()

# Now detections are faster
result1 <- sam_detect(image = "img1.tif", text = "building")
result2 <- sam_detect(image = "img2.tif", text = "building")

# Unload when done
sam_unload()
} # }
```

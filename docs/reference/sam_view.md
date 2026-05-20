# Interactive Viewer for SAM Detections

Opens an interactive map viewer to view SAM detections.

## Usage

``` r
sam_view(
  x,
  fill = "#FACC15",
  border = "#EAB308",
  fill_opacity = 0.5,
  source = NULL
)
```

## Arguments

- x:

  A geosam object from
  [`sam_detect()`](https://walker-data.com/geosam/reference/sam_detect.md).

- fill:

  Fill color for detection polygons. Default is "#FACC15" (yellow).

- border:

  Border/outline color for polygons. Default is "#EAB308".

- fill_opacity:

  Fill opacity for detection polygons (0-1). Default is 0.5.

- source:

  Imagery source for basemap: "mapbox", "esri", or "maptiler". If NULL
  (default), uses the source from the geosam object if available,
  otherwise falls back to "mapbox" or "esri" if no API key is set.

## Value

The geosam object when the user clicks "Done".

## Details

The viewer shows the satellite imagery with current detections overlaid.

Note: Point-based refinement is not yet fully implemented. The current
version is view-only.

## Examples

``` r
if (FALSE) { # \dontrun{
result <- sam_detect(image = "satellite.tif", text = "building")
refined <- sam_view(result)

# Custom colors
sam_view(result, fill = "#3B82F6", border = "#1D4ED8")
} # }
```

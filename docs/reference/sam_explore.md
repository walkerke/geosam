# Interactive Discovery and Detection

Opens an interactive map to explore satellite imagery, draw prompts, and
run SAM detection.

## Usage

``` r
sam_explore(
  source = c("mapbox", "esri", "maptiler"),
  center = NULL,
  bbox = NULL,
  zoom = 15,
  units = c("metric", "imperial"),
  quality = c("balanced", "fast", "accurate"),
  ...
)
```

## Arguments

- source:

  Imagery source: "mapbox", "esri", or "maptiler".

  - "mapbox": Requires `MAPBOX_PUBLIC_TOKEN` environment variable

  - "esri": Free Esri World Imagery (no API key required)

  - "maptiler": Requires `MAPTILER_API_KEY` environment variable

- center:

  Initial map center as c(lng, lat). If NULL, defaults to US center.

- bbox:

  Initial bounding box as c(xmin, ymin, xmax, ymax) or sf object. If
  provided, map will zoom to this extent.

- zoom:

  Initial zoom level (default 15).

- units:

  Unit system for the scale bar and area display: `"metric"` (km/m) or
  `"imperial"` (mi/ft). Default is `"metric"`.

- quality:

  Initial detection quality preset: `"balanced"` (default, chunked with
  moderate overlap), `"fast"` (single-pass, no chunking — best for small
  areas), or `"accurate"` (chunked with smaller tiles and more overlap).
  Exposed in the UI under "Advanced".

- ...:

  Additional arguments passed to
  [`sam_detect()`](https://walker-data.com/geosam/reference/sam_detect.md).
  Useful for setting `min_area`, `max_area`, or other detection
  parameters.

## Value

A geosam object when the user clicks "Done", or NULL if cancelled.

## Details

The explorer provides a complete workflow:

1.  Navigate the map to find an area of interest

2.  Select a prompt type (text, box, points, or exemplar)

3.  Enter text or draw prompts on the map

4.  Click "Detect" to run SAM on the current viewport

5.  View results and optionally refine with +/- points

6.  Click "Done" to return the geosam object

## Examples

``` r
if (FALSE) { # \dontrun{
# Start exploring with Mapbox satellite
result <- sam_explore(source = "mapbox", center = c(-102.5, 31.8), zoom = 15)

# Extract results
if (!is.null(result)) {
  polygons <- sam_as_sf(result)
}
} # }
```

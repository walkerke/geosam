# Merge Polygons Split at Tile Boundaries

When detecting objects over large areas, geosam uses tiled processing
which can split objects that span tile boundaries into multiple
polygons. This function merges polygons that are close together, healing
those splits.

## Usage

``` r
sam_merge_edges(x, buffer = 2, by_prompt = TRUE)
```

## Arguments

- x:

  A geosam object or sf object with detection results.

- buffer:

  Distance in meters to buffer polygons before checking for overlap.
  Larger values merge polygons that are further apart. Default is 2.

- by_prompt:

  If TRUE and a `prompt` column exists, only merge polygons with the
  same prompt value. Default is TRUE.

## Value

An sf object with merged polygons. Scores are aggregated by taking the
maximum score from merged polygons.

## Details

This function is useful when you notice objects being split at regular
intervals (tile boundaries). The default buffer of 2 meters catches most
boundary splits without merging truly separate objects.

For aggressive merging of nearby objects (not just boundary splits), use
a larger buffer value, but be aware this may merge objects that should
remain separate.

## Examples

``` r
if (FALSE) { # \dontrun{
# Detect buildings over a large area (uses tiling internally)
buildings <- sam_detect(
  bbox = c(-118.5, 34.0, -118.4, 34.1),
  text = "building",
  zoom = 18
)

# Merge any buildings split at tile boundaries
merged <- sam_merge_edges(buildings)

# More aggressive merging (5m buffer)
merged <- sam_merge_edges(buildings, buffer = 5)
} # }
```

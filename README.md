# geosam
Geospatial Image Segmentation with Meta's SAM3 in R.

## Overview

geosam provides a native R interface to Meta's Segment Anything Model 3 (SAM3) for detecting objects in satellite imagery and photos. It supports:

- **Text prompts**: Describe what you're looking for ("swimming pool", "solar panel", "building")
- **Exemplar detection**: Draw a box around one example, find all similar objects
- **Point prompts**: Click positive/negative points to guide segmentation (interactive mode)

Works with both **georeferenced satellite imagery** (returns sf polygons in real-world coordinates) and **regular images** (returns pixel coordinates).

## Installation

```r
# Install from GitHub
remotes::install_github("walkerke/geosam")

# Install Python dependencies
library(geosam)
geosam_install()
```

You'll need a HuggingFace account with access to SAM3. Set your token:

```r
Sys.setenv(HF_TOKEN = "your_huggingface_token")
```

For satellite imagery, set your Mapbox token:

```r
Sys.setenv(MAPBOX_PUBLIC_TOKEN = "your_mapbox_token")
```

## Quick Start

### Satellite Imagery Detection

```r
library(geosam)

# Detect swimming pools in Beverly Hills
pools <- sam_detect(
  bbox = c(-118.42, 34.08, -118.40, 34.10),
  text = "swimming pool",
  source = "mapbox",
  zoom = 18
)

# View results interactively
sam_view(pools)

# Extract as sf polygons
pools_sf <- sam_as_sf(pools)
```

For large areas, detection is automatically tiled to maintain accuracy.

### Regular Image Detection

```r
# Detect objects in a photo
dogs <- sam_image("photo.jpg", text = "dog")

# View results
plot(dogs)

# Extract polygons (in pixel coordinates)
dogs_sf <- sam_as_sf(dogs)
```

### Interactive Discovery

Explore satellite imagery and detect interactively:

```r
# Opens interactive map viewer
result <- sam_explore(source = "mapbox")

# Navigate, enter text prompts, draw boxes or points...
# Returns geosam object when done

polygons <- sam_as_sf(result)
```

### Exemplar Workflow

Find all objects similar to one example:

```r
# In sam_explore(), use "Draw Example" mode to
# draw a box around one object - SAM3 finds all similar objects
result <- sam_explore(source = "mapbox")

# Or programmatically with an sf polygon as exemplar
similar <- sam_detect(image = "satellite.tif", exemplar = my_example_polygon)
```

## Key Functions

### Detection
- `sam_detect()` - Detect objects in georeferenced imagery with text/box/point/exemplar prompts
- `sam_image()` - Detect objects in regular images (photos, screenshots)
- `sam_explore()` - Interactive satellite imagery explorer with detection
- `sam_explore_image()` - Interactive image explorer with detection
- `get_imagery()` - Download satellite imagery tiles as GeoTIFF

### Viewing & Export
- `sam_view()` - Interactive map viewer with confidence filtering
- `sam_view_image()` - Interactive image viewer
- `plot()` - Static plotting for geosam objects
- `sam_as_sf()` - Extract sf polygons
- `sam_as_raster()` - Extract terra SpatRaster masks
- `sam_as_matrix()` - Extract raw mask matrices
- `sam_bbox()` - Get bounding box of detections
- `sam_scores()` - Get confidence scores
- `sam_count()` - Get number of detections

### Refinement
- `sam_filter()` - Filter by area or score
- `sam_select()` - Select specific detections by index
- `sam_find_similar()` - Use one detection as exemplar to find similar

### Batch Processing
- `sam_batch()` - Process multiple images or bounding boxes

### Configuration
- `geosam_install()` - Install Python dependencies
- `geosam_configure()` - Set API keys and preferences
- `geosam_status()` - Check installation status
- `sam_load()` / `sam_unload()` - Manage model in memory

## Imagery Sources

| Source | Interactive | Download | API Key Required |
|--------|-------------|----------|------------------|
| Mapbox | Yes | Yes | Yes (MAPBOX_PUBLIC_TOKEN) |
| Esri | Yes | Yes | No |
| MapTiler | Yes | Yes | Yes (MAPTILER_API_KEY) |

## Requirements

- R >= 4.1
- Python >= 3.12
- HuggingFace account with SAM3 access
- For interactive features: shiny, mapgl packages
- For Mapbox imagery: MAPBOX_PUBLIC_TOKEN environment variable

## License

MIT

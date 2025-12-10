# geosam

Geospatial Image Segmentation with Meta's SAM3 in R.

## Overview

geosam provides a native R interface to Meta's Segment Anything Model 3 (SAM3) for detecting objects in satellite imagery. It supports:

- **Text prompts**: Describe what you're looking for ("building", "well pad", "solar panel")
- **Box prompts**: Draw bounding boxes around areas of interest
- **Point prompts**: Click positive and negative points to guide segmentation
- **Exemplar detection**: Draw a box around one example, find all similar objects

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

## Workflows

### Programmatic Detection

For scripted/batch workflows:
```r
library(geosam)

# Download satellite imagery
img <- get_imagery(
  bbox = c(-102.5, 31.8, -102.4, 31.9),
  source = "mapbox",
  zoom = 17
)

# Detect objects with text prompt
result <- sam_detect(img, text = "well pad")

# Filter and extract as sf
wells <- result |>
  sam_filter(min_area = 500, min_score = 0.7) |>
  sam_as_sf()
```

### Interactive Discovery

Explore satellite imagery and detect interactively:
```r
# Opens interactive map viewer
result <- sam_explore(source = "mapbox", center = c(-102.5, 31.8), zoom = 15)

# Navigate, draw prompts, run detection, refine...
# Returns geosam object when done

polygons <- sam_as_sf(result)
```

### Interactive Refinement

Start programmatically, refine interactively:
```r
# Run initial detection
result <- sam_detect(img, text = "building")

# Open viewer to add +/- refinement points
refined <- sam_view(result)

# Extract final polygons
buildings <- sam_as_sf(refined)
```

### Exemplar Workflow

Find all objects similar to one example:
```r
# Detect some candidates
result <- sam_detect(img, text = "cleared area")

# View results, select the best one, find all similar
similar <- result |>
  sam_select(3) |>
  sam_find_similar()

# Extract
all_pads <- sam_as_sf(similar)
```

## Key Functions

### Entry Points
- `sam_detect()` - Programmatic detection with text/box/point/exemplar prompts
- `sam_explore()` - Interactive discovery from satellite map
- `get_imagery()` - Download satellite imagery tiles

### Extraction
- `sam_as_sf()` - Extract sf polygons from geosam object
- `sam_as_raster()` - Extract terra SpatRaster masks
- `sam_bbox()` - Get bounding box of detections
- `sam_scores()` - Get confidence scores
- `sam_count()` - Get number of detections

### Refinement
- `sam_filter()` - Filter by area or score
- `sam_select()` - Select specific detections by index
- `sam_refine()` - Add refinement points programmatically
- `sam_find_similar()` - Use one detection as exemplar to find similar
- `sam_view()` - Interactive refinement viewer

### Configuration
- `geosam_install()` - Install Python dependencies
- `geosam_configure()` - Set API keys and preferences
- `geosam_status()` - Check installation status
- `sam_load()` / `sam_unload()` - Manage model in memory

## Imagery Sources

| Source | Interactive | Download | API Key Required |
|--------|-------------|----------|------------------|
| Mapbox | Yes | Yes | Yes (MAPBOX_PUBLIC_TOKEN) |
| ESRI | Yes | Yes | No |
| MapTiler | Yes | Yes | Yes |
| Google | No | Yes | No |

## Requirements

- R >= 4.0
- Python >= 3.12
- HuggingFace account with SAM3 access
- For interactive features: shiny, mapgl packages

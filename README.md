# geosam

Geospatial Image Segmentation with Meta's SAM3 in R.

## Overview

geosam provides a native R interface to Meta's Segment Anything Model 3 (SAM3) for detecting objects in satellite imagery and photos. Unlike traditional computer vision, SAM3 lets you describe what you're looking for in plain text—no training data or model fine-tuning required.

- **Text prompts**: Describe what you're looking for ("swimming pool", "building", "solar panel")
- **Exemplar detection**: Draw a box around one example, find all similar objects
- **Point prompts**: Click positive/negative points to guide segmentation
- **Interactive tools**: Shiny-powered viewers for exploration and refinement

Works with both **georeferenced satellite imagery** (returns sf polygons in real-world coordinates) and **regular images** (returns pixel coordinates).

## Installation

```r
# Install from GitHub
remotes::install_github("walkerke/geosam")

# Install Python dependencies
library(geosam)
geosam_install()
```

You'll need a HuggingFace account with access to SAM3. Visit the [SAM3 model page](https://huggingface.co/facebook/sam3) to request access, then set your token:

```r
Sys.setenv(HF_TOKEN = "your_huggingface_token")
```

For satellite imagery with Mapbox:

```r
Sys.setenv(MAPBOX_PUBLIC_TOKEN = "your_mapbox_token")
```

Esri World Imagery works without an API key.

## Quick Start

### Satellite Imagery

Detect swimming pools in Beverly Hills:

```r
library(geosam)

pools <- sam_detect(
  bbox = c(-118.41, 34.09, -118.405, 34.095),
  text = "swimming pool",
  source = "mapbox",
  zoom = 18
)

# View results
plot(pools)

# Interactive viewer with confidence slider
sam_view(pools)

# Extract as sf polygons
pools_sf <- sam_as_sf(pools)
```

### Bring Your Own Imagery

geosam includes a sample WorldView-3 chip from Mumbai (SpaceNet dataset):

```r
mumbai <- system.file("extdata", "mumbai_chip.tif", package = "geosam")

buildings <- sam_detect(image = mumbai, text = "building")
plot(buildings)

# Try different prompts
roads <- sam_detect(image = mumbai, text = "road")
```

### Regular Images

Detect objects in photos using `sam_image()`:

```r
# Sample fruit plate image included in package
fruit <- system.file("extdata", "fruit_plate.jpg", package = "geosam")

berries <- sam_image(fruit, text = "strawberry")
plot(berries)

# Multiple prompts, combine results
cherries <- sam_image(fruit, text = "cherry")
p <- plot(cherries)
plot(sam_image(fruit, text = "chocolate"), add = TRUE, base_img = p, fill = "purple")
```

### Interactive Exploration

Explore satellite imagery and detect interactively:

```r
result <- sam_explore(source = "mapbox")

# Navigate the map, enter text prompts, draw boxes, click points...
# Returns geosam object when you click "Done"

polygons <- sam_as_sf(result)
```

For regular images:

```r
result <- sam_explore_image("photo.jpg")
```

## Key Functions

| Function | Description |
|----------|-------------|
| `sam_detect()` | Detect objects in satellite imagery |
| `sam_image()` | Detect objects in regular images |
| `sam_explore()` | Interactive satellite imagery explorer |
| `sam_explore_image()` | Interactive image explorer |
| `sam_view()` | Interactive viewer for detection results |
| `sam_as_sf()` | Extract sf polygons |
| `sam_filter()` | Filter by area or confidence score |
| `plot()` | Static plotting for geosam objects |

## Imagery Sources

| Source | API Key Required | Notes |
|--------|------------------|-------|
| Mapbox | Yes (`MAPBOX_PUBLIC_TOKEN`) | High-quality imagery |
| Esri | No | Good default option |
| MapTiler | Yes (`MAPTILER_API_KEY`) | Alternative provider |

## Requirements

- R >= 4.1
- Python >= 3.12
- HuggingFace account with SAM3 access
- For interactive features: shiny, mapgl packages

## License

MIT

# Package index

## Detection

Core detection functions

- [`sam_detect()`](https://walker-data.com/geosam/reference/sam_detect.md)
  : Detect Objects in Geospatial Imagery Using SAM3
- [`sam_image()`](https://walker-data.com/geosam/reference/sam_image.md)
  : Detect Objects in a Local Image
- [`sam_explore()`](https://walker-data.com/geosam/reference/sam_explore.md)
  : Interactive Discovery and Detection
- [`sam_explore_image()`](https://walker-data.com/geosam/reference/sam_explore_image.md)
  : Interactive Image Exploration and Detection
- [`get_imagery()`](https://walker-data.com/geosam/reference/get_imagery.md)
  : Download Satellite Imagery

## Viewing & Export

View results and extract data

- [`sam_view()`](https://walker-data.com/geosam/reference/sam_view.md) :
  Interactive Viewer for SAM Detections
- [`sam_view_image()`](https://walker-data.com/geosam/reference/sam_view_image.md)
  : View Detection Results on Image
- [`plot(`*`<geosam>`*`)`](https://walker-data.com/geosam/reference/plot.geosam.md)
  : Plot geosam Detection Results
- [`plot(`*`<geosam_image>`*`)`](https://walker-data.com/geosam/reference/plot.geosam_image.md)
  : Plot geosam_image Detection Results
- [`sam_as_sf()`](https://walker-data.com/geosam/reference/sam_as_sf.md)
  : Extract sf Polygons from geosam or geosam_image Object
- [`sam_as_raster()`](https://walker-data.com/geosam/reference/sam_as_raster.md)
  : Extract Raster Masks from geosam Object
- [`sam_as_matrix()`](https://walker-data.com/geosam/reference/sam_as_matrix.md)
  : Extract Masks as Matrices
- [`sam_bbox()`](https://walker-data.com/geosam/reference/sam_bbox.md) :
  Get Bounding Box of Detections
- [`sam_scores()`](https://walker-data.com/geosam/reference/sam_scores.md)
  : Get Detection Scores
- [`sam_count()`](https://walker-data.com/geosam/reference/sam_count.md)
  : Get Number of Detections

## Refinement

Filter and refine detections

- [`sam_filter()`](https://walker-data.com/geosam/reference/sam_filter.md)
  : Filter Detections by Area or Score
- [`sam_select()`](https://walker-data.com/geosam/reference/sam_select.md)
  : Select Specific Detections by Index
- [`sam_find_similar()`](https://walker-data.com/geosam/reference/sam_find_similar.md)
  : Find Similar Objects Using Selected Detection as Exemplar
- [`sam_merge_edges()`](https://walker-data.com/geosam/reference/sam_merge_edges.md)
  : Merge Polygons Split at Tile Boundaries

## Batch Processing

Process multiple inputs

- [`sam_batch()`](https://walker-data.com/geosam/reference/sam_batch.md)
  : Batch Process Multiple Areas

## Configuration

Setup and configuration

- [`geosam_install()`](https://walker-data.com/geosam/reference/geosam_install.md)
  : Install Python Dependencies for geosam
- [`geosam_configure()`](https://walker-data.com/geosam/reference/geosam_configure.md)
  : Configure geosam Options
- [`geosam_status()`](https://walker-data.com/geosam/reference/geosam_status.md)
  : Check geosam Installation Status
- [`geosam_diagnose()`](https://walker-data.com/geosam/reference/geosam_diagnose.md)
  : Diagnose geosam Installation Issues
- [`sam_load()`](https://walker-data.com/geosam/reference/sam_load.md) :
  Load SAM3 Model
- [`sam_unload()`](https://walker-data.com/geosam/reference/sam_unload.md)
  : Unload SAM3 Model
- [`sam_is_loaded()`](https://walker-data.com/geosam/reference/sam_is_loaded.md)
  : Check if Model is Loaded
- [`geosam_clear_cache()`](https://walker-data.com/geosam/reference/geosam_clear_cache.md)
  : Clear Imagery Cache

## Class Helpers

Object validation and methods

- [`is_geosam()`](https://walker-data.com/geosam/reference/is_geosam.md)
  : Check if Object is a geosam
- [`is_geosam_image()`](https://walker-data.com/geosam/reference/is_geosam_image.md)
  : Check if Object is a geosam_image
- [`print(`*`<geosam>`*`)`](https://walker-data.com/geosam/reference/print.geosam.md)
  : Print Method for geosam Objects
- [`print(`*`<geosam_image>`*`)`](https://walker-data.com/geosam/reference/print.geosam_image.md)
  : Print Method for geosam_image Objects

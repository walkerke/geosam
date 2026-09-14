# geosam 0.1.2 (development)

## Bug fixes

* Fetched imagery mosaics could be vertically mirrored on some installs,
  causing every detection from `sam_detect()` and `sam_explore()` to land
  in the wrong place (flipped north/south within the fetched extent). The
  tile stitcher previously flipped the mosaic and round-tripped it through
  an ungeoreferenced PNG, relying on terra/GDAL flipping the rows back on
  read; recent terra/GDAL builds no longer do so. The GeoTIFF is now
  written directly from the stitched pixel data, one tile row at a time via
  terra's block writer, which is independent of file-reader behaviour and
  keeps peak memory well below that of materialising the full mosaic.

## Chunked detection quality

* Chunked detection now suppresses overlapping duplicate masks via
  score-ranked non-maximum suppression (IoU + containment tests) before
  boundary healing, then trims any residual partial overlaps so output
  polygons never overlap. SAM3 frequently emits nested partial masks and
  offset duplicates for the same object; these previously either stacked
  on top of each other or were welded into blobs by boundary merging. The
  new exported `sam_dedupe()` applies the same suppression (and optional
  trimming) to any result, e.g. unchunked detections.
* Boundary healing after chunked detection is now seam-aware: polygons are
  only merged when they straddle a chunk-core boundary and look like
  fragments of a single split object. Previously any detections within 1m
  of each other were unioned, which welded together distinct adjacent
  objects (e.g. truck trailers parked side by side). `sam_merge_edges()`
  gains a `seam_aware` argument (default `TRUE`) and uses seam lines
  stored on chunked results; `seam_aware = FALSE` restores proximity-only
  merging.

## Interactive performance

* `sam_explore()` gains a **Detection Quality** control (Fast / Balanced /
  Accurate) tucked under an "Advanced" disclosure. The default is `"balanced"`
  (chunked with moderate overlap) and can be overridden with the new
  `quality` argument. Fast mode skips chunking entirely for a more responsive
  feel on small areas; Accurate uses smaller chunks with more overlap.
* Esri satellite view in `sam_explore()` and `sam_view()` reorders the
  OpenFreeMap overlay so road geometry sits beneath the satellite and only
  symbol/label layers ride above. Road name labels are restyled (light text,
  dark halo) for legibility on dark imagery.
* When Fast mode runs multiple text prompts on the same view, vision
  embeddings are computed once and reused across prompts (via a new
  `detect_text_multi()` Python helper modeled on HuggingFace's
  multi-prompt SAM3 pattern).
* A text-embedding cache (LRU bounded to 64 entries) is available for
  experimental opt-in reuse across `detect_text()` calls with the same
  prompt. Disabled by default — the cached path passes `pooler_output`
  where the model documents a different shape and may produce different
  detections than the combined-processor call. The chunked detection path
  remains on the unchanged combined-processor code path.

## Chunked detection

* Chunked detection now overlaps neighboring chunks and keeps detections
  whose centroids fall inside the chunk's core region. This reduces missed
  detections and duplicate detections at tile boundaries.
* `sam_detect()` exposes `chunk_size` and `chunk_overlap` for tuning.
* `sam_filter()` now filters chunked results (`sf_result`) — previously a
  no-op when only the merged sf was present.

## Imagery

* Tiles downloaded from Mapbox/Esri/MapTiler are now cached on disk under
  `rappdirs::user_cache_dir("geosam")/tiles/...`. `geosam_clear_cache()`
  removes them. Cache keys for paid providers use a hash of the API token
  rather than a fragment of the token itself.
* `get_imagery()` warns when missing tiles are filled with blank pixels.

## Installation & diagnostics

* `geosam_status()` distinguishes `Sam3Model` from `Sam3TrackerModel`
  availability and (with `check_access = TRUE`) verifies that the
  configured HuggingFace token can reach the gated `facebook/sam3`
  metadata. Failures are classified (`missing_token`,
  `gated_not_accepted`, `invalid_token`, `network_error`).
* Installation and documentation updated to reflect that SAM3 weights are
  gated — users must accept the model's access terms in addition to
  logging in.

## Misc

* References to a `"google"` imagery source corrected to `"maptiler"` in
  docs and arg checks.
* First test suite added (chunked filter + interactive quality policy).

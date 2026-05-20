# Changelog

## geosam 0.1.2 (development)

### Interactive performance

- [`sam_explore()`](https://walker-data.com/geosam/reference/sam_explore.md)
  gains a **Detection Quality** control (Fast / Balanced / Accurate)
  tucked under an “Advanced” disclosure. The default is `"balanced"`
  (chunked with moderate overlap) and can be overridden with the new
  `quality` argument. Fast mode skips chunking entirely for a more
  responsive feel on small areas; Accurate uses smaller chunks with more
  overlap.
- Esri satellite view in
  [`sam_explore()`](https://walker-data.com/geosam/reference/sam_explore.md)
  and
  [`sam_view()`](https://walker-data.com/geosam/reference/sam_view.md)
  reorders the OpenFreeMap overlay so road geometry sits beneath the
  satellite and only symbol/label layers ride above. Road name labels
  are restyled (light text, dark halo) for legibility on dark imagery.
- When Fast mode runs multiple text prompts on the same view, vision
  embeddings are computed once and reused across prompts (via a new
  `detect_text_multi()` Python helper modeled on HuggingFace’s
  multi-prompt SAM3 pattern).
- A text-embedding cache (LRU bounded to 64 entries) is available for
  experimental opt-in reuse across `detect_text()` calls with the same
  prompt. Disabled by default — the cached path passes `pooler_output`
  where the model documents a different shape and may produce different
  detections than the combined-processor call. The chunked detection
  path remains on the unchanged combined-processor code path.

### Chunked detection

- Chunked detection now overlaps neighboring chunks and keeps detections
  whose centroids fall inside the chunk’s core region. This reduces
  missed detections and duplicate detections at tile boundaries.
- [`sam_detect()`](https://walker-data.com/geosam/reference/sam_detect.md)
  exposes `chunk_size` and `chunk_overlap` for tuning.
- [`sam_filter()`](https://walker-data.com/geosam/reference/sam_filter.md)
  now filters chunked results (`sf_result`) — previously a no-op when
  only the merged sf was present.

### Imagery

- Tiles downloaded from Mapbox/Esri/MapTiler are now cached on disk
  under `rappdirs::user_cache_dir("geosam")/tiles/...`.
  [`geosam_clear_cache()`](https://walker-data.com/geosam/reference/geosam_clear_cache.md)
  removes them. Cache keys for paid providers use a hash of the API
  token rather than a fragment of the token itself.
- [`get_imagery()`](https://walker-data.com/geosam/reference/get_imagery.md)
  warns when missing tiles are filled with blank pixels.

### Installation & diagnostics

- [`geosam_status()`](https://walker-data.com/geosam/reference/geosam_status.md)
  distinguishes `Sam3Model` from `Sam3TrackerModel` availability and
  (with `check_access = TRUE`) verifies that the configured HuggingFace
  token can reach the gated `facebook/sam3` metadata. Failures are
  classified (`missing_token`, `gated_not_accepted`, `invalid_token`,
  `network_error`).
- Installation and documentation updated to reflect that SAM3 weights
  are gated — users must accept the model’s access terms in addition to
  logging in.

### Misc

- References to a `"google"` imagery source corrected to `"maptiler"` in
  docs and arg checks.
- First test suite added (chunked filter + interactive quality policy).

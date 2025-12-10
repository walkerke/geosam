#' Batch Process Multiple Areas
#'
#' Runs SAM3 detection on multiple areas (polygons) and returns combined results.
#'
#' @param areas An sf object with polygon geometries defining areas to process.
#' @param text Text prompt for detection.
#' @param source Imagery source: "mapbox", "esri", or "google".
#' @param zoom Tile zoom level for imagery download.
#' @param threshold Detection confidence threshold.
#' @param min_area Minimum object area in square meters.
#' @param max_area Maximum object area in square meters.
#' @param .progress Logical. If TRUE, shows a progress bar.
#'
#' @return An sf data frame with detection results. Includes an `area_id` column
#'   linking detections to input areas.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(tigris)
#'
#' # Process multiple counties
#' counties <- counties("TX") |>
#'   filter(NAME %in% c("Midland", "Ector"))
#'
#' pads <- sam_batch(
#'   areas = counties,
#'   text = "well pad",
#'   min_area = 500
#' )
#' }
sam_batch <- function(
    areas,
    text,
    source = "mapbox",
    zoom = 17,
    threshold = 0.5,
    min_area = NULL,
    max_area = NULL,
    .progress = TRUE
) {
  if (!inherits(areas, "sf")) {
    cli::cli_abort("{.arg areas} must be an sf object.")
  }

  n_areas <- nrow(areas)
  cli::cli_alert_info("Processing {n_areas} area{?s}...")

  # Pre-load model for efficiency
  if (!sam_is_loaded()) {
    sam_load()
  }

  results <- list()

  if (.progress) {
    pb <- cli::cli_progress_bar(total = n_areas, format = "{cli::pb_bar} {cli::pb_current}/{cli::pb_total}")
  }

  for (i in seq_len(n_areas)) {
    if (.progress) {
      cli::cli_progress_update(id = pb)
    }

    area <- areas[i, ]
    bbox <- sf::st_bbox(area)

    tryCatch({
      # Run detection (returns geosam object)
      geosam_result <- sam_detect(
        bbox = bbox,
        text = text,
        source = source,
        zoom = zoom,
        threshold = threshold
      )

      if (!is.null(geosam_result)) {
        # Filter and extract sf
        detection <- geosam_result |>
          sam_filter(min_area = min_area, max_area = max_area) |>
          sam_as_sf()

        if (!is.null(detection) && nrow(detection) > 0) {
          detection$area_id <- i
          results[[i]] <- detection
        }
      }
    }, error = function(e) {
      cli::cli_alert_warning("Area {i} failed: {e$message}")
    })
  }

  if (.progress) {
    cli::cli_progress_done(id = pb)
  }

  if (length(results) == 0) {
    cli::cli_alert_info("No objects detected in any area.")
    return(NULL)
  }

  # Combine results
  combined <- do.call(rbind, results)
  cli::cli_alert_success("Total detections: {nrow(combined)}")

  combined
}

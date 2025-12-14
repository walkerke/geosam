#' Configure geosam Options
#'
#' Set package-wide configuration options for model selection and device.
#'
#' @param model Model to use: "sam2" (default, latest) or "sam" (original).
#' @param device Computing device: "auto" (default), "mps", "cuda", or "cpu".
#'   "auto" selects the best available device.
#' @param mapbox_token Mapbox access token for satellite imagery. Can also be
#'   set via the MAPBOX_ACCESS_TOKEN environment variable.
#'
#' @return Invisibly returns the previous configuration.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Use SAM 2 on CPU
#' geosam_configure(model = "sam2", device = "cpu")
#'
#' # Set Mapbox token
#' geosam_configure(mapbox_token = "pk.xxx")
#' }
geosam_configure <- function(
    model = NULL,
    device = NULL,
    mapbox_token = NULL
) {
  prev <- list(
    model = getOption("geosam.model"),
    device = getOption("geosam.device"),
    mapbox_token = getOption("geosam.mapbox_token")
  )

  if (!is.null(model)) {
    model <- match.arg(model, c("sam2", "sam"))
    options(geosam.model = model)
    cli::cli_alert_info("Model set to: {model}")
  }

  if (!is.null(device)) {
    device <- match.arg(device, c("auto", "mps", "cuda", "cpu"))
    options(geosam.device = device)
    cli::cli_alert_info("Device set to: {device}")
  }

  if (!is.null(mapbox_token)) {
    options(geosam.mapbox_token = mapbox_token)
    cli::cli_alert_info("Mapbox token configured")
  }

  invisible(prev)
}


#' Get Effective Device
#'
#' Internal function to determine the device to use for inference.
#'
#' @return Character string: "mps", "cuda", or "cpu"
#' @noRd
.get_device <- function() {
  device <- getOption("geosam.device", "auto")

  if (device != "auto") {
    return(device)
  }

  # Auto-detect
  if (.geosam_env$device != is.null(.geosam_env$device)) {
    return(.geosam_env$device)
  }

  # Check via Python
  tryCatch({
    torch <- reticulate::import("torch", delay_load = TRUE)

    if (torch$backends$mps$is_available()) {
      .geosam_env$device <- "mps"
    } else if (torch$cuda$is_available()) {
      .geosam_env$device <- "cuda"
    } else {
      .geosam_env$device <- "cpu"
    }

    .geosam_env$device
  }, error = function(e) {
    "cpu"
  })
}


#' Get Mapbox Token
#'
#' Internal function to get the Mapbox access token from various sources.
#'
#' @return Character string with the token, or NULL if not found.
#' @noRd
.get_mapbox_token <- function() {
  # Priority order:
  # 1. Package option
  # 2. MAPBOX_PUBLIC_TOKEN env var (consistent with walkerke packages)
  # 3. MAPBOX_ACCESS_TOKEN env var (common alternative)
  # 4. mapboxapi package (if installed)

 token <- getOption("geosam.mapbox_token")

  if (is.null(token) || token == "") {
    token <- Sys.getenv("MAPBOX_PUBLIC_TOKEN", "")
  }

  if (token == "") {
    token <- Sys.getenv("MAPBOX_ACCESS_TOKEN", "")
  }

  if (token == "" && requireNamespace("mapboxapi", quietly = TRUE)) {
    tryCatch({
      token <- mapboxapi::mb_access_token()
    }, error = function(e) NULL)
  }

  if (is.null(token) || token == "") {
    return(NULL)
  }

  token
}


#' Get Model Configuration
#'
#' Internal function to get the model name for HuggingFace.
#'
#' @return Character string with model identifier
#' @noRd
.get_model_id <- function() {
  model <- getOption("geosam.model", "sam2")

  switch(model,
    sam2 = "facebook/sam2-hiera-large",
    sam = "facebook/sam-vit-huge",
    "facebook/sam2-hiera-large"
  )
}

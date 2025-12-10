# Package environment for storing state
.geosam_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  # Initialize package state
  .geosam_env$model_loaded <- FALSE
  .geosam_env$model_type <- NULL
  .geosam_env$device <- NULL
  .geosam_env$python_module <- NULL
  .geosam_env$worker_dir <- NULL
}

.onAttach <- function(libname, pkgname) {
  # Check if geosam environment is configured
  config_path <- file.path(rappdirs::user_config_dir("geosam"), "env_config.rds")

  if (!file.exists(config_path)) {
    packageStartupMessage(
      "geosam environment not found. Run geosam_install() to set up."
    )
  }
}

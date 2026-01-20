#' Install Python Dependencies for geosam
#'
#' Creates a Python virtual environment and installs required packages for SAM3
#' inference. This only needs to be run once.
#'
#' @param envname Name of the virtual environment to create (default: "geosam")
#' @param method Installation method: "uv" (recommended, fast), "virtualenv",
#'   or "conda". uv is the default and recommended approach.
#' @param gpu Logical. If TRUE, installs GPU-enabled PyTorch. If NULL (default),
#'   auto-detects based on available hardware.
#' @param hf_token Optional HuggingFace token for accessing SAM3 model weights.
#'   Can also be set via the HF_TOKEN environment variable. Required for SAM3.
#' @param python_version Python version to use (default: "3.12"). SAM3 requires 3.12+.
#' @param conda Path to conda executable. Only used when `method = "conda"`.
#'   If "auto" (default), uses reticulate's default conda. Useful when you have
#'   multiple conda installations (miniconda, miniforge, anaconda, etc.).
#'
#' @return Invisibly returns TRUE on success.
#'
#' @details
#' This function installs:
#' - PyTorch 2.7+ (with MPS support on Apple Silicon, CUDA 12.6+ on NVIDIA GPUs)
#' - SAM3 from Meta (https://github.com/facebookresearch/sam3)
#' - rasterio, geopandas, shapely, pyproj (geospatial processing)
#'
#' ## Installation Methods
#'
#' **uv (recommended)**: Fast, modern Python package manager. Install uv first:
#' ```
#' # macOS/Linux
#' curl -LsSf https://astral.sh/uv/install.sh | sh
#'
#' # Windows (PowerShell)
#' powershell -ExecutionPolicy ByPass -c "irm https://astral.sh/uv/install.ps1 | iex"
#'
#' # Or via pip (any platform)
#' pip install uv
#' ```
#'
#' **virtualenv**: Uses Python's built-in venv via reticulate. Requires Python 3.12+
#' to be installed on your system.
#'
#' **conda**: Uses conda/miniconda. Slower but handles complex dependencies well.
#' If you have multiple conda installations, use the `conda` parameter to specify
#' which one to use.
#'
#' ## SAM3 Access
#'
#' You must have HuggingFace access approval for SAM3 model weights.
#' Request access at: https://huggingface.co/facebook/sam3
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Standard installation with uv (recommended)
#' geosam_install()
#'
#' # With HuggingFace token
#' geosam_install(hf_token = "hf_xxxxx")
#'
#' # Use virtualenv instead of uv
#' geosam_install(method = "virtualenv")
#'
#' # Use conda
#' geosam_install(method = "conda")
#'
#' # Use a specific conda installation (e.g., miniforge instead of miniconda)
#' geosam_install(method = "conda", conda = "~/miniforge3/bin/conda")
#' }
geosam_install <- function(
    envname = "geosam",
    method = c("uv", "virtualenv", "conda"),
    gpu = NULL,
    hf_token = NULL,
    python_version = "3.12",
    conda = "auto"
) {
  method <- match.arg(method)

  cli::cli_h1("Installing geosam Python environment")
  cli::cli_alert_info("Method: {method}")

  # Detect GPU availability
  if (is.null(gpu)) {
    gpu <- .detect_gpu()
  }
  cli::cli_alert_info("GPU support: {if (gpu) 'enabled' else 'CPU only'}")

  # Dispatch to appropriate installer
  success <- switch(method,
    uv = .install_uv(envname, gpu, python_version),
    virtualenv = .install_virtualenv(envname, gpu, python_version),
    conda = .install_conda(envname, gpu, python_version, conda)
  )

  if (!success) {
    cli::cli_abort("Installation failed.")
  }

  # Store HuggingFace token if provided
  if (!is.null(hf_token)) {
    .store_hf_token(hf_token)
  }

  cli::cli_alert_success("Installation complete!")

  cli::cli_alert_info("Restart R and run {.code geosam_status()} to verify.")

  invisible(TRUE)
}


#' Install using uv (recommended)
#' @noRd
.install_uv <- function(envname, gpu, python_version) {
  # Check if uv is installed
  uv_path <- Sys.which("uv")
  if (uv_path == "") {
    if (.Platform$OS.type == "windows") {
      cli::cli_abort(c(
        "uv is not installed.",
        "i" = "Install uv with: {.code pip install uv}",
        "i" = "Or use {.code method = 'conda'} instead (recommended for Windows)."
      ))
    } else {
      cli::cli_abort(c(
        "uv is not installed.",
        "i" = "Install uv with: {.code curl -LsSf https://astral.sh/uv/install.sh | sh}",
        "i" = "Or use {.code method = 'virtualenv'} instead."
      ))
    }
  }

  # Determine environment path - use path without spaces to avoid issues
  env_path <- file.path(path.expand("~"), ".geosam", "venvs", envname)

  # Check if environment already exists
  if (dir.exists(env_path)) {
    cli::cli_alert_warning("Environment already exists at: {env_path}")
    if (interactive()) {
      response <- readline("Reinstall? (y/N): ")
      if (tolower(response) != "y") {
        return(TRUE)
      }
    }
    unlink(env_path, recursive = TRUE)
  }

  # Create environment with uv
  cli::cli_alert_info("Creating virtual environment with uv...")
  dir.create(dirname(env_path), recursive = TRUE, showWarnings = FALSE)

  # Build command as single string to handle properly
  cmd <- sprintf('uv venv "%s" --python %s', env_path, python_version)
  result <- system(cmd, intern = TRUE, ignore.stderr = FALSE)
  exit_code <- attr(result, "status")

  if (!is.null(exit_code) && exit_code != 0) {
    cli::cli_alert_danger("Failed to create environment")
    return(FALSE)
  }

  # Verify it was created (path differs on Windows vs Unix)
  if (.Platform$OS.type == "windows") {
    python_path <- file.path(env_path, "Scripts", "python.exe")
  } else {
    python_path <- file.path(env_path, "bin", "python")
  }
  if (!file.exists(python_path)) {
    cli::cli_alert_danger("Environment creation failed - python not found at {python_path}")
    return(FALSE)
  }

  # Install packages using uv pip
  cli::cli_alert_info("Installing PyTorch...")
  .uv_pip_install(env_path, .get_pytorch_packages(gpu))

  cli::cli_alert_info("Installing HuggingFace transformers (dev branch for SAM3)...")
  .uv_pip_install(env_path, c("git+https://github.com/huggingface/transformers.git", "huggingface_hub"))

  cli::cli_alert_info("Installing geospatial dependencies...")
  .uv_pip_install(env_path, c(
    "rasterio",
    "geopandas",
    "shapely",
    "pyproj",
    "numpy",
    "pillow",
    "requests"
  ))

  # Save environment path for later use
  .save_env_config(env_path, method = "uv")

  cli::cli_alert_success("Environment created at: {env_path}")
  TRUE
}


#' Run uv pip install
#' @noRd
.uv_pip_install <- function(env_path, packages) {
  # Quote each package spec to handle >=
  pkgs_quoted <- paste(shQuote(packages), collapse = " ")
  cmd <- sprintf('uv pip install --python "%s" %s', env_path, pkgs_quoted)

  result <- system(cmd, intern = TRUE, ignore.stderr = FALSE)
  exit_code <- attr(result, "status")

  if (!is.null(exit_code) && exit_code != 0) {
    cli::cli_abort("pip install failed: {paste(result, collapse = '\n')}")
  }

  invisible(TRUE)
}


#' Install using virtualenv (via reticulate)
#' @noRd
.install_virtualenv <- function(envname, gpu, python_version) {
  # Check Python availability
  python_cmd <- Sys.which("python3")
  if (python_cmd == "") {
    python_cmd <- Sys.which("python")
  }

  if (python_cmd == "") {
    cli::cli_abort(c(
      "Python not found.",
      "i" = "Install Python {python_version}+ from https://python.org"
    ))
  }

  # Check version
  version_output <- system2(python_cmd, "--version", stdout = TRUE)
  cli::cli_alert_info("Found: {version_output}")

  # Determine environment path
 env_path <- file.path(rappdirs::user_data_dir("geosam"), "venvs", envname)

  # Check if environment already exists
  if (dir.exists(env_path)) {
    cli::cli_alert_warning("Environment already exists at: {env_path}")
    if (interactive()) {
      response <- readline("Reinstall? (y/N): ")
      if (tolower(response) != "y") {
        return(TRUE)
      }
    }
    unlink(env_path, recursive = TRUE)
  }

  # Create environment
  cli::cli_alert_info("Creating virtual environment...")
  dir.create(dirname(env_path), recursive = TRUE, showWarnings = FALSE)

  reticulate::virtualenv_create(
    envname = env_path,
    python = python_cmd
  )

  # Install packages
  cli::cli_alert_info("Installing PyTorch...")
  reticulate::virtualenv_install(
    envname = env_path,
    packages = .get_pytorch_packages(gpu),
    pip_options = character()
  )

  cli::cli_alert_info("Installing HuggingFace transformers (dev branch for SAM3)...")
  reticulate::virtualenv_install(
    envname = env_path,
    packages = c("git+https://github.com/huggingface/transformers.git", "huggingface_hub")
  )

  cli::cli_alert_info("Installing geospatial dependencies...")
  reticulate::virtualenv_install(
    envname = env_path,
    packages = c(
      "rasterio>=1.3",
      "geopandas>=0.14",
      "shapely>=2.0",
      "pyproj>=3.6",
      "numpy>=1.24",
      "pillow>=10.0",
      "requests>=2.31"
    )
  )

  # Save environment path
  .save_env_config(env_path, method = "virtualenv")

  cli::cli_alert_success("Environment created at: {env_path}")
  TRUE
}


#' Install using conda
#' @noRd
.install_conda <- function(envname, gpu, python_version, conda = "auto") {
  # Resolve conda path
 if (identical(conda, "auto")) {
    conda_path <- tryCatch(reticulate::conda_binary(), error = function(e) NULL)
    if (is.null(conda_path)) {
      # No conda found - check if there's one elsewhere with helpful message
      all_condas <- .find_conda_installations()
      if (length(all_condas) > 0) {
        cli::cli_abort(c(
          "Conda not found by reticulate, but found conda installation(s) at:",
          "i" = paste(all_condas, collapse = "\n"),
          "i" = "Try specifying the conda path explicitly:",
          "i" = "{.code geosam_install(method = 'conda', conda = '{all_condas[1]}')}"
        ))
      } else {
        cli::cli_abort(c(
          "Conda not found.",
          "i" = "Install miniconda from https://docs.conda.io/en/latest/miniconda.html",
          "i" = "Or use {.code method = 'uv'} instead (recommended)."
        ))
      }
    }
  } else {
    # User specified a conda path
    conda_path <- path.expand(conda)
    if (!file.exists(conda_path)) {
      cli::cli_abort(c(
        "Specified conda not found at: {conda_path}",
        "i" = "Run {.code geosam_diagnose()} to see available conda installations."
      ))
    }
  }

  cli::cli_alert_info("Using conda: {conda_path}")

  # Check for existing environment in THIS conda
  envs <- tryCatch({
    reticulate::conda_list(conda = conda_path)
  }, error = function(e) {
    cli::cli_abort(c(
      "Failed to list conda environments.",
      "x" = e$message
    ))
  })

  if (envname %in% envs$name) {
    # Environment exists in this conda
    cli::cli_alert_warning("Conda environment '{envname}' already exists.")
    if (interactive()) {
      response <- readline("Reinstall? (y/N): ")
      if (tolower(response) != "y") {
        # Save config so geosam knows where to find it
        .save_env_config(envname, method = "conda", conda = conda_path)
        return(TRUE)
      }
      cli::cli_alert_info("Removing existing environment...")
      reticulate::conda_remove(envname, conda = conda_path)
    } else {
      .save_env_config(envname, method = "conda", conda = conda_path)
      return(TRUE)
    }
  } else {
    # Environment doesn't exist in this conda - check if it exists elsewhere
    found_elsewhere <- .find_geosam_env(envname)
    if (!is.null(found_elsewhere) && normalizePath(found_elsewhere$conda, mustWork = FALSE) != normalizePath(conda_path, mustWork = FALSE)) {
      cli::cli_alert_warning("Environment '{envname}' was not found in the current conda.")
      cli::cli_alert_info("However, '{envname}' exists in a different conda installation:")
      cli::cli_alert_info("  Conda: {found_elsewhere$conda}")
      cli::cli_alert_info("  Python: {found_elsewhere$python}")
      cli::cli_alert("")
      cli::cli_alert_info("To use the existing environment, run:")
      cli::cli_alert_info("{.code geosam_install(method = 'conda', conda = '{found_elsewhere$conda}')}")
      cli::cli_alert("")

      if (interactive()) {
        response <- readline("Create a NEW environment in the current conda instead? (y/N): ")
        if (tolower(response) != "y") {
          return(FALSE)
        }
      } else {
        cli::cli_abort(c(
          "Environment exists in different conda installation.",
          "i" = "Use {.code conda = '{found_elsewhere$conda}'} to use the existing environment."
        ))
      }
    }
  }

  # Create environment
  cli::cli_alert_info("Creating conda environment '{envname}'...")
  reticulate::conda_create(
    envname = envname,
    python_version = python_version,
    conda = conda_path
  )

  # Install PyTorch
  cli::cli_alert_info("Installing PyTorch...")
  reticulate::conda_install(
    envname = envname,
    packages = .get_pytorch_packages(gpu),
    pip = TRUE,
    conda = conda_path
  )

  # Install HuggingFace transformers (dev branch for SAM3)
  cli::cli_alert_info("Installing HuggingFace transformers...")
  reticulate::conda_install(
    envname = envname,
    packages = c("git+https://github.com/huggingface/transformers.git", "huggingface_hub"),
    pip = TRUE,
    conda = conda_path
  )

  # Install geospatial deps
  cli::cli_alert_info("Installing geospatial dependencies...")
  reticulate::conda_install(
    envname = envname,
    packages = c(
      "rasterio>=1.3",
      "geopandas>=0.14",
      "shapely>=2.0",
      "pyproj>=3.6",
      "numpy>=1.24",
      "pillow>=10.0",
      "requests>=2.31"
    ),
    pip = TRUE,
    conda = conda_path
  )

  # Save config with conda path
  .save_env_config(envname, method = "conda", conda = conda_path)

  cli::cli_alert_success("Conda environment '{envname}' created.")
  TRUE
}


#' Get PyTorch packages based on GPU availability
#' @noRd
.get_pytorch_packages <- function(gpu) {
  if (gpu && .is_apple_silicon()) {
    # Apple Silicon - MPS support comes automatically
    c("torch>=2.7", "torchvision", "torchaudio")
  } else if (gpu) {
    # NVIDIA CUDA
    c("torch>=2.7", "torchvision", "torchaudio", "--index-url", "https://download.pytorch.org/whl/cu126")
  } else {
    # CPU only
    c("torch>=2.7", "torchvision", "torchaudio")
  }
}


#' Save environment configuration
#' @noRd
.save_env_config <- function(env_path, method, conda = NULL) {
  config_dir <- rappdirs::user_config_dir("geosam")
  dir.create(config_dir, recursive = TRUE, showWarnings = FALSE)

  config <- list(
    env_path = env_path,
    method = method,
    conda = conda,
    created = Sys.time()
  )

  saveRDS(config, file.path(config_dir, "env_config.rds"))
}


#' Load environment configuration
#' @noRd
.load_env_config <- function() {
  config_path <- file.path(rappdirs::user_config_dir("geosam"), "env_config.rds")

  if (!file.exists(config_path)) {
    return(NULL)
  }

  readRDS(config_path)
}


#' Store HuggingFace token
#' @noRd
.store_hf_token <- function(hf_token) {
  cli::cli_alert_info("Storing HuggingFace token...")
  Sys.setenv(HF_TOKEN = hf_token)

  # Save to .Renviron for persistence (use path.expand for Windows compatibility)
  renviron_path <- file.path(path.expand("~"), ".Renviron")
  if (file.exists(renviron_path)) {
    lines <- readLines(renviron_path)
    lines <- lines[!grepl("^HF_TOKEN=", lines)]
  } else {
    lines <- character()
  }
  lines <- c(lines, paste0("HF_TOKEN=", hf_token))
  writeLines(lines, renviron_path)

  cli::cli_alert_success("HuggingFace token saved to ~/.Renviron")
}


#' Check geosam Installation Status
#'
#' Checks the Python environment and reports which features are available.
#'
#' @return A list with installation status information, invisibly.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' geosam_status()
#' }
geosam_status <- function() {
  cli::cli_h1("geosam Status")

  status <- list(
    python_available = FALSE,
    torch_available = FALSE,
    torch_version = NULL,
    device = "unknown",
    mps_available = FALSE,
    cuda_available = FALSE,
    sam3_available = FALSE,
    hf_token_set = FALSE,
    env_path = NULL,
    env_method = NULL
  )

  # Check for saved environment config
  config <- .load_env_config()
  if (!is.null(config)) {
    status$env_path <- config$env_path
    status$env_method <- config$method
    cli::cli_alert_info("Environment: {config$method} at {config$env_path}")

    # Activate the environment
    .activate_env(config)
  } else {
    cli::cli_alert_warning("No geosam environment found. Run {.code geosam_install()} first.")
  }

  # Check Python
  if (!reticulate::py_available(initialize = TRUE)) {
    cli::cli_alert_danger("Python not available.")
    return(invisible(status))
  }

  status$python_available <- TRUE
  py_config <- reticulate::py_config()
  cli::cli_alert_success("Python: {py_config$version}")
  cli::cli_alert_info("Path: {py_config$python}")

  # Check PyTorch
  tryCatch({
    torch <- reticulate::import("torch")
    status$torch_available <- TRUE
    status$torch_version <- torch$`__version__`

    if (torch$backends$mps$is_available()) {
      status$device <- "mps"
      status$mps_available <- TRUE
      cli::cli_alert_success("PyTorch {status$torch_version}: MPS (Apple Silicon)")
    } else if (torch$cuda$is_available()) {
      status$device <- "cuda"
      status$cuda_available <- TRUE
      cli::cli_alert_success("PyTorch {status$torch_version}: CUDA")
    } else {
      status$device <- "cpu"
      cli::cli_alert_success("PyTorch {status$torch_version}: CPU")
    }
  }, error = function(e) {
    cli::cli_alert_danger("PyTorch: not available")
  })

  # Check transformers (for SAM3 via HuggingFace)
  tryCatch({
    transformers <- reticulate::import("transformers")
    status$sam3_available <- TRUE
    cli::cli_alert_success("transformers: available (SAM3 via HuggingFace)")
  }, error = function(e) {
    cli::cli_alert_danger("transformers: not available")
  })

  # Check HuggingFace token
  hf_token <- Sys.getenv("HF_TOKEN", "")
  if (nchar(hf_token) > 0) {
    status$hf_token_set <- TRUE
    cli::cli_alert_success("HuggingFace token: configured")
  } else {
    cli::cli_alert_warning("HuggingFace token: not set (required for SAM3)")
    cli::cli_alert_info("Set with: {.code Sys.setenv(HF_TOKEN = 'hf_xxxxx')}")
  }

  invisible(status)
}


#' Diagnose geosam Installation Issues
#'
#' Scans the system for conda installations and geosam environments to help
#' troubleshoot installation problems. Useful when you have multiple conda
#' distributions installed (miniconda, miniforge, anaconda, etc.).
#'
#' @return A list with diagnostic information, invisibly.
#'
#' @export
#'
#' @examples
#' \dontrun{
#' geosam_diagnose()
#' }
geosam_diagnose <- function() {
  cli::cli_h1("geosam Installation Diagnostics")

  result <- list(
    conda_installations = character(),
    geosam_environments = list(),
    reticulate_conda = NULL,
    saved_config = NULL
  )

  # Check what reticulate thinks is the default conda
  cli::cli_h2("Reticulate Configuration")
  tryCatch({
    result$reticulate_conda <- reticulate::conda_binary()
    cli::cli_alert_success("Reticulate default conda: {result$reticulate_conda}")
  }, error = function(e) {
    cli::cli_alert_warning("Reticulate cannot find conda")
  })

  # Find all conda installations
  cli::cli_h2("Conda Installations Found")
  condas <- .find_conda_installations()
  result$conda_installations <- condas

  if (length(condas) == 0) {
    cli::cli_alert_warning("No conda installations found.")
    cli::cli_alert_info("Install miniconda from: https://docs.conda.io/en/latest/miniconda.html")
  } else {
    for (conda in condas) {
      cli::cli_alert_success("{conda}")
    }

    if (length(condas) > 1) {
      cli::cli_alert_info("Multiple conda installations detected. Use the {.code conda} parameter to specify which one to use.")
    }
  }

  # Search for geosam environments
  cli::cli_h2("geosam Environments")
  for (conda in condas) {
    tryCatch({
      envs <- reticulate::conda_list(conda = conda)
      if ("geosam" %in% envs$name) {
        idx <- which(envs$name == "geosam")[1]
        env_info <- list(
          conda = conda,
          python = envs$python[idx]
        )
        result$geosam_environments[[length(result$geosam_environments) + 1]] <- env_info
        cli::cli_alert_success("Found 'geosam' environment:")
        cli::cli_alert_info("  Conda: {conda}")
        cli::cli_alert_info("  Python: {envs$python[idx]}")
      }
    }, error = function(e) NULL)
  }

  if (length(result$geosam_environments) == 0) {
    cli::cli_alert_warning("No 'geosam' conda environments found.")
    cli::cli_alert_info("Run {.code geosam_install(method = 'conda')} to create one.")
  } else if (length(result$geosam_environments) > 1) {
    cli::cli_alert_warning("Multiple 'geosam' environments found! This may cause confusion.")
  }

  # Check saved geosam configuration
  cli::cli_h2("Saved geosam Configuration")
  config <- .load_env_config()
  result$saved_config <- config

  if (is.null(config)) {
    cli::cli_alert_warning("No saved geosam configuration found.")
  } else {
    cli::cli_alert_info("Method: {config$method}")
    cli::cli_alert_info("Environment: {config$env_path}")
    if (!is.null(config$conda)) {
      cli::cli_alert_info("Conda: {config$conda}")
    }
    cli::cli_alert_info("Created: {config$created}")
  }

  # Provide recommendations
  cli::cli_h2("Recommendations")

  if (length(result$geosam_environments) == 1 && !is.null(result$reticulate_conda)) {
    env <- result$geosam_environments[[1]]
    if (normalizePath(env$conda, mustWork = FALSE) != normalizePath(result$reticulate_conda, mustWork = FALSE)) {
      cli::cli_alert_warning("Your geosam environment is in a different conda than reticulate's default!")
      cli::cli_alert_info("To fix this, run:")
      cli::cli_alert_info("{.code geosam_install(method = 'conda', conda = '{env$conda}')}")
    } else {
      cli::cli_alert_success("Configuration looks good.")
    }
  } else if (length(result$geosam_environments) == 0 && length(condas) > 0) {
    cli::cli_alert_info("Create a geosam environment with:")
    cli::cli_alert_info("{.code geosam_install(method = 'conda')}")
  } else if (length(condas) == 0) {
    cli::cli_alert_info("Consider using the 'uv' method instead:")
    cli::cli_alert_info("{.code geosam_install(method = 'uv')}")
  }

  invisible(result)
}


#' Activate the geosam Python environment
#' @noRd
.activate_env <- function(config = NULL) {
  if (is.null(config)) {
    config <- .load_env_config()
  }

  if (is.null(config)) {
    return(FALSE)
  }

  if (config$method == "conda") {
    tryCatch({
      # Use stored conda path if available
      if (!is.null(config$conda)) {
        reticulate::use_condaenv(config$env_path, conda = config$conda, required = TRUE)
      } else {
        reticulate::use_condaenv(config$env_path, required = TRUE)
      }
      TRUE
    }, error = function(e) FALSE)
  } else {
    # uv or virtualenv - both create standard venvs
    tryCatch({
      reticulate::use_virtualenv(config$env_path, required = TRUE)
      TRUE
    }, error = function(e) FALSE)
  }
}


#' Detect GPU availability
#' @noRd
.detect_gpu <- function() {
  if (Sys.info()["sysname"] == "Darwin") {
    return(.is_apple_silicon())
  }

  # Check for NVIDIA GPU
  tryCatch({
    output <- system("nvidia-smi", intern = TRUE, ignore.stderr = TRUE)
    length(output) > 0
  }, error = function(e) FALSE)
}


#' Detect Apple Silicon
#' @noRd
.is_apple_silicon <- function() {

  if (Sys.info()["sysname"] != "Darwin") {
    return(FALSE)
  }

  tryCatch({
    output <- system("sysctl -n machdep.cpu.brand_string", intern = TRUE)
    grepl("Apple", output, ignore.case = TRUE)
  }, error = function(e) FALSE)
}


#' Find All Conda Installations
#'
#' Searches common locations for conda installations.
#'
#' @return Character vector of paths to conda executables that exist.
#' @noRd
.find_conda_installations <- function() {
  condas <- character()


  # Try reticulate's conda_binary() first
  tryCatch({
    condas <- c(condas, reticulate::conda_binary())
  }, error = function(e) NULL)

  # Common locations vary by OS
  if (.Platform$OS.type == "windows") {
    home <- Sys.getenv("USERPROFILE")
    candidates <- c(
      file.path(home, "miniconda3", "Scripts", "conda.exe"),
      file.path(home, "miniforge3", "Scripts", "conda.exe"),
      file.path(home, "mambaforge", "Scripts", "conda.exe"),
      file.path(home, "anaconda3", "Scripts", "conda.exe"),
      file.path(home, "Miniconda3", "Scripts", "conda.exe"),
      file.path(home, "Miniforge3", "Scripts", "conda.exe"),
      file.path(home, "Mambaforge", "Scripts", "conda.exe"),
      file.path(home, "Anaconda3", "Scripts", "conda.exe"),
      "C:/ProgramData/miniconda3/Scripts/conda.exe",
      "C:/ProgramData/miniforge3/Scripts/conda.exe",
      "C:/ProgramData/Anaconda3/Scripts/conda.exe"
    )
  } else {
    home <- path.expand("~")
    candidates <- c(
      file.path(home, "miniconda3", "bin", "conda"),
      file.path(home, "miniforge3", "bin", "conda"),
      file.path(home, "mambaforge", "bin", "conda"),
      file.path(home, "anaconda3", "bin", "conda"),
      "/opt/miniconda3/bin/conda",
      "/opt/miniforge3/bin/conda",
      "/opt/anaconda3/bin/conda",
      "/usr/local/miniconda3/bin/conda",
      "/usr/local/Caskroom/miniconda/base/bin/conda"
    )
  }

  existing <- candidates[file.exists(candidates)]
  unique(c(condas, existing))
}


#' Find geosam Environment Across All Conda Installations
#'
#' Searches all detected conda installations for an existing geosam environment.
#'
#' @param envname Name of the environment to search for (default: "geosam")
#' @return A list with conda path, environment name, and python path if found; NULL otherwise.
#' @noRd
.find_geosam_env <- function(envname = "geosam") {
  condas <- .find_conda_installations()

  for (conda_path in condas) {
    tryCatch({
      envs <- reticulate::conda_list(conda = conda_path)
      if (envname %in% envs$name) {
        idx <- which(envs$name == envname)[1]
        return(list(
          conda = conda_path,
          name = envname,
          python = envs$python[idx]
        ))
      }
    }, error = function(e) NULL)
  }

  NULL
}

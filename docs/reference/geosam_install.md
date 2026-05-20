# Install Python Dependencies for geosam

Creates a Python virtual environment and installs required packages for
SAM3 inference. This only needs to be run once.

## Usage

``` r
geosam_install(
  envname = "geosam",
  method = c("uv", "virtualenv", "conda"),
  gpu = NULL,
  hf_token = NULL,
  python_version = "3.12",
  conda = "auto"
)
```

## Arguments

- envname:

  Name of the virtual environment to create (default: "geosam")

- method:

  Installation method: "uv" (recommended, fast), "virtualenv", or
  "conda". uv is the default and recommended approach.

- gpu:

  Logical. If TRUE, installs GPU-enabled PyTorch. If NULL (default),
  auto-detects based on available hardware.

- hf_token:

  Optional HuggingFace token for accessing SAM3 model weights. Can also
  be set via the HF_TOKEN environment variable. Required for SAM3.

- python_version:

  Python version to use (default: "3.12"). SAM3 requires 3.12+.

- conda:

  Path to conda executable. Only used when `method = "conda"`. If "auto"
  (default), uses reticulate's default conda. Useful when you have
  multiple conda installations (miniconda, miniforge, anaconda, etc.).

## Value

Invisibly returns TRUE on success.

## Details

This function installs:

- PyTorch 2.7+ (with MPS support on Apple Silicon, CUDA 12.6+ on NVIDIA
  GPUs)

- SAM3 from Meta (https://github.com/facebookresearch/sam3)

- rasterio, geopandas, shapely, pyproj (geospatial processing)

### Installation Methods

**uv (recommended)**: Fast, modern Python package manager. Install uv
first:

    # macOS/Linux
    curl -LsSf https://astral.sh/uv/install.sh | sh

    # Windows (PowerShell)
    powershell -ExecutionPolicy ByPass -c "irm https://astral.sh/uv/install.ps1 | iex"

    # Or via pip (any platform)
    pip install uv

**virtualenv**: Uses Python's built-in venv via reticulate. Requires
Python 3.12+ to be installed on your system.

**conda**: Uses conda/miniconda. Slower but handles complex dependencies
well. If you have multiple conda installations, use the `conda`
parameter to specify which one to use.

### SAM3 Access

You must log in to HuggingFace and accept the gated model terms for SAM3
model weights at: https://huggingface.co/facebook/sam3

## Examples

``` r
if (FALSE) { # \dontrun{
# Standard installation with uv (recommended)
geosam_install()

# With HuggingFace token
geosam_install(hf_token = "hf_xxxxx")

# Use virtualenv instead of uv
geosam_install(method = "virtualenv")

# Use conda
geosam_install(method = "conda")

# Use a specific conda installation (e.g., miniforge instead of miniconda)
geosam_install(method = "conda", conda = "~/miniforge3/bin/conda")
} # }
```

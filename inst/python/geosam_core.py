"""
geosam Core Python Module

Provides SAM3 inference for geospatial image segmentation.
Designed to be called from R via reticulate.

Uses HuggingFace transformers for SAM3 model access.

NOTE: This module only handles SAM inference. All geospatial processing
(reading GeoTIFFs, converting masks to polygons) is done in R using terra.
"""

import os
from typing import Optional, List, Dict, Any

# Fix OpenMP conflict on macOS
os.environ["KMP_DUPLICATE_LIB_OK"] = "TRUE"
# Disable tokenizers parallelism to avoid fork warnings
os.environ["TOKENIZERS_PARALLELISM"] = "false"

import numpy as np
from PIL import Image
import torch

# Global model references (for keeping model warm)
_MODEL = None
_PROCESSOR = None
_DEVICE = None


def get_device() -> str:
    """Detect best available device for PyTorch."""
    if torch.backends.mps.is_available():
        return "mps"
    elif torch.cuda.is_available():
        return "cuda"
    return "cpu"


def load_model(device: Optional[str] = None) -> bool:
    """
    Load SAM3 model and processor via HuggingFace transformers.

    Args:
        device: Computing device ("mps", "cuda", "cpu", or None for auto)

    Returns:
        True if successful
    """
    global _MODEL, _PROCESSOR, _DEVICE

    if _MODEL is not None:
        return True

    from transformers import Sam3Processor, Sam3Model

    _DEVICE = device or get_device()
    print(f"Loading SAM3 model on {_DEVICE}...")

    _MODEL = Sam3Model.from_pretrained("facebook/sam3").to(_DEVICE)
    _PROCESSOR = Sam3Processor.from_pretrained("facebook/sam3")

    print("SAM3 model loaded and ready!")
    return True


def unload_model() -> bool:
    """Unload model to free memory."""
    global _MODEL, _PROCESSOR, _DEVICE

    _MODEL = None
    _PROCESSOR = None
    _DEVICE = None

    import gc
    gc.collect()

    if torch.cuda.is_available():
        torch.cuda.empty_cache()

    return True


def is_model_loaded() -> bool:
    """Check if model is currently loaded."""
    return _MODEL is not None


def detect_text(
    img_array: np.ndarray,
    text_prompt: str,
    threshold: float = 0.5
) -> Dict[str, Any]:
    """
    Detect objects matching a text prompt using SAM3.

    Args:
        img_array: RGB image as numpy array (height, width, 3)
        text_prompt: Text description of objects to find
        threshold: Detection confidence threshold (0-1)

    Returns:
        Dict with 'masks' (list of 2D numpy arrays) and 'scores' (list of floats)
    """
    global _MODEL, _PROCESSOR, _DEVICE

    if _MODEL is None:
        load_model()

    height, width = img_array.shape[:2]
    pil_image = Image.fromarray(img_array.astype(np.uint8))

    # Run inference with text prompt
    print(f"Running SAM3 with text prompt: '{text_prompt}'")
    inputs = _PROCESSOR(images=pil_image, text=text_prompt, return_tensors="pt").to(_DEVICE)

    with torch.no_grad():
        outputs = _MODEL(**inputs)

    results = _PROCESSOR.post_process_instance_segmentation(
        outputs,
        threshold=threshold,
        mask_threshold=0.5,
        target_sizes=[[height, width]]
    )[0]

    masks = results["masks"]
    scores = results["scores"]

    print(f"SAM3 found {len(masks)} objects")

    # Convert to numpy arrays for R
    mask_list = [mask.cpu().numpy().astype(np.uint8) for mask in masks]
    score_list = [float(s.cpu()) if hasattr(s, 'cpu') else float(s) for s in scores]

    return {
        "masks": mask_list,
        "scores": score_list,
        "count": len(mask_list),
        "prompt": text_prompt
    }


def detect_boxes(
    img_array: np.ndarray,
    pixel_boxes: List[List[int]],
    threshold: float = 0.5
) -> Dict[str, Any]:
    """
    Detect objects within bounding boxes using SAM3.

    Args:
        img_array: RGB image as numpy array (height, width, 3)
        pixel_boxes: List of bounding boxes in PIXEL coordinates [[xmin, ymin, xmax, ymax], ...]
        threshold: Detection confidence threshold

    Returns:
        Dict with 'masks' (list of 2D numpy arrays) and 'scores' (list of floats)
    """
    global _MODEL, _PROCESSOR, _DEVICE

    if _MODEL is None:
        load_model()

    height, width = img_array.shape[:2]
    pil_image = Image.fromarray(img_array.astype(np.uint8))

    print(f"Running SAM3 with {len(pixel_boxes)} box prompt(s)")

    # Run inference with box prompts
    inputs = _PROCESSOR(
        images=pil_image,
        input_boxes=[pixel_boxes],
        return_tensors="pt"
    ).to(_DEVICE)

    with torch.no_grad():
        outputs = _MODEL(**inputs)

    results = _PROCESSOR.post_process_instance_segmentation(
        outputs,
        threshold=threshold,
        mask_threshold=0.5,
        target_sizes=[[height, width]]
    )[0]

    masks = results["masks"]
    scores = results["scores"]

    print(f"SAM3 found {len(masks)} objects")

    # Convert to numpy arrays for R
    mask_list = [mask.cpu().numpy().astype(np.uint8) for mask in masks]
    score_list = [float(s.cpu()) if hasattr(s, 'cpu') else float(s) for s in scores]

    return {
        "masks": mask_list,
        "scores": score_list,
        "count": len(mask_list)
    }


def detect_points(
    img_array: np.ndarray,
    pixel_points: List[List[int]],
    labels: List[int],
    threshold: float = 0.5
) -> Dict[str, Any]:
    """
    Detect objects at point locations using SAM3.

    Args:
        img_array: RGB image as numpy array (height, width, 3)
        pixel_points: List of point coordinates in PIXEL space [[col, row], ...]
        labels: List of labels (1 for foreground, 0 for background)
        threshold: Detection confidence threshold

    Returns:
        Dict with 'masks' (list of 2D numpy arrays) and 'scores' (list of floats)
    """
    global _MODEL, _PROCESSOR, _DEVICE

    if _MODEL is None:
        load_model()

    height, width = img_array.shape[:2]
    pil_image = Image.fromarray(img_array.astype(np.uint8))

    print(f"Running SAM3 with {len(pixel_points)} point prompt(s)")

    # Run inference with point prompts
    inputs = _PROCESSOR(
        images=pil_image,
        input_points=[pixel_points],
        input_labels=[labels],
        return_tensors="pt"
    ).to(_DEVICE)

    with torch.no_grad():
        outputs = _MODEL(**inputs)

    results = _PROCESSOR.post_process_instance_segmentation(
        outputs,
        threshold=threshold,
        mask_threshold=0.5,
        target_sizes=[[height, width]]
    )[0]

    masks = results["masks"]
    scores = results["scores"]

    print(f"SAM3 found {len(masks)} objects")

    # Convert to numpy arrays for R
    mask_list = [mask.cpu().numpy().astype(np.uint8) for mask in masks]
    score_list = [float(s.cpu()) if hasattr(s, 'cpu') else float(s) for s in scores]

    return {
        "masks": mask_list,
        "scores": score_list,
        "count": len(mask_list)
    }


def check_environment() -> Dict[str, Any]:
    """
    Check the Python environment and available features.

    Returns:
        Dict with environment info
    """
    info = {
        "device": get_device(),
        "mps_available": torch.backends.mps.is_available(),
        "cuda_available": torch.cuda.is_available(),
        "transformers_available": False,
        "model_loaded": is_model_loaded()
    }

    try:
        from transformers import Sam3Processor, Sam3Model
        info["transformers_available"] = True
    except ImportError:
        pass

    return info

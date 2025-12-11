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
# Sam3Model for text prompts (PCS - Promptable Category Segmentation)
_MODEL = None
_PROCESSOR = None
# Sam3TrackerModel for point/box prompts (PVS - Promptable Visual Segmentation)
_TRACKER_MODEL = None
_TRACKER_PROCESSOR = None
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


def load_tracker_model(device: Optional[str] = None) -> bool:
    """
    Load SAM3 Tracker model for point/box prompts.

    The tracker model supports PVS (Promptable Visual Segmentation) with
    point and box prompts, unlike Sam3Model which only supports text prompts.

    Args:
        device: Computing device ("mps", "cuda", "cpu", or None for auto)

    Returns:
        True if successful
    """
    global _TRACKER_MODEL, _TRACKER_PROCESSOR, _DEVICE

    if _TRACKER_MODEL is not None:
        return True

    from transformers import Sam3TrackerProcessor, Sam3TrackerModel

    _DEVICE = device or get_device()
    print(f"Loading SAM3 Tracker model on {_DEVICE}...")

    _TRACKER_MODEL = Sam3TrackerModel.from_pretrained("facebook/sam3").to(_DEVICE)
    _TRACKER_PROCESSOR = Sam3TrackerProcessor.from_pretrained("facebook/sam3")

    print("SAM3 Tracker model loaded and ready!")
    return True


def unload_model() -> bool:
    """Unload all models to free memory."""
    global _MODEL, _PROCESSOR, _TRACKER_MODEL, _TRACKER_PROCESSOR, _DEVICE

    _MODEL = None
    _PROCESSOR = None
    _TRACKER_MODEL = None
    _TRACKER_PROCESSOR = None
    _DEVICE = None

    import gc
    gc.collect()

    if torch.cuda.is_available():
        torch.cuda.empty_cache()

    return True


def is_model_loaded() -> bool:
    """Check if text model is currently loaded."""
    return _MODEL is not None


def is_tracker_loaded() -> bool:
    """Check if tracker model (for point/box prompts) is currently loaded."""
    return _TRACKER_MODEL is not None


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
    Detect objects within bounding boxes using SAM3 Tracker model.

    Args:
        img_array: RGB image as numpy array (height, width, 3)
        pixel_boxes: List of bounding boxes in PIXEL coordinates [[xmin, ymin, xmax, ymax], ...]
        threshold: Detection confidence threshold

    Returns:
        Dict with 'masks' (list of 2D numpy arrays) and 'scores' (list of floats)
    """
    global _TRACKER_MODEL, _TRACKER_PROCESSOR, _DEVICE

    if _TRACKER_MODEL is None:
        load_tracker_model()

    height, width = img_array.shape[:2]
    pil_image = Image.fromarray(img_array.astype(np.uint8))

    print(f"Running SAM3 Tracker with {len(pixel_boxes)} box prompt(s)")

    # Run inference with box prompts using tracker model
    inputs = _TRACKER_PROCESSOR(
        images=pil_image,
        input_boxes=[pixel_boxes],
        return_tensors="pt"
    ).to(_DEVICE)

    with torch.no_grad():
        outputs = _TRACKER_MODEL(**inputs)

    # Post-process masks
    masks = _TRACKER_PROCESSOR.post_process_masks(
        outputs.pred_masks,
        inputs["original_sizes"],
        inputs["reshaped_input_sizes"]
    )

    # masks shape: (batch, num_boxes, num_masks_per_box, H, W)
    # We typically want the best mask per box (index 0 usually has highest IoU)
    mask_list = []
    score_list = []

    if len(masks) > 0:
        batch_masks = masks[0]  # First (only) batch
        batch_scores = outputs.iou_scores[0]  # IoU scores

        for box_idx in range(batch_masks.shape[0]):
            # Get best mask for this box (highest IoU score)
            box_scores = batch_scores[box_idx].cpu().numpy()
            best_mask_idx = np.argmax(box_scores)
            best_score = float(box_scores[best_mask_idx])

            if best_score >= threshold:
                mask = batch_masks[box_idx, best_mask_idx].cpu().numpy().astype(np.uint8)
                mask_list.append(mask)
                score_list.append(best_score)

    print(f"SAM3 Tracker found {len(mask_list)} objects")

    return {
        "masks": mask_list,
        "scores": score_list,
        "count": len(mask_list)
    }


def detect_points(
    img_array: np.ndarray,
    pixel_points: List[List[int]],
    labels: List[int],
    threshold: float = 0.5,
    multi_object: bool = True
) -> Dict[str, Any]:
    """
    Detect objects at point locations using SAM3 Tracker model.

    Args:
        img_array: RGB image as numpy array (height, width, 3)
        pixel_points: List of point coordinates in PIXEL space [[col, row], ...]
        labels: List of labels (1 for foreground, 0 for background)
        threshold: Detection confidence threshold
        multi_object: If True, each point is a separate object. If False, all points
            refine a single object (use for refinement with pos/neg points).

    Returns:
        Dict with 'masks' (list of 2D numpy arrays) and 'scores' (list of floats)
    """
    global _TRACKER_MODEL, _TRACKER_PROCESSOR, _DEVICE

    if _TRACKER_MODEL is None:
        load_tracker_model()

    height, width = img_array.shape[:2]
    pil_image = Image.fromarray(img_array.astype(np.uint8))

    print(f"Running SAM3 Tracker with {len(pixel_points)} point prompt(s)")

    # Ensure labels is a list (reticulate may pass single int)
    if isinstance(labels, (int, np.integer)):
        labels = [labels]
    else:
        labels = list(labels)

    # Run inference with point prompts using tracker model
    # Sam3Tracker input_points: 4 levels [image_batch, object, points_per_object, coordinates]
    # Sam3Tracker input_labels: 3 levels [image_batch, object, labels_per_point]
    if multi_object:
        # Each point is a separate object (to segment multiple objects)
        # Format: [[[[p1]], [[p2]], [[p3]]]] = 3 objects, each with 1 point
        points_per_object = [[[p] for p in pixel_points]]
        labels_per_object = [[[int(l)] for l in labels]]
    else:
        # All points belong to ONE object (for refinement with pos/neg points)
        # Format: [[[p1, p2, p3]]] = 1 object with multiple points
        points_per_object = [[pixel_points]]
        labels_per_object = [[[int(l) for l in labels]]]

    inputs = _TRACKER_PROCESSOR(
        images=pil_image,
        input_points=points_per_object,
        input_labels=labels_per_object,
        return_tensors="pt"
    ).to(_DEVICE)

    with torch.no_grad():
        outputs = _TRACKER_MODEL(**inputs)

    # Post-process masks to original size using the processor
    masks = _TRACKER_PROCESSOR.post_process_masks(
        outputs.pred_masks.cpu(),
        inputs["original_sizes"]
    )[0]  # Get first (only) batch

    iou_scores = outputs.iou_scores[0]  # First batch

    print(f"Mask shape: {masks.shape}, Score shape: {iou_scores.shape}")
    print(f"multi_object: {multi_object}")

    mask_list = []
    score_list = []

    # Handle different output shapes based on multi_object mode
    if masks.dim() == 4:
        # (num_objects, num_masks_per_object, H, W)
        for obj_idx in range(masks.shape[0]):
            obj_scores = iou_scores[obj_idx].cpu().numpy()
            best_idx = np.argmax(obj_scores)
            best_score = float(obj_scores[best_idx])

            if best_score >= threshold:
                mask = masks[obj_idx, best_idx].cpu().numpy().astype(np.uint8)
                mask_list.append(mask)
                score_list.append(best_score)
    elif masks.dim() == 3:
        # (num_masks, H, W) - single object
        all_scores = iou_scores.cpu().numpy().flatten()
        best_idx = np.argmax(all_scores)
        best_score = float(all_scores[best_idx])

        if best_score >= threshold:
            mask = masks[best_idx].cpu().numpy().astype(np.uint8)
            mask_list.append(mask)
            score_list.append(best_score)

    print(f"SAM3 Tracker found {len(mask_list)} objects")

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
        "tracker_available": False,
        "model_loaded": is_model_loaded(),
        "tracker_loaded": is_tracker_loaded()
    }

    try:
        from transformers import Sam3Processor, Sam3Model
        info["transformers_available"] = True
    except ImportError:
        pass

    try:
        from transformers import Sam3TrackerProcessor, Sam3TrackerModel
        info["tracker_available"] = True
    except ImportError:
        pass

    return info

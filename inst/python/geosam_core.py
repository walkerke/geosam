"""
geosam Core Python Module

Provides SAM3 inference for geospatial image segmentation.
Designed to be called from R via reticulate.

Uses HuggingFace transformers for SAM3 model access.

NOTE: This module only handles SAM inference. All geospatial processing
(reading GeoTIFFs, converting masks to polygons) is done in R using terra.
"""

import os
from collections import OrderedDict
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
_TEXT_FEATURE_CACHE = OrderedDict()
_TEXT_FEATURE_CACHE_MAX = 64


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

    _MODEL = Sam3Model.from_pretrained("facebook/sam3").to(_DEVICE)
    _PROCESSOR = Sam3Processor.from_pretrained("facebook/sam3")

    return True


def _as_text_embeds(text_features):
    """Normalize text feature return values across transformers versions."""
    if hasattr(text_features, "pooler_output"):
        return text_features.pooler_output
    return text_features


def _get_cached_text_features(text_prompt: str):
    """Return cached SAM3 text embeddings and attention mask for a prompt."""
    global _MODEL, _PROCESSOR, _DEVICE, _TEXT_FEATURE_CACHE

    if _MODEL is None:
        load_model()

    cache_key = (_DEVICE, text_prompt)
    if cache_key in _TEXT_FEATURE_CACHE:
        _TEXT_FEATURE_CACHE.move_to_end(cache_key)
        return _TEXT_FEATURE_CACHE[cache_key]

    text_inputs = _PROCESSOR(text=text_prompt, return_tensors="pt").to(_DEVICE)
    with torch.no_grad():
        text_features = _MODEL.get_text_features(**text_inputs)
    _TEXT_FEATURE_CACHE[cache_key] = {
        "text_embeds": _as_text_embeds(text_features),
        "attention_mask": text_inputs.get("attention_mask", None)
    }
    while len(_TEXT_FEATURE_CACHE) > _TEXT_FEATURE_CACHE_MAX:
        _TEXT_FEATURE_CACHE.popitem(last=False)

    return _TEXT_FEATURE_CACHE[cache_key]


def _target_sizes_from_inputs(inputs, height: int, width: int):
    """Return processor original_sizes when available, otherwise image size."""
    original_sizes = inputs.get("original_sizes", None)
    if original_sizes is not None:
        return original_sizes.tolist()
    return [[height, width]]


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

    _TRACKER_MODEL = Sam3TrackerModel.from_pretrained("facebook/sam3").to(_DEVICE)
    _TRACKER_PROCESSOR = Sam3TrackerProcessor.from_pretrained("facebook/sam3")

    return True


def unload_model() -> bool:
    """Unload all models to free memory."""
    global _MODEL, _PROCESSOR, _TRACKER_MODEL, _TRACKER_PROCESSOR, _DEVICE, _TEXT_FEATURE_CACHE

    _MODEL = None
    _PROCESSOR = None
    _TRACKER_MODEL = None
    _TRACKER_PROCESSOR = None
    _DEVICE = None
    _TEXT_FEATURE_CACHE = OrderedDict()

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
    threshold: float = 0.5,
    use_text_cache: bool = False
) -> Dict[str, Any]:
    """
    Detect objects matching a text prompt using SAM3.

    Args:
        img_array: RGB image as numpy array (height, width, 3)
        text_prompt: Text description of objects to find
        threshold: Detection confidence threshold (0-1)
        use_text_cache: Experimental. If True, reuse cached text embeddings
            for the prompt. Off by default because the cached path passes
            `text_embeds` (pooler_output shape (B, H)) where the model
            documents `(B, L, H)`, and may produce different detections from
            the combined-processor call.

    Returns:
        Dict with 'masks' (list of 2D numpy arrays) and 'scores' (list of floats)
    """
    global _MODEL, _PROCESSOR, _DEVICE

    if _MODEL is None:
        load_model()

    height, width = img_array.shape[:2]
    pil_image = Image.fromarray(img_array.astype(np.uint8))

    if use_text_cache:
        img_inputs = _PROCESSOR(images=pil_image, return_tensors="pt").to(_DEVICE)
        text_features = _get_cached_text_features(text_prompt)
        model_args = {
            "pixel_values": img_inputs.pixel_values,
            "text_embeds": text_features["text_embeds"]
        }
        if text_features["attention_mask"] is not None:
            model_args["attention_mask"] = text_features["attention_mask"]

        with torch.no_grad():
            outputs = _MODEL(**model_args)
        target_sizes = _target_sizes_from_inputs(img_inputs, height, width)
    else:
        # Run inference with text prompt
        inputs = _PROCESSOR(images=pil_image, text=text_prompt, return_tensors="pt").to(_DEVICE)

        with torch.no_grad():
            outputs = _MODEL(**inputs)
        target_sizes = [[height, width]]

    results = _PROCESSOR.post_process_instance_segmentation(
        outputs,
        threshold=threshold,
        mask_threshold=0.5,
        target_sizes=target_sizes
    )[0]

    masks = results["masks"]
    scores = results["scores"]

    # Convert to numpy arrays for R
    mask_list = [mask.cpu().numpy().astype(np.uint8) for mask in masks]
    score_list = [float(s.cpu()) if hasattr(s, 'cpu') else float(s) for s in scores]

    return {
        "masks": mask_list,
        "scores": score_list,
        "count": len(mask_list),
        "prompt": text_prompt
    }


def detect_text_multi(
    img_array: np.ndarray,
    text_prompts: List[str],
    threshold: float = 0.5
) -> List[Dict[str, Any]]:
    """
    Run multiple text prompts on one image while reusing vision embeddings.

    Args:
        img_array: RGB image as numpy array (height, width, 3)
        text_prompts: Prompt strings to run on the same image
        threshold: Detection confidence threshold (0-1)

    Returns:
        List of result dicts, one per prompt.
    """
    global _MODEL, _PROCESSOR, _DEVICE

    if _MODEL is None:
        load_model()

    pil_image = Image.fromarray(img_array.astype(np.uint8))
    img_inputs = _PROCESSOR(images=pil_image, return_tensors="pt").to(_DEVICE)

    with torch.no_grad():
        vision_embeds = _MODEL.get_vision_features(pixel_values=img_inputs.pixel_values)

    height, width = img_array.shape[:2]
    target_sizes = _target_sizes_from_inputs(img_inputs, height, width)
    all_results = []

    for text_prompt in text_prompts:
        text_inputs = _PROCESSOR(text=text_prompt, return_tensors="pt").to(_DEVICE)
        with torch.no_grad():
            outputs = _MODEL(vision_embeds=vision_embeds, **text_inputs)

        results = _PROCESSOR.post_process_instance_segmentation(
            outputs,
            threshold=threshold,
            mask_threshold=0.5,
            target_sizes=target_sizes
        )[0]

        masks = results["masks"]
        scores = results["scores"]
        mask_list = [mask.cpu().numpy().astype(np.uint8) for mask in masks]
        score_list = [float(s.cpu()) if hasattr(s, 'cpu') else float(s) for s in scores]

        all_results.append({
            "masks": mask_list,
            "scores": score_list,
            "count": len(mask_list),
            "prompt": text_prompt
        })

    return all_results


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

    # Run inference with box prompts using tracker model
    inputs = _TRACKER_PROCESSOR(
        images=pil_image,
        input_boxes=[pixel_boxes],
        return_tensors="pt"
    ).to(_DEVICE)

    with torch.no_grad():
        outputs = _TRACKER_MODEL(**inputs)

    # Post-process masks to original size using the processor
    masks = _TRACKER_PROCESSOR.post_process_masks(
        outputs.pred_masks.cpu(),
        inputs["original_sizes"]
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

    return {
        "masks": mask_list,
        "scores": score_list,
        "count": len(mask_list)
    }


def detect_exemplar(
    img_array: np.ndarray,
    pixel_box: List[int],
    threshold: float = 0.5
) -> Dict[str, Any]:
    """
    Find all objects similar to the exemplar using SAM3 model.

    This uses the main Sam3Model (not Tracker) with input_boxes and positive labels
    to find all instances matching the visual concept within the exemplar box.

    Args:
        img_array: RGB image as numpy array (height, width, 3)
        pixel_box: Bounding box of exemplar in PIXEL coordinates [xmin, ymin, xmax, ymax]
        threshold: Detection confidence threshold (0-1)

    Returns:
        Dict with 'masks' (list of 2D numpy arrays) and 'scores' (list of floats)
    """
    global _MODEL, _PROCESSOR, _DEVICE

    if _MODEL is None:
        load_model()

    height, width = img_array.shape[:2]
    pil_image = Image.fromarray(img_array.astype(np.uint8))

    # Use input_boxes with positive label (1) for exemplar-based detection
    # This tells SAM3 "find all objects similar to what's in this box"
    input_boxes = [[pixel_box]]  # [batch, num_boxes, 4]
    input_boxes_labels = [[1]]   # 1 = positive exemplar

    inputs = _PROCESSOR(
        images=pil_image,
        input_boxes=input_boxes,
        input_boxes_labels=input_boxes_labels,
        return_tensors="pt"
    ).to(_DEVICE)

    with torch.no_grad():
        outputs = _MODEL(**inputs)

    # Post-process results
    results = _PROCESSOR.post_process_instance_segmentation(
        outputs,
        threshold=threshold,
        mask_threshold=0.5,
        target_sizes=[[height, width]]
    )[0]

    masks = results["masks"]
    scores = results["scores"]

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

    return {
        "masks": mask_list,
        "scores": score_list,
        "count": len(mask_list)
    }


def clear_cache() -> None:
    """
    Clear GPU/MPS memory cache.

    Call this after processing each tile in chunked detection to prevent
    memory buildup across tiles.
    """
    import gc
    gc.collect()

    if torch.cuda.is_available():
        torch.cuda.empty_cache()

    # MPS doesn't have empty_cache, but gc.collect helps
    if torch.backends.mps.is_available():
        # Force synchronization to ensure operations complete
        if _DEVICE == "mps":
            torch.mps.synchronize()


def check_model_access(model_id: str = "facebook/sam3") -> Dict[str, Any]:
    """
    Check whether HuggingFace credentials can access the SAM3 model metadata.

    This is intentionally metadata-only; it does not download model weights.
    """
    info = {
        "model_id": model_id,
        "huggingface_hub_available": False,
        "hf_token_set": bool(os.environ.get("HF_TOKEN") or os.environ.get("HUGGING_FACE_HUB_TOKEN")),
        "cached_token_set": False,
        "access_ok": False,
        "classification": "unknown",
        "message": None,
        "cache_path": None,
        "model_cached": False
    }

    try:
        from huggingface_hub import HfApi
        try:
            from huggingface_hub import get_token
        except ImportError:
            get_token = None
        try:
            from huggingface_hub import scan_cache_dir
        except ImportError:
            scan_cache_dir = None
    except ImportError as exc:
        info["classification"] = "missing_huggingface_hub"
        info["message"] = str(exc)
        return info

    info["huggingface_hub_available"] = True

    cached_token = get_token() if get_token is not None else None
    token = os.environ.get("HF_TOKEN") or os.environ.get("HUGGING_FACE_HUB_TOKEN") or cached_token
    info["cached_token_set"] = bool(cached_token)
    info["hf_token_set"] = bool(token)

    try:
        cache_info = scan_cache_dir() if scan_cache_dir is not None else None
        if cache_info is not None:
            info["cache_path"] = str(cache_info.cache_dir)
            info["model_cached"] = any(
                getattr(repo, "repo_id", None) == model_id
                for repo in getattr(cache_info, "repos", [])
            )
    except Exception:
        pass

    if not token:
        info["classification"] = "missing_token"
        info["message"] = "No HuggingFace token found. Log in with huggingface-cli or set HF_TOKEN."
        return info

    try:
        HfApi().model_info(model_id, token=token)
        info["access_ok"] = True
        info["classification"] = "ok"
        info["message"] = "HuggingFace access verified."
    except Exception as exc:
        # HuggingFace error messages aren't a stable contract; this is a
        # best-effort classification used only for friendlier diagnostics.
        message = str(exc)
        message_lower = message.lower()
        info["message"] = message
        if "gated" in message_lower or "restricted" in message_lower or "403" in message_lower:
            info["classification"] = "gated_not_accepted"
        elif "401" in message_lower or "unauthorized" in message_lower or "invalid token" in message_lower:
            info["classification"] = "invalid_token"
        elif (
            "connection" in message_lower
            or "timeout" in message_lower
            or "name or service not known" in message_lower
            or "nodename nor servname provided" in message_lower
        ):
            info["classification"] = "network_error"
        else:
            info["classification"] = "access_error"

    return info


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
        "tracker_loaded": is_tracker_loaded(),
        "text_feature_cache_size": len(_TEXT_FEATURE_CACHE)
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

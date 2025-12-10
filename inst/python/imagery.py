"""
Satellite Imagery Download Module

Downloads and merges satellite tiles from various sources
into georeferenced GeoTIFF files.
"""

import os
import math
from typing import List, Optional, Tuple
from pathlib import Path

import numpy as np
from PIL import Image
import requests


def lng_lat_to_tile(lng: float, lat: float, zoom: int) -> Tuple[int, int]:
    """Convert longitude/latitude to tile coordinates."""
    n = 2 ** zoom
    x = int((lng + 180.0) / 360.0 * n)
    lat_rad = math.radians(lat)
    y = int((1.0 - math.asinh(math.tan(lat_rad)) / math.pi) / 2.0 * n)
    return x, y


def tile_to_lng_lat(x: int, y: int, zoom: int) -> Tuple[float, float]:
    """Convert tile coordinates to longitude/latitude (NW corner)."""
    n = 2 ** zoom
    lng = x / n * 360.0 - 180.0
    lat_rad = math.atan(math.sinh(math.pi * (1 - 2 * y / n)))
    lat = math.degrees(lat_rad)
    return lng, lat


def download_tile(x: int, y: int, zoom: int, source: str, api_key: Optional[str] = None) -> Optional[Image.Image]:
    """Download a single tile from the specified source."""

    if source == "mapbox":
        if not api_key:
            raise ValueError("Mapbox API key required")
        url = f"https://api.mapbox.com/v4/mapbox.satellite/{zoom}/{x}/{y}@2x.jpg?access_token={api_key}"
    elif source == "esri":
        url = f"https://server.arcgisonline.com/ArcGIS/rest/services/World_Imagery/MapServer/tile/{zoom}/{y}/{x}"
    elif source == "google":
        url = f"https://mt1.google.com/vt/lyrs=s&x={x}&y={y}&z={zoom}"
    else:
        raise ValueError(f"Unknown source: {source}")

    try:
        response = requests.get(url, timeout=30)
        response.raise_for_status()
        return Image.open(requests.io.BytesIO(response.content))
    except Exception as e:
        print(f"Failed to download tile {x},{y}: {e}")
        return None


def download_tiles(
    bbox: List[float],
    output_path: str,
    source: str = "mapbox",
    zoom: int = 17,
    api_key: Optional[str] = None
) -> str:
    """
    Download and merge satellite tiles for a bounding box.

    Args:
        bbox: [west, south, east, north] in EPSG:4326
        output_path: Path for output GeoTIFF
        source: Tile source ("mapbox", "esri", "google")
        zoom: Tile zoom level (17-19 recommended)
        api_key: API key for the tile source (required for mapbox)

    Returns:
        Path to output GeoTIFF
    """
    import rasterio
    from rasterio.transform import from_bounds

    west, south, east, north = bbox

    # Get tile range
    x_min, y_max = lng_lat_to_tile(west, north, zoom)
    x_max, y_min = lng_lat_to_tile(east, south, zoom)

    # Ensure proper order
    x_min, x_max = min(x_min, x_max), max(x_min, x_max)
    y_min, y_max = min(y_min, y_max), max(y_min, y_max)

    print(f"Downloading tiles: x={x_min}-{x_max}, y={y_min}-{y_max} (zoom={zoom})")

    # Download and merge tiles
    tile_size = 512 if source == "mapbox" else 256  # Mapbox @2x tiles are 512px
    width = (x_max - x_min + 1) * tile_size
    height = (y_max - y_min + 1) * tile_size

    merged = Image.new('RGB', (width, height))

    for x in range(x_min, x_max + 1):
        for y in range(y_min, y_max + 1):
            tile = download_tile(x, y, zoom, source, api_key)
            if tile:
                # Resize if needed
                if tile.size != (tile_size, tile_size):
                    tile = tile.resize((tile_size, tile_size))

                px = (x - x_min) * tile_size
                py = (y - y_min) * tile_size
                merged.paste(tile, (px, py))

    # Calculate bounds in Web Mercator
    nw_lng, nw_lat = tile_to_lng_lat(x_min, y_min, zoom)
    se_lng, se_lat = tile_to_lng_lat(x_max + 1, y_max + 1, zoom)

    # Convert to Web Mercator (EPSG:3857)
    def lng_lat_to_mercator(lng: float, lat: float) -> Tuple[float, float]:
        x = lng * 20037508.34 / 180
        y = math.log(math.tan((90 + lat) * math.pi / 360)) / (math.pi / 180)
        y = y * 20037508.34 / 180
        return x, y

    west_m, north_m = lng_lat_to_mercator(nw_lng, nw_lat)
    east_m, south_m = lng_lat_to_mercator(se_lng, se_lat)

    # Create transform
    transform = from_bounds(west_m, south_m, east_m, north_m, width, height)

    # Save as GeoTIFF
    os.makedirs(os.path.dirname(output_path) or ".", exist_ok=True)

    img_array = np.array(merged)

    with rasterio.open(
        output_path,
        'w',
        driver='GTiff',
        height=height,
        width=width,
        count=3,
        dtype=img_array.dtype,
        crs='EPSG:3857',
        transform=transform
    ) as dst:
        for i in range(3):
            dst.write(img_array[:, :, i], i + 1)

    print(f"Saved: {output_path} ({width}x{height})")

    return output_path



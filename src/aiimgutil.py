"""
Image processing utilities for annotating NetLogo simulation outputs.

This module provides functions to:
1.  Generate an image containing the parameters of a specific simulation run.
2.  Combine this parameter image with the corresponding visual output from the
    NetLogo simulation (the 'view').

It relies on the Pillow library for image manipulation and the previously defined
`ExperimentParser` to access simulation data.
"""

from pathlib import Path
from typing import Union

from PIL import Image, ImageDraw, ImageFont

# Assuming the previously refactored parser class is available.
from aiparse import ExperimentParser


def create_parameter_image(
    experiment_parser: ExperimentParser,
    run_number: int,
    font_path: str = "/usr/share/fonts/truetype/dejavu/DejaVuSans-Bold.ttf",
    image_size: tuple[int, int] = (300, 800),
    font_size: int = 14,
) -> Image.Image:
    """
    Generates an image listing the parameters for a given simulation run.

    Args:
        experiment_parser: An `ExperimentParser` object containing the loaded
                           experiment data.
        run_number: The specific run number to display parameters for.
        font_path: Path to the .ttf font file to use.
        image_size: A tuple (width, height) for the output image.
        font_size: The font size for the parameter text.

    Returns:
        A PIL Image object containing the formatted parameter text.

    Raises:
        FileNotFoundError: If the specified font file does not exist.
        ValueError: If the run number is not found in the experiment data.
        KeyError: If a run number column isn't found in the DataFrame.
    """
    if experiment_parser.data is None:
        raise ValueError("Experiment data has not been parsed or is empty.")

    df = experiment_parser.data

    # *** DEBUGGED SECTION ***
    # NetLogo BehaviorSpace uses '[run number]'. We also check for 'run' as a fallback.
    run_column_name = '[run number]'
    if run_column_name not in df.columns:
        run_column_name = 'run'  # Fallback to 'run'
        if run_column_name not in df.columns:
            raise KeyError("DataFrame is missing a '[run number]' or 'run' column.")

    # Filter the DataFrame to find the row for the specific run number.
    run_df = df[df[run_column_name] == run_number]

    if run_df.empty:
        raise ValueError(f"Run number {run_number} not found in the '{run_column_name}' column.")

    # Select the first (and should be only) row as a pandas Series.
    run_data = run_df.iloc[0].round(2)

    # Prepare the text content
    text_lines = [
        f"Experiment: {experiment_parser.file_path.stem}",
        f"Run: {run_number}",
        "---",
    ]

    # Iterate over the Series. `param` is the index (column name) and `value` is the scalar value.
    for param, value in run_data.items():
        text_lines.append(f"{param} = {value}")

    text_content = "\n".join(text_lines)

    # Load font
    if not Path(font_path).exists():
         # Provide a fallback font path for different systems if needed
        font_path = "/System/Library/Fonts/Supplemental/Arial.ttf"
        if not Path(font_path).exists():
            raise FileNotFoundError(f"Font file not found at {font_path} or fallback.")

    font = ImageFont.truetype(font_path, font_size)

    # Create image
    image = Image.new("RGB", image_size, "white")
    draw = ImageDraw.Draw(image)
    draw.text((10, 10), text_content, fill="black", font=font)

    return image


def combine_images_horizontally(
    image_path_left: Union[str, Path],
    image_right: Image.Image
) -> Image.Image:
    """
    Combines two images by placing them side-by-side.

    The height of the new image will match the tallest of the two images.

    Args:
        image_path_left: The file path to the left image (e.g., the simulation view).
        image_right: The PIL Image object for the right image (e.g., the parameter list).

    Returns:
        A new PIL Image object representing the combined image.
    """
    image_left = Image.open(image_path_left)
    images = [image_left, image_right]

    widths, heights = zip(*(i.size for i in images))
    total_width = sum(widths)
    max_height = max(heights)

    combined_image = Image.new("RGB", (total_width, max_height), "white")

    x_offset = 0
    for img in images:
        combined_image.paste(img, (x_offset, 0))
        x_offset += img.size[0]

    return combined_image

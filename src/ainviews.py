#!/usr/bin/env python3
"""
Generates an HTML file to display NetLogo simulation output images.

This script scans a specified experiment's 'views' directory for image files
and creates an HTML gallery page for easy viewing. It can either link to the
images or embed them directly into the HTML file using base64 encoding.

Usage:
    python html_generator.py /path/to/experiment_directory --type viewdata --embed

Arguments:
    exp_dir: The path to the experiment's base directory.

Options:
    --type [view|viewdata|overlay]: Specifies the type of images to process.
        - view:     Process 'view*.png' images (raw simulation output).
        - viewdata: Process 'viewdata*.png' images (annotated with parameters).
        - overlay:  Process 'v-*.png' images.
    
    --embed: If set, embeds images directly into the HTML file. By default,
             the script links to the image files.
    
    --no-link: If set, displays the image without a hyperlink wrapper.
"""
import argparse
import base64
from pathlib import Path
from typing import Callable, List


def generate_html_linked_image(file_path: Path) -> str:
    """Creates an HTML snippet for a linked image."""
    relative_path = f"views/{file_path.name}"
    return (
        f'<a href="{relative_path}" target="_blank">'
        f'<img style="border:2px solid black;" title="{file_path.name}" '
        f'src="{relative_path}" loading="lazy" height="500" width="500">'
        f'</a>'
    )


def generate_html_standalone_image(file_path: Path) -> str:
    """Creates an HTML snippet for a standalone image without a link."""
    relative_path = f"views/{file_path.name}"
    return (
        f'<img style="border:2px solid black;" title="{file_path.name}" '
        f'src="{relative_path}" loading="lazy" height="500" width="500">'
    )


def generate_html_embedded_image(file_path: Path) -> str:
    """Creates an HTML snippet for an image embedded using base64."""
    try:
        with open(file_path, "rb") as f:
            data_uri = base64.b64encode(f.read()).decode("utf-8")
        
        # When embedded, the link should also point to the original file if possible
        relative_path = f"views/{file_path.name}"
        return (
            f'<a href="{relative_path}" target="_blank">'
            f'<img src="data:image/png;base64,{data_uri}" title="{file_path.name}" '
            f'style="border:2px solid black;" width="500" height="500">'
            f'</a>'
        )
    except FileNotFoundError:
        return f"<!-- Could not find file to embed: {file_path.name} -->"


def create_html_gallery(image_paths: List[Path], generator_func: Callable[[Path], str]) -> str:
    """
    Creates the full HTML document for the image gallery.
    
    Args:
        image_paths: A list of Path objects for the images to include.
        generator_func: The function to use for generating each image's HTML.

    Returns:
        A string containing the full HTML document.
    """
    image_tags = [generator_func(path) for path in image_paths]
    
    body_content = "\n".join(image_tags)
    
    return f"""<!DOCTYPE html>
<html lang="en">
<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>Image Gallery</title>
    <style>
        body {{ font-family: sans-serif; display: flex; flex-wrap: wrap; gap: 15px; }}
        img {{ object-fit: contain; }}
    </style>
</head>
<body>
    {body_content}
</body>
</html>
"""

def main():
    """Main function to parse arguments and generate the HTML file."""
    parser = argparse.ArgumentParser(description="Generate an HTML gallery for experiment images.")
    parser.add_argument(
        "exp_dir",
        type=str,
        help="The path to the experiment's base directory."
    )
    
    group = parser.add_mutually_exclusive_group(required=True)
    group.add_argument("--type", choices=['view', 'viewdata', 'overlay'], help="Type of images to process.")
    
    parser.add_argument("--embed", action='store_true', help="Embed images directly in the HTML file.")
    parser.add_argument("--no-link", action='store_true', help="Display images without a hyperlink.")

    args = parser.parse_args()

    # Determine file pattern and output filename based on type
    image_patterns = {
        'view': "view[0-9]*.png",
        'viewdata': "viewdata*.png",
        'overlay': "v-*.png"
    }
    output_filenames = {
        'view': 'views.html',
        'viewdata': 'views_data.html',
        'overlay': 'views_overlay.html'
    }

    pattern = image_patterns[args.type]
    output_filename = output_filenames[args.type]
    
    # Determine which HTML generation function to use
    if args.embed:
        html_generator = generate_html_embedded_image
    elif args.no_link:
        html_generator = generate_html_standalone_image
    else:
        html_generator = generate_html_linked_image

    # Find the image files
    experiment_dir = Path(args.exp_dir)
    views_dir = experiment_dir / "views"

    if not views_dir.is_dir():
        print(f"Error: 'views' subdirectory not found in '{experiment_dir}'")
        return
        
    image_files = sorted(views_dir.glob(pattern))

    if not image_files:
        print(f"No images found in '{views_dir}' matching the pattern '{pattern}'.")
        return

    print(f"Found {len(image_files)} images of type '{args.type}'. Generating '{output_filename}'...")

    # Generate the HTML content
    html_content = create_html_gallery(image_files, html_generator)

    # Write the output file
    output_path = experiment_dir / output_filename
    with open(output_path, 'w') as f:
        f.write(html_content)
        
    print(f"Successfully created HTML file at: {output_path}")


if __name__ == "__main__":
    main()


#!/usr/bin/env python3
"""
Batch processor to annotate NetLogo simulation views with their parameters.

This script iterates through all simulation view images (e.g., 'view1.png',
'view2.png', etc.) in a specified directory. For each view, it:
1.  Loads the corresponding experiment parameter data.
2.  Generates an image listing the parameters for that specific run.
3.  Stitches the simulation view and the parameter image together.
4.  Saves the new, combined image.

Usage:
    python batch_image_processor.py /path/to/experiment/directory
"""

import argparse
import re
import sys
from pathlib import Path

# Assuming the refactored utilities and parsers are available.
from aiimgutil import create_parameter_image, combine_images_horizontally
from aiparse import ExperimentParser

def main():
    """
    Main function to parse arguments and orchestrate the image processing workflow.
    """
    parser = argparse.ArgumentParser(
        description="Combine NetLogo view images with their corresponding run parameters."
    )
    parser.add_argument(
        "exp_dir",
        type=str,
        help="The path to the experiment's base directory.",
    )
    args = parser.parse_args()

    experiment_dir = Path(args.exp_dir)
    views_dir = experiment_dir / "views"

    # Validate paths
    if not experiment_dir.is_dir():
        print(f"Error: Experiment directory not found at '{experiment_dir}'")
        return
    if not views_dir.is_dir():
        print(f"Error: 'views' subdirectory not found in '{experiment_dir}'")
        return

    # Load the main experiment data from its pre-processed pickle file
    exp_pickle_path = experiment_dir / f"{experiment_dir.name}.pkl"
    if not exp_pickle_path.exists():
        print(f"Error: Processed experiment file not found at '{exp_pickle_path}'.")
        print("Please run the parallel_processor.py script first.")
        return
    
    print("Loading experiment data...")
    experiment_parser = ExperimentParser.restore(str(exp_pickle_path))
    print("Experiment data loaded.")

    # Find all 'view*.png' files in the views directory
    view_files = sorted(views_dir.glob("view*.png"))
    if not view_files:
        print(f"No view images found in '{views_dir}'.")
        return

    print(f"Found {len(view_files)} images to process...")

    for view_path in view_files:
        # Extract the run number from the filename (e.g., 'view42.png' -> 42)
        match = re.search(r"view(\d+)\.png", view_path.name)
        if not match:
            continue
        
        run_number = int(match.group(1))
        print(f"Processing run {run_number} (image: {view_path.name})...")

        try:
            # 1. Create the image with parameter data
            param_image = create_parameter_image(experiment_parser, run_number)

            # 2. Combine it with the simulation view
            combined_image = combine_images_horizontally(view_path, param_image)

            # 3. Save the new, annotated image
            output_path = views_dir / f"viewdata{run_number}.png"
            combined_image.save(output_path)
            print(f"  -> Saved combined image to '{output_path.name}'")

        except (ValueError, FileNotFoundError) as e:
            print(f"  -> Skipping run {run_number} due to an error: {e}")
        except Exception as e:
            print(f"  -> An unexpected error occurred on run {run_number}: {e}")

    print("\nBatch processing complete.")


if __name__ == "__main__":
    main()


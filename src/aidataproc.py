#!/usr/bin/env python3
"""
Parallel processing script for NetLogo simulation data.

This script automates the process of parsing and preprocessing raw data files
from a NetLogo experiment. It is designed to work with the `ExperimentParser`
and `WorldParser` classes from the `netlogo_parser` module.

The script performs the following steps:
1.  Parses the main BehaviorSpace experiment file to get an overview of the runs.
2.  Uses a multiprocessing pool to concurrently process each individual 'world' 
    data file from the experiment's 'plots' directory.
3.  For each world file, it creates a `WorldParser` instance, parses the data,
    and stores the resulting object as a serialized pickle file for fast access later.

This parallel approach significantly speeds up the data preparation stage for
large experiments with many runs.
"""

import argparse
import subprocess
from pathlib import Path
from multiprocessing import Pool

# Assuming the previously refactored parser classes are in a file named `netlogo_parser`.
from aiparse import ExperimentParser, WorldParser

def process_world_file(file_path: str):
    """
    Worker function to parse, process, and store a single world data file.

    This function is intended to be called by a multiprocessing pool. It takes
    a file path, uses the WorldParser to process it, and saves the parsed
    object to a .pkl file in the same directory.

    Args:
        file_path (str): The full path to the 'world*.csv' file to process.
    """
    try:
        print(f"Processing: {file_path}")
        parser = WorldParser(file_path)
        parser.parse()
        # The timeadj method was part of the old design. If you still need
        # time adjustments, that logic should be integrated into your analysis
        # or the parser itself. For now, we'll assume parsing is sufficient.
        parser.store()
        print(f"Successfully stored: {file_path}.pkl")
    except Exception as e:
        print(f"Error processing {file_path}: {e}")

def main():
    """
    Main function to orchestrate the parallel processing of an experiment.
    """
    parser = argparse.ArgumentParser(
        description="Parse and preprocess NetLogo experiment data in parallel."
    )
    parser.add_argument(
        "exp_dir",
        type=str,
        help="The path to the experiment's base directory."
    )
    args = parser.parse_args()

    experiment_directory = Path(args.exp_dir)
    
    # 1. Process the main experiment file first
    exp_file = experiment_directory / f"{experiment_directory.name}.csv"
    if not exp_file.exists():
        print(f"Error: Main experiment file not found at {exp_file}")
        return

    print(f"Processing main experiment file: {exp_file}")
    try:
        ep = ExperimentParser(str(exp_file))
        ep.parse()
        ep.store()
        print(f"Successfully stored main experiment data: {exp_file}.pkl")
    except Exception as e:
        print(f"Failed to process main experiment file: {e}")
        return

    # 2. Find all world files to be processed in parallel
    plots_dir = experiment_directory / "plots"
    if not plots_dir.is_dir():
        print(f"Error: 'plots' subdirectory not found in {experiment_directory}")
        return

    try:
        # Use a more robust method to find files
        find_command = f"find {plots_dir} -name 'world*.csv'"
        result = subprocess.run(find_command, shell=True, capture_output=True, text=True)
        if result.returncode != 0:
            print(f"Error finding world files: {result.stderr}")
            return
        world_files = result.stdout.strip().split('\n')
        if not world_files or not world_files[0]:
            print("No 'world*.csv' files found to process.")
            return
    except Exception as e:
        print(f"An error occurred while searching for world files: {e}")
        return

    # 3. Use a multiprocessing pool to process world files
    print(f"\nFound {len(world_files)} world files to process. Starting parallel processing...")
    with Pool() as pool:
        pool.map(process_world_file, world_files)

    print("\nAll processing complete.")

if __name__ == "__main__":
    main()


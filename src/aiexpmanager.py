"""
A module for managing and analyzing a full NetLogo experiment dataset.

This class acts as a high-level interface to an experiment's output, 
combining the BehaviorSpace summary with individual world/plot data from each run.
"""
import re
from pathlib import Path
from typing import Dict, List, Optional, Union

import pandas as pd

# Assuming the previously refactored parser classes are in a file named `netlogo_parser`.
# If they are in the same file, you can remove these imports.
from aiparse import ExperimentParser, WorldParser


class ExperimentManager:
    """
    Manages, aggregates, and provides access to a complete NetLogo experiment's data.

    This class links a main BehaviorSpace experiment file with all the individual
    world data dumps generated during each run. It provides methods to easily
    access and combine this data for analysis.

    Attributes:
        name (str): The name of the experiment, derived from the base directory.
        base_dir (Path): The root directory of the experiment.
        description (str): The content of the experiment's description file.
        experiment_parser (ExperimentParser): The parsed BehaviorSpace experiment data.
        worlds (Dict[int, WorldParser]): A cache for lazily-loaded world data parsers,
                                         keyed by run number.
    """
    def __init__(self, experiment_base_dir: str):
        """
        Initializes the manager for a given experiment directory.

        Args:
            experiment_base_dir (str): The path to the experiment's root directory.
                                       This directory should contain the main experiment
                                       CSV, a '.desc' file, and a 'plots' subdirectory.

        Raises:
            FileNotFoundError: If the required directories or files are not found.
        """
        self.base_dir = Path(experiment_base_dir)
        self.name = self.base_dir.name
        self.worlds: Dict[int, WorldParser] = {}

        # Validate directory structure
        self._validate_paths()
        
        # Load the main experiment data from its pickle file
        pickle_path = self.base_dir / f"{self.name}.pkl"
        if not pickle_path.exists():
             raise FileNotFoundError(
                f"Pickle file not found: {pickle_path}. "
                f"Please parse and store the ExperimentParser first."
            )
        self.experiment_parser = ExperimentParser.restore(str(pickle_path))
        
        # Load the experiment description
        with open(self.base_dir / f"{self.name}.desc", 'r') as f:
            self.description = f.read()

    def _validate_paths(self):
        """Checks for the existence of required files and directories."""
        if not self.base_dir.is_dir():
            raise FileNotFoundError(f"Experiment directory not found: {self.base_dir}")
        if not (self.base_dir / 'plots').is_dir():
            raise FileNotFoundError(f"Subdirectory 'plots' not found in {self.base_dir}")
        if not (self.base_dir / f"{self.name}.desc").is_file():
            raise FileNotFoundError(f"Description file not found: {self.name}.desc")

    def get_world(self, run_number: int) -> WorldParser:
        """
        Retrieves the parsed world data for a specific run number.

        This method uses lazy loading: the world data file is only parsed
        the first time it is requested. Subsequent calls for the same run
        number will return a cached object.

        Args:
            run_number (int): The run number to load.

        Returns:
            WorldParser: The parsed world data object.
        
        Raises:
            FileNotFoundError: If the pickle file for the requested run does not exist.
        """
        if run_number not in self.worlds:
            pickle_file = self.base_dir / 'plots' / f"world{run_number}.csv.pkl"
            if not pickle_file.is_file():
                raise FileNotFoundError(
                    f"Pickle file for run {run_number} not found at {pickle_file}. "
                    "Please ensure all world files have been parsed and stored."
                )
            self.worlds[run_number] = WorldParser.restore(str(pickle_file))
        return self.worlds[run_number]

    @property
    def experiment_data(self) -> pd.DataFrame:
        """Returns the main DataFrame from the BehaviorSpace experiment."""
        if self.experiment_parser.data is None:
            raise ValueError("Experiment data not parsed. Call .parse() on the ExperimentParser.")
        return self.experiment_parser.data
        
    @property
    def fixed_parameters(self) -> pd.Series:
        """Returns a Series of parameters that were fixed during the experiment."""
        if self.experiment_parser.fixed_parameters is None:
             raise ValueError("Fixed parameters not available. Call .parse() on the ExperimentParser.")
        return self.experiment_parser.fixed_parameters

    @property
    def variable_parameters(self) -> pd.DataFrame:
        """Returns a DataFrame summarizing parameters that varied."""
        if self.experiment_parser.variable_parameters is None:
            raise ValueError("Variable parameters not available. Call .parse() on the ExperimentParser.")
        return self.experiment_parser.variable_parameters

    @property
    def number_of_runs(self) -> int:
        """Returns the total number of runs in the experiment."""
        return len(self.experiment_data.index)

    def get_plot_names(self, sample_run_number: int = 1) -> List[str]:
        """
        Gets a list of all available plot names from a sample run.

        Args:
            sample_run_number (int): The run number to use for sampling plot names.

        Returns:
            List[str]: A list of plot names available in the world data.
        """
        sample_world = self.get_world(sample_run_number)
        return list(sample_world.plots.keys())

    def get_aggregated_plot_data(
        self, 
        plot_name: str, 
        run_numbers: Optional[List[int]] = None
    ) -> pd.DataFrame:
        """
        Aggregates data for a specific plot from multiple runs into a single DataFrame.

        Args:
            plot_name (str): The name of the plot to retrieve data for.
            run_numbers (Optional[List[int]]): A list of run numbers to include.
                If None, data from all runs in the experiment will be aggregated.

        Returns:
            pd.DataFrame: A single DataFrame containing the concatenated plot
                          data from all specified runs. Returns an empty DataFrame
                          if no data is found.
        """
        if run_numbers is None:
            run_numbers = list(range(1, self.number_of_runs + 1))
        
        data_frames = []
        for run in run_numbers:
            world = self.get_world(run)
            if plot_name in world.plots and world.plots[plot_name].data is not None:
                df = world.plots[plot_name].data.copy()
                # The 'run' column should already be in the WorldParser data,
                # but we ensure it's there.
                if 'run' not in df.columns:
                    df['run'] = run
                data_frames.append(df)
        
        if not data_frames:
            return pd.DataFrame()

        return pd.concat(data_frames, ignore_index=True)

    def __repr__(self) -> str:
        """Provides a developer-friendly representation of the object."""
        return (
            f"<ExperimentManager(name='{self.name}', "
            f"runs={self.number_of_runs})>"
        )


"""
A module for parsing output files from NetLogo simulations.

This module provides classes for parsing different types of NetLogo output:
- World/Plot dumps
- BehaviorSpace experiments

The parsers extract data into pandas DataFrames and provide a clean, 
object-oriented interface to access the simulation results.
"""

import re
import pickle
from io import StringIO
from pathlib import Path
from typing import TextIO, Dict, Any, List, Optional, Tuple

import pandas as pd

# Default rounding factor for floating-point data in DataFrames.
DEFAULT_ROUNDING_FACTOR = 7


class NetLogoParser:
    """
    Abstract base class for NetLogo output file parsers.

    This class provides common functionality for file handling, data serialization
    (saving/loading parsed data), and a consistent interface for subclasses.

    Attributes:
        file_path (Path): The path to the input file.
        rounding_factor (int): The number of decimal places to round numeric data to.
        version (Optional[str]): The NetLogo version used for the simulation.
        model_file (Optional[str]): The path to the .nlogo model file.
        timestamp (Optional[str]): The timestamp of when the simulation run started.
    """

    def __init__(self, file_path: str, rounding_factor: int = DEFAULT_ROUNDING_FACTOR):
        """
        Initializes the parser with the path to the data file.

        Args:
            file_path (str): The path to the NetLogo output file.
            rounding_factor (int): The number of decimal places for rounding data.
        """
        self.file_path = Path(file_path)
        if not self.file_path.is_file():
            raise FileNotFoundError(f"The file {file_path} was not found.")
        self.rounding_factor = rounding_factor
        self.version: Optional[str] = None
        self.model_file: Optional[str] = None
        self.timestamp: Optional[str] = None

    def parse(self):
        """
        Abstract method to parse the file. Subclasses must implement this.
        """
        raise NotImplementedError("Each parser subclass must implement the 'parse' method.")

    @property
    def run_number(self) -> int:
        """
        Extracts the run number from the filename.

        Assumes a filename format like '...[type][number].csv', 
        e.g., 'world1.csv' or 'plot-42.csv'.

        Returns:
            int: The extracted run number, or 0 if not found.
        """
        match = re.search(r'(\d+)', self.file_path.stem)
        return int(match.group(1)) if match else 0

    def _read_block(self, file_handle: TextIO) -> str:
        """
        Reads a block of text from the file until a blank line is encountered.

        Args:
            file_handle (TextIO): The file object to read from.

        Returns:
            str: A string containing the lines of the block.
        """
        lines = []
        for line in file_handle:
            if not line.strip():
                break
            lines.append(line)
        return "".join(lines)

    def _parse_header(self, file_handle: TextIO):
        """
        Parses the common header section of a NetLogo output file.

        Args:
            file_handle (TextIO): The file object to read from, positioned at the start.
        """
        self.version = re.sub(r'^.*(Net.*)\).*', r'\g<1>', file_handle.readline()).strip()
        self.model_file = file_handle.readline().strip()
        self.timestamp = file_handle.readline().strip()

    def store(self, destination_path: Optional[str] = None):
        """
        Serializes and saves the parsed object to a pickle file.

        Args:
            destination_path (Optional[str]): The path to save the pickle file to.
                If None, defaults to the original filename with a .pkl extension.
        """
        if destination_path:
            pfile = Path(destination_path)
        else:
            pfile = self.file_path.with_suffix('.pkl')
        
        with open(pfile, 'wb') as pf:
            pickle.dump(self, pf, protocol=pickle.HIGHEST_PROTOCOL)

    @classmethod
    def restore(cls, source_path: str) -> 'NetLogoParser':
        """
        Loads a parsed object from a pickle file.

        Args:
            source_path (str): The path to the pickle file.

        Returns:
            An instance of the parser class with data loaded.
        """
        with open(source_path, 'rb') as pf:
            return pickle.load(pf)

    def __repr__(self) -> str:
        """Provides a developer-friendly representation of the object."""
        return f"<{self.__class__.__name__}(file='{self.file_path.name}')>"


class PlotData:
    """
    A data container for a single NetLogo plot.

    Attributes:
        name (str): The name of the plot.
        state (Optional[pd.DataFrame]): DataFrame containing the plot's state (e.g., axes min/max).
        status (Optional[pd.DataFrame]): DataFrame containing the plot's status (e.g., mode).
        data (Optional[pd.DataFrame]): The main DataFrame of the plot's time-series data.
        variables (List[str]): A list of variable names included in the plot.
    """
    def __init__(self, name: str):
        self.name = name
        self.state: Optional[pd.DataFrame] = None
        self.status: Optional[pd.DataFrame] = None
        self.data: Optional[pd.DataFrame] = None
        self.variables: List[str] = []

    def __repr__(self):
        data_shape = self.data.shape if self.data is not None else "No data"
        return f"<PlotData(name='{self.name}', shape={data_shape})>"


class WorldParser(NetLogoParser):
    """
    Parses NetLogo world/plot output files.

    These files typically contain a snapshot of the simulation world, including
    global variables, turtle/agent data, and data from all interface plots.

    Attributes:
        plots (Dict[str, PlotData]): A dictionary mapping plot names to PlotData objects.
        globals (Optional[pd.DataFrame]): DataFrame of global variables over time.
        turtles (Optional[pd.DataFrame]): DataFrame of turtle variables over time.
    """
    def __init__(self, file_path: str, rounding_factor: int = DEFAULT_ROUNDING_FACTOR):
        super().__init__(file_path, rounding_factor)
        self.plots: Dict[str, PlotData] = {}
        self.globals: Optional[pd.DataFrame] = None
        self.turtles: Optional[pd.DataFrame] = None

    def parse(self):
        """
        Parses the world data file, populating plot, global, and turtle data.
        """
        with self.file_path.open('r') as f:
            self._parse_header(f)
            
            # Read through file until entities or plots start
            for line in f:
                if line.strip().startswith('"GLOBALS"'):
                    self._parse_entity(f, "GLOBALS", line)
                elif line.strip().startswith('"TURTLES"'):
                    self._parse_entity(f, "TURTLES", line)
                elif line.strip().startswith('"""'): # Start of a plot
                    self._parse_plot(f, line)
        
        # Assign convenience attributes
        self.globals = self.plots.get("GLOBALS", PlotData("")).data
        self.turtles = self.plots.get("TURTLES", PlotData("")).data
        
    def _parse_entity(self, f: TextIO, name: str, first_line: str):
        """Parses a globals or turtles data block."""
        plot = PlotData(name)
        header_line = f.readline()
        plot.variables = self._get_world_vars(header_line)
        
        data_block = self._read_block(f)
        csv_data = header_line + data_block
        
        if csv_data.strip():
            df = pd.read_csv(StringIO(csv_data))
            df['run'] = self.run_number
            plot.data = df.round(self.rounding_factor)

        self.plots[name] = plot

    def _parse_plot(self, f: TextIO, first_line: str):
        """Parses a standard plot data block."""
        name = first_line.replace('"', '').strip()
        plot = PlotData(name)
        
        # Read state and status blocks
        plot.state = pd.read_csv(StringIO(self._read_block(f)))
        plot.status = pd.read_csv(StringIO(self._read_block(f)))

        # Read plot data
        header_line, plot.variables = self._get_plot_vars(f)
        data_block = self._read_block(f)
        
        if len(data_block) > 10: # Ensure there's meaningful data
            csv_data = header_line + data_block
            df = pd.read_csv(StringIO(csv_data))

            # Clean up DataFrame
            df = df.loc[:, ~df.columns.str.replace(r'(\.\d+)$', '', regex=True).duplicated()]
            if 'pen down?' in df.columns:
                 df = df.drop(columns=['pen down?'])
            df['run'] = self.run_number
            
            plot.data = df.round(self.rounding_factor)
            self.plots[name] = plot

    def _get_world_vars(self, header_line: str) -> List[str]:
        """Extracts variable names from a simple comma-quoted header."""
        return [var for var in re.sub(r'[",\n]+', ';', header_line).strip(';').split(';') if var]

    def _get_plot_vars(self, f: TextIO) -> Tuple[str, List[str]]:
        """
        Parses the two-line header for plot data to get variable names.
        
        Returns:
            A tuple containing the reconstructed CSV header line and a list of variable names.
        """
        var_names_line = f.readline()
        header_template_line = f.readline()

        names = [var for var in re.sub(r'[",\n]+', ';', var_names_line).strip(';').split(';') if var]

        # Substitute placeholder 'y' with actual variable names
        for name in names:
            header_template_line = header_template_line.replace('"y"', f'"{name}"', 1)
        
        return header_template_line, names

    def timeadj(self):
        for plot in self.plots.keys():
            if plot == 'GLOBALS' or plot == 'TURTLES':
                continue
            df = self.plots[plot].data
            df['time'] = self.plots['DeltaT'].data['dt']
            for i in range(1,len(df)):
                df.loc[i, 'time'] = round(df.loc[i, 'time'] + df.loc[i-1, 'time'], 0)

    @staticmethod
    def netlogo_color_to_name(netlogo_color: float) -> str:
        """
        Converts a NetLogo color number to an approximate color name.
        """
        # A mapping from NetLogo color numbers to CSS-compatible color names.
        color_map = {
            5: "grey", 9.9: "black", 15: "red", 25: "orange", 35: "brown",
            45: "yellow", 55: "green", 65: "lime", 75: "turquoise", 85: "cyan",
            95: "blue", 105: "violet", 115: "magenta", 125: "pink",
        }
        # Find the closest key in the map
        closest_color = min(color_map.keys(), key=lambda k: abs(k - netlogo_color))
        return color_map.get(closest_color, "black")


class ExperimentParser(NetLogoParser):
    """
    Parses NetLogo BehaviorSpace experiment output files.

    These files are typically in a tabular CSV format and summarize the results
    of running the model with varying input parameters.

    Attributes:
        data (pd.DataFrame): The core DataFrame containing all variable run data.
        fixed_parameters (pd.Series): A Series of parameters that were constant across all runs.
        variable_parameters (pd.DataFrame): A DataFrame summarizing the ranges and statistics
                                            of parameters that varied across runs.
    """
    def __init__(self, file_path: str, rounding_factor: int = DEFAULT_ROUNDING_FACTOR):
        super().__init__(file_path, rounding_factor)
        self.data: Optional[pd.DataFrame] = None
        self.fixed_parameters: Optional[pd.Series] = None
        self.variable_parameters: Optional[pd.DataFrame] = None

    def parse(self, header_row: int = 6):
        """
        Parses the BehaviorSpace CSV file.

        Args:
            header_row (int): The row number (0-indexed) where the data headers are located.
                              NetLogo typically places this on line 7 (index 6).
        """
        full_df = pd.read_csv(self.file_path, header=header_row)

        fixed_params = {}
        var_summary = {}
        
        # Identify fixed vs. variable columns
        for col in full_df.columns:
            # Check if all values in the column are the same
            if full_df[col].nunique() == 1:
                fixed_params[col] = full_df[col].iloc[0]
            else:
                series = full_df[col]
                if pd.api.types.is_numeric_dtype(series.dtype):
                    var_summary[col] = {
                        'min': series.min(),
                        'max': series.max(),
                        'mean': series.mean(),
                        'median': series.median(),
                        'std': series.std()
                    }
                else: # For non-numeric (e.g., string) columns
                     var_summary[col] = {
                        'min': series.min(),
                        'max': series.max(),
                        'mean': 'N/A', 'median': 'N/A', 'std': 'N/A'
                    }

        self.fixed_parameters = pd.Series(fixed_params, name="Fixed Parameters")
        self.variable_parameters = pd.DataFrame(var_summary)

        # The main data should only contain columns that actually varied
        self.data = full_df.drop(columns=self.fixed_parameters.index).round(self.rounding_factor)

    @property
    def df(self):
        return self.data

    @property
    def numruns(self):
        return len(self.data.index)

    def addruntime(self):
        self.df['runtime'] = self.df['timeend'] - self.df['timestart']
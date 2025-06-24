#!/usr/bin/env python3
"""
Generates a comprehensive HTML report for a NetLogo experiment.

This script loads the pre-processed data for an experiment and uses a Jinja2
template to create a detailed HTML summary page. The report includes:
- The experiment's description.
- A table of fixed parameters.
- A table summarizing the variable parameters.
- A full, sortable table of the data from every run.

Usage:
    python report_generator.py /path/to/experiment_directory
"""
import argparse
from pathlib import Path

from jinja2 import Environment, FileSystemLoader

# Assuming the refactored ExperimentManager class is available.
# Make sure it's in a file named 'experiment_manager.py' or similar.
from aiexpmanager import ExperimentManager


def main():
    """Main function to parse arguments and generate the HTML report."""
    parser = argparse.ArgumentParser(
        description="Generate an HTML summary report for a NetLogo experiment."
    )
    parser.add_argument(
        "exp_dir",
        type=str,
        help="The path to the experiment's base directory.",
    )
    args = parser.parse_args()
    experiment_dir = Path(args.exp_dir)

    # --- 1. Load the pre-processed experiment data ---
    try:
        # We assume the ExperimentManager and its dependencies are in place
        manager = ExperimentManager(str(experiment_dir))
    except FileNotFoundError as e:
        print(f"Error: Could not initialize ExperimentManager. {e}")
        print("Please ensure you have run the parallel_processor.py script first.")
        return

    # --- 2. Prepare data for the template ---
    description = manager.description

    # Convert DataFrames to HTML tables
    # Transpose the fixed parameters Series to make it a single row.
    fixed_params_html = manager.fixed_parameters.to_frame().T.to_html(
        classes="dataframe table table-sm table-striped", border=1, index=False
    )

    # The variable parameters are displayed with stats as rows and params as columns.
    variable_params_df = manager.variable_parameters
    variable_params_df.index.name = "Statistic"
    variable_params_html = variable_params_df.to_html(
        classes="dataframe table table-sm table-striped", border=1
    )

    # Add the 'sortable' class to enable the JavaScript sorting library.
    run_data_html = manager.experiment_data.to_html(
        classes="dataframe table table-striped sortable",
        border=1,
        index=False,  # Don't include the DataFrame index in the table
    )

    # --- 3. Set up Jinja2 environment and render the template ---
    # This tells Jinja2 to look for templates in the same directory as the script.
    env = Environment(loader=FileSystemLoader('.'))
    template = env.get_template("report_template.html")

    html_content = template.render(
        experiment_name=manager.name,
        description=description,
        fixed_params_table=fixed_params_html,
        variable_params_table=variable_params_html,
        run_data_table=run_data_html,
    )

    # --- 4. Write the final HTML file ---
    output_path = experiment_dir / "data.html"
    with open(output_path, "w") as f:
        f.write(html_content)

    print(f"Successfully generated report at: {output_path}")


if __name__ == "__main__":
    main()

#!/usr/bin/env python3
"""
An interactive Plotly Dash dashboard for exploring NetLogo experiment data.

This application provides a user-friendly web interface to filter and visualize
the results of complex simulation experiments.

Usage:
    python dashboard.py /path/to/your/experiment_directory

The dashboard will then be available in your web browser, typically at:
    http://127.0.0.1:8050/
"""

import argparse
from pathlib import Path

import dash
import pandas as pd
import plotly.express as px
import plotly.graph_objects as go
from dash import dcc, html
from dash.dependencies import Input, Output

import dash_bootstrap_components as dbc

# We assume the refactored ExperimentManager class from our previous work is
# available in a file named `experiment_manager.py` in the same directory.
from aiexpmanager import ExperimentManager


# --- Data Loading ---
# We load the data once when the app starts.
parser = argparse.ArgumentParser(
    description="Launch an interactive dashboard for a NetLogo experiment."
)
parser.add_argument(
    "exp_dir", type=str, help="The path to the experiment's base directory."
)
args = parser.parse_args()
exp_dir = Path(args.exp_dir)

try:
    manager = ExperimentManager(str(exp_dir))
    DATA = manager.experiment_data
    VARIABLE_PARAMS = manager.variable_parameters.T  # Transposed for easier access
except (FileNotFoundError, ValueError) as e:
    print(f"Error loading experiment data: {e}")
    print("Please ensure you have run the data processing scripts first.")
    exit()

# --- App Initialization ---
app = dash.Dash(__name__, external_stylesheets=[dbc.themes.BOOTSTRAP])

# --- Reusable Components ---
def create_filter_control(param_name, param_stats):
    """Dynamically creates a filter control based on parameter type."""
    if pd.api.types.is_numeric_dtype(param_stats) and param_stats['std'] > 0:
        # Use a RangeSlider for numeric types
        min_val, max_val = param_stats['min'], param_stats['max']
        return dcc.RangeSlider(
            id={'type': 'filter-slider', 'index': param_name},
            min=min_val,
            max=max_val,
            value=[min_val, max_val],
            marks={min_val: str(round(min_val, 2)), max_val: str(round(max_val, 2))},
            tooltip={"placement": "bottom", "always_visible": True}
        )
    else:
        # Use a multi-select Dropdown for categorical/string types or fixed numbers
        options = DATA[param_name].unique()
        return dcc.Dropdown(
            id={'type': 'filter-dropdown', 'index': param_name},
            options=[{'label': i, 'value': i} for i in options],
            multi=True,
            value=options
        )

# --- App Layout ---
controls = [
    dbc.Card(
        [
            html.H4("Plot Controls", className="card-title"),
            html.Label("Plot Type"),
            dcc.Dropdown(
                id='plot-type-dropdown',
                options=[
                    {'label': 'Line Plot with Std Dev', 'value': 'line_std'},
                    {'label': 'Scatter Plot', 'value': 'scatter'},
                    {'label': 'Histogram', 'value': 'histogram'},
                ],
                value='line_std'
            ),
            html.Hr(),
            html.Label("X-Axis"),
            dcc.Dropdown(id='xaxis-dropdown', options=[{'label': i, 'value': i} for i in DATA.columns]),
            html.Label("Y-Axis"),
            dcc.Dropdown(id='yaxis-dropdown', options=[{'label': i, 'value': i} for i in DATA.columns]),
             html.Label("Color By (Scatter Plot)"),
            dcc.Dropdown(id='color-dropdown', options=[{'label': i, 'value': i} for i in DATA.columns]),
        ],
        body=True,
        className="mb-3"
    ),
    dbc.Card(
        [
            html.H4("Parameter Filters", className="card-title"),
            *[
                html.Div([
                    html.Label(param),
                    create_filter_control(param, VARIABLE_PARAMS.loc[param])
                ], className="mb-3")
                for param in VARIABLE_PARAMS.index
            ]
        ],
        body=True
    )
]

app.layout = dbc.Container(
    [
        html.H1(f"Experiment Dashboard: {manager.name}", className="my-4"),
        dbc.Row(
            [
                dbc.Col(controls, md=4),
                dbc.Col(
                    dcc.Loading(
                        id="loading-icon",
                        children=[dcc.Graph(id='main-graph')],
                        type="circle",
                    ),
                    md=8
                ),
            ]
        ),
    ],
    fluid=True,
)


# --- Callbacks ---
@app.callback(
    Output('main-graph', 'figure'),
    [
        Input('plot-type-dropdown', 'value'),
        Input('xaxis-dropdown', 'value'),
        Input('yaxis-dropdown', 'value'),
        Input('color-dropdown', 'value'),
        # Use pattern matching for dynamic inputs
        Input({'type': 'filter-slider', 'index': dash.ALL}, 'value'),
        Input({'type': 'filter-dropdown', 'index': dash.ALL}, 'value'),
        # Also get the IDs to map values to parameters
        dash.dependencies.State({'type': 'filter-slider', 'index': dash.ALL}, 'id'),
        dash.dependencies.State({'type': 'filter-dropdown', 'index': dash.ALL}, 'id'),
    ]
)
def update_graph(
    plot_type, x_axis, y_axis, color_by,
    slider_values, dropdown_values,
    slider_ids, dropdown_ids
):
    if not x_axis or (plot_type != 'histogram' and not y_axis):
        return go.Figure().update_layout(title_text="Please select axes to plot.")

    # --- 1. Filter the DataFrame ---
    filtered_df = DATA.copy()
    
    # Handle sliders
    for slider_id, value_range in zip(slider_ids, slider_values):
        param = slider_id['index']
        filtered_df = filtered_df[
            (filtered_df[param] >= value_range[0]) & (filtered_df[param] <= value_range[1])
        ]

    # Handle dropdowns
    for dropdown_id, selected_options in zip(dropdown_ids, dropdown_values):
        param = dropdown_id['index']
        filtered_df = filtered_df[filtered_df[param].isin(selected_options)]

    if filtered_df.empty:
        return go.Figure().update_layout(title_text="No data matches the selected filters.")

    # --- 2. Generate the appropriate plot ---
    if plot_type == 'line_std':
        # Group by the x-axis variable and calculate mean and std dev
        grouped = filtered_df.groupby(x_axis)[y_axis].agg(['mean', 'std']).reset_index()
        
        fig = go.Figure()
        # Add the standard deviation as a shaded area
        fig.add_trace(go.Scatter(
            x=grouped[x_axis],
            y=grouped['mean'] + grouped['std'],
            mode='lines',
            line=dict(width=0),
            showlegend=False
        ))
        fig.add_trace(go.Scatter(
            x=grouped[x_axis],
            y=grouped['mean'] - grouped['std'],
            mode='lines',
            line=dict(width=0),
            fill='tonexty',
            fillcolor='rgba(0,100,80,0.2)',
            name='Std Dev'
        ))
        # Add the mean line
        fig.add_trace(go.Scatter(
            x=grouped[x_axis],
            y=grouped['mean'],
            mode='lines',
            line=dict(color='rgb(0,100,80)'),
            name='Mean'
        ))
        fig.update_layout(
            title=f"Mean of {y_axis} vs {x_axis} with Std Dev",
            xaxis_title=x_axis, yaxis_title=y_axis
        )
        return fig

    elif plot_type == 'scatter':
        fig = px.scatter(
            filtered_df, x=x_axis, y=y_axis, color=color_by,
            title=f"Scatter plot of {y_axis} vs {x_axis}"
        )
        return fig

    elif plot_type == 'histogram':
        fig = px.histogram(
            filtered_df, x=x_axis,
            title=f"Distribution of {x_axis}"
        )
        return fig

    return go.Figure()


if __name__ == '__main__':
    app.run(debug=True, port=8050)


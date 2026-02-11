# load_project

Loads a specified GCAM project into the global environment.

## Usage

``` r
load_project(
  project_path,
  desired_regions = "All",
  scenarios = NULL,
  GCAM_version = "v7.1"
)
```

## Arguments

- project_path:

  Full path to the GCAM project file, including the project name and
  extension (e.g., .dat, .proj). This file is loaded into the global
  environment.

- desired_regions:

  Regions to include in the project data. Defaults to 'All'. Specify a
  vector to include specific regions. To view available regions, run
  \`available_regions()\`. Note: Only the specified regions will be
  included in the dataset, forming the "World" for the project.

- scenarios:

  Names of the scenarios to consider from the project. Defaults to all
  scenarios available within the project. If specific scenarios are
  needed, provide a vector of scenario names.

- GCAM_version:

  Main GCAM compatible version: 'v7.1' (default), 'v7.2', 'v7.0'.

## Value

The function loads the specified GCAM project into the global
environment. It does not return a value, but the project data becomes
available for use in further analysis or reporting.

## Details

This function imports a GCAM project from the specified path and makes
it available in the global environment.

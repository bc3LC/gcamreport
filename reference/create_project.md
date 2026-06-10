# create_project

Creates and loads a specified GCAM project into the global environment.

## Usage

``` r
create_project(
  db_path,
  db_name,
  prj_name,
  scenarios = NULL,
  desired_regions = "All",
  desired_variables = "All",
  GCAM_version = "v8.2",
  queries_general_file = NULL,
  queries_nonCO2_file = NULL
)
```

## Arguments

- db_path:

  Optional. Full path to the GCAM database. Required if creating a new
  project.

- db_name:

  Optional. Name of the GCAM database. Required if creating a new
  project.

- prj_name:

  Name of the GCAM project. Can be an existing project name (loads the
  project) or a new project name (creates a new project). Accepts
  extensions: .dat and .proj.

- scenarios:

  Names of the scenarios to include. Defaults to all scenarios available
  in the project or database.

- desired_regions:

  Regions to include in the report. Defaults to 'All'. Specify a vector
  for specific regions. To view available options, run
  \`available_regions()\`. Note: The dataset will include only the
  specified regions, forming the "World" for the project.

- desired_variables:

  Variables to include in the report. Defaults to 'All'. Specify a
  vector for specific variables. To view available options, run
  \`available_variables()\`. Note: Global variables like "Emissions"
  will only account for selected variables. For example, selecting
  "Emissions" and "Emissions\|CO2" will make "Emissions" account only
  for "Emissions\|CO2", excluding other variables such as
  "Emissions\|CH4" or "Emissions\|NH3".

- GCAM_version:

  Name of the GCAM compatible version. Run \`available_GCAM_versions()\`
  to see the list of supported options.

- queries_general_file:

  Optional. Full path to a general XML query file (including file name
  and extension). Defaults to a general query file compatible with the
  specified \`GCAM_version\` that reports all standardized variables.

- queries_nonCO2_file:

  Optional. Full path to an XML query file (including file name and
  extension) for non-CO2 queries, such as "nonCO2 emissions by subsector
  (excluding resource production)" and "nonCO2 emissions by region".
  Defaults to a non-CO2 query file compatible with the specified
  \`GCAM_version\`.

## Value

Loads the specified project into the global environment and saves the
project locally if it is created. The function sets up the project with
the given parameters and makes it available for further analysis and
reporting.

## Details

This function allows for the creation of a new GCAM project or loading
an existing one. It sets up the project with specified scenarios,
regions, and variables, and saves it locally if needed. The project is
then made available in the global environment for further use.

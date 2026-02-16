# generate_report

Main function for generating a GCAM project report. This function
handles: - Creating or loading a GCAM project. - Standardizing the data
and saving it in RData, CSV, and XLSX formats. - Running vetting
verifications. - Launching the User Interface (UI).

## Usage

``` r
generate_report(
  db_path = NULL,
  db_name = NULL,
  prj_name,
  scenarios = NULL,
  final_year = 2100,
  desired_variables = "All",
  ignore = NULL,
  desired_regions = "All",
  desired_continents = "All",
  save_output = TRUE,
  output_file = NULL,
  launch_ui = TRUE,
  interactive = F,
  GCAM_version = "v7.1",
  GWP_version = "AR5",
  ref_scen_name = NULL,
  queries_general_file = NULL,
  queries_nonCO2_file = NULL,
  all_tier1 = F
)
```

## Arguments

- db_path:

  Full path to the GCAM database. Required if creating a new project.

- db_name:

  Name of the GCAM database. Required if creating a new project.

- prj_name:

  Name of the GCAM project. Can be an existing project name (loads the
  project) or a new project name (creates a new project). Accepts
  extensions: .dat and .proj.

- scenarios:

  Names of the scenarios to consider. Defaults to all scenarios in the
  project or database.

- final_year:

  Final year of the data. Defaults to 2100. Note: \`final_year\` must be
  at least 2025 and must align with available 5-year intervals, such as
  2025, 2030, 2035, 2040, etc.

- desired_variables:

  Variables to include in the report. Defaults to 'All'. Specify a
  vector for specific variables. To view available options, run
  \`available_variables()\`. Note: Global variables like "Emissions"
  will only account for selected variables. E.g., if you select
  "Emissions" and "Emissions\|CO2", "Emissions" will only account for
  "Emissions\|CO2", and will not account for other variables such as
  "Emissions\|CH4" or "Emissions\|NH3".

- ignore:

  Policy names introduced by the user to ignore during gcamreport
  processing of physical quantities, since otherwise they will be
  flagged as names missing from mapping files and cause an error. Note:
  Currently having one of the specified name patterns in any column of
  the query results, such as sector, subsector, input, etc. will cause
  the error to be disregarded. Same behavior than adding the policy
  names to the corresponding mappings indicating \`NoReported\`.

- desired_regions:

  Regions to include in the report. Defaults to 'All'. Specify a vector
  for specific regions. To view available options, run
  \`available_regions()\`. Note: The dataset will include only the
  specified regions, which will make up "World".

- desired_continents:

  Continent/region groups to include in the report. Defaults to 'All'.
  Specify a vector for specific groups. To view available options, run
  \`available_continents()\`. Note: The dataset will include only the
  specified groups, which will make up "World".

- save_output:

  If \`TRUE\` (default), saves reporting data in CSV and XLSX formats.
  If \`FALSE\`, data is not saved. If 'CSV' or 'XLSX', data will be
  saved only in the specified format.

- output_file:

  File path and name for saving the data. If not specified, defaults to
  the directory of the database or project file with a default name
  containing 'standardized'. Provide a full path without an extension,
  which will be automatically added.

- launch_ui:

  If \`TRUE\` (default), launches the User Interface. If \`FALSE\`, does
  not launch the UI.

- interactive:

  If \`TRUE\` (not default), asks the user what to do when Warnings
  appear. Otherwise, the Warnings are displayed at the end of the
  script.

- GCAM_version:

  Main GCAM compatible version: 'v7.1' (default), 'v7.2', 'v7.0'.

- GWP_version:

  Global Warming Potential (GWP) version: 'AR5' (default), 'AR6', or
  'AR4'.

- ref_scen_name:

  Name of the Reference scenario. Necessary to compute Expenditure
  variables. If left empty, \`Reference\` and \`Baseline\` tags will be
  looked for. If not encountered, the first scenario of your project
  will be considered as the Reference.

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

- all_tier1:

  If \`TRUE\` (not default), introduce all Tier 1 Variables (as 0) in
  the standardized report.

## Value

Saves RData, CSV, and XLSX files with standardized variables, launches
the user interface, and saves the GCAM project file if created.

## Details

You can specify the regions and/or variables to be reported and
provide: - Either \`db_path\`, \`db_name\`, \`prj_name\`, and
\`scenarios\` to create a new project and generate the report. - Or
\`prj_name\` and \`scenarios\` to produce a report from an existing
project.

The resulting RData output can be used to manually call
\`launch_gcamreport_ui\`.

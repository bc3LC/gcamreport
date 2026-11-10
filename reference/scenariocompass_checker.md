# scenariocompass_checker

Assess scenario run against Scenario Compass Criteria. Evaluates a
standardized reporting dataset to determine feasibility and
sustainability outcomes based on the Scenario Compass frameworks (more
info at <https://scenario-evaluation-criteria.iamconsortium.org/>.)

## Usage

``` r
scenariocompass_checker(
  report,
  desired_scenarios = NULL,
  desired_regions = NULL,
  GCAM_version = "v8.2",
  save_output = TRUE,
  output_file = NULL
)
```

## Arguments

- report:

  A data frame containing the standardized report (e.g., output from
  \`generate_report()\`), or a character string representing the file
  path to a saved report. Supported file extensions: \`.csv\`,
  \`.xlsx\`, and \`.RData\`.

- desired_scenarios:

  A character vector of specific scenarios to assess. If \`NULL\` (the
  default), all scenarios present in \`report\` are evaluated.

- desired_regions:

  A character vector of specific regions to assess. If \`NULL\` (the
  default), all regions present in the report are evaluated. You can
  provide \`'World'\` for a global assessment, \`'Regional'\` to
  evaluate all sub-regions, or explicitly list specific GCAM region
  names. Run \`available_regions()\` to see the complete list of
  supported options.

- GCAM_version:

  A character string specifying the GCAM version used for regional
  mappings and reference datasets (e.g., \`'v8.2'\`). Run
  \`available_GCAM_versions()\` to see the complete list of supported
  options.

- save_output:

  Logical (\`TRUE\`/\`FALSE\`). Determines whether the resulting
  assessment should be saved to disk. Defaults to \`TRUE\`.

- output_file:

  A character string specifying the absolute or relative file path for
  the saved output. If \`NULL\` and \`save_output\` is enabled, it
  defaults to the \`report\` file path (if present) or is set to the
  current working directory.

## Value

A data frame containing the evaluation outcomes (\`ok\`, \`medium\`,
\`high\`, \`failed\`, or \`not_assigned\`) for the assessed variables
and regions. If \`save_output\` is enabled, the dataset is also saved to
disk.

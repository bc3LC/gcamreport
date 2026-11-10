# Scenario Compass Evaluation

The [Scenario Compass
Initiative](https://scenario-evaluation-criteria.iamconsortium.org/stable/)
is a community-driven framework designed to continuously collect,
curate, and assess global emissions scenarios. It provides a robust
infrastructure for transparently vetting modeled pathways against
real-world historical data and updated feasibility benchmarks.

In `gcamreport`, we have streamlined the process of evaluating your
model runs. By bridging our standardization workflow directly with the
Scenario Compass criteria, you can instantly assess how your scenarios
measure up!

This functionality is powered by the
[`scenariocompass_checker()`](https://bc3lc.github.io/gcamreport/reference/scenariocompass_checker.html)
function. You can run it as a standalone tool, or trigger it
automatically while standardizing your data via the
[`generate_report()`](https://bc3lc.github.io/gcamreport/reference/generate_report.md)
function.

newline

**Before we begin:** For the examples presented here, we assume you
already have `gcamreport` installed and know the basics. If you are new
here, check out our [Step-by-Step
Tutorial](https://bc3lc.github.io/gcamreport/articles/Step_By_Step_Full_Example.html)
first.

**Note**: This process saves a `.csv` evaluation file in the same folder
as the standardized data. You can also view the results directly in the
console

newline

## Example 1: standardize and evaluate an entire dataset

Suppose you have a project file (or analogously, a database) that you
wish to standardize and evaluate. To evaluate all the regions and
scenarios contained within, you can indicate it as follows:

``` r

generate_report(
  prj_name = 'path/to/project/name', 
  GCAM_version = 'your_GCAM_version',
  scenariocompass_evaluation = TRUE
)
```

newline

## Example 2: evaluate a GCAM run agains SCI criteria

Suppose your primary objective is to assess whether a GCAM run complies
with the Scenario Compass criteria. You can optimize the process by
restricting standardization exclusively to the variables required for
the assessment.

``` r

generate_report(
  prj_name = 'path/to/project/name', 
  GCAM_version = 'your_GCAM_version',
  scenariocompass_evaluation = TRUE,
  desired_variables = 'SCI'
)
```

newline

## Example 3: evaluate only the global or the regional criteria

Suppose you want to evaluate a GCAM run against specific Scenario
Compass thresholds; whether (a) global criteria, (b) broad regional
criteria, or (c) criteria for a specific country or region present in
your GCAM version. You can easily tailor the assessment by defining the
regional scope. If no scope is specified (the default setting), the tool
will automatically perform both the global and the standard regional
evaluations.

``` r

# To evaluate against global criteria only:
generate_report(
  prj_name = 'path/to/project/name', 
  GCAM_version = 'your_GCAM_version',
  scenariocompass_evaluation = TRUE,
  scenariocompass_regions = 'World'
)
                
# To evaluate against regional criteria only:
generate_report(
  prj_name = 'path/to/project/name', 
  GCAM_version = 'your_GCAM_version',
  scenariocompass_evaluation = TRUE,
  scenariocompass_regions = 'Regional'
)

# To evaluate against a certain region/country (e.g., USA and China) criteria only:
generate_report(
  prj_name = 'path/to/project/name', 
  GCAM_version = 'your_GCAM_version',
  scenariocompass_evaluation = TRUE,
  scenariocompass_regions = c('USA','China')
)
```

newline

## Example 4: evaluate targeted scenarios

Suppose you have multiple GCAM runs and, although you wish to
standardize them all, you only want a subset to be assessed. You can
specify the target scenarios for evaluation. If nothing is indicated
(default), all scenarios will be evaluated

``` r

generate_report(prj_name = 'path/to/project/name', 
                GCAM_version = 'your_GCAM_version',
                scenariocompass_evaluation = TRUE,
                scenariocompass_scenarios = c('scen1','scen2'))
              
```

newline

## Example 5: evaluate an already standardized dataset

Suppose you have previous GCAM runs that are already standardized. You
can assess them against the Scenario Compass criteria directly by
loading the standardized `RData`, `.csv` or `.xlsx` file.

``` r

scenariocompass_checker(
  report = 'path/to/standardized/dataset.RData', # also accepts .csv or .xlsx
  GCAM_version = 'your_GCAM_version'
)
```

# GCAM-Europe Particularities

[GCAM-Europe](https://github.com/bc3LC-GCAMEurope/gcam-core) is an
advanced regional expansion of the Global Change Analysis Model (GCAM).
While the core version of GCAM aggregates the world into 32 regions,
with the European continent divided into just five (EU-12, EU-15,
Eastern Europe, Europe-non-EU, and EFTA), GCAM-Europe disaggregates 39
European countries into individual model regions. This high level of
geographical resolution makes it possible to explore the country-level
impacts of European policy packages and transformational strategies,
alongside potential international spillover effects (e.g., carbon
leakage) across 27 distinct non-European global regions. More details
can be found in the upcoming publication!

------------------------------------------------------------------------

## Output Standardization Workaround

Due to an open `rgcam` issue
([\#93](https://github.com/JGCRI/rgcam/issues/93)), output
standardization requires manually handling four specific queries. You
must extract and rename them as follows:

- `CO2 emissions by tech (nested subsector) (excluding resource production)`
  $`\rightarrow`$ Rename to:
  `CO2 emissions by tech (excluding resource production)`
- `CO2 sequestration by tech (nested subsector)` $`\rightarrow`$ Rename
  to: `CO2 sequestration by tech`
- `water withdrawals by subsector` $`\rightarrow`$ Keep original name
- `water consumption by subsector` $`\rightarrow`$ Keep original name

Follow the steps below to complete this manual integration:

1.  Initialize the project: Execute `gcamreport` in your R console,
    specifying either `GCAM_version = "vEurope7.2"` or
    `GCAM_version = "vEurope8.7"`. This initializes the directory
    structure and begins the usual standardization process.

2.  Extract queries via Model Interface: Open the *GCAM Model Interface*
    and load the four queries listed above for all regions and your
    target scenarios.

3.  Format the CSV file: Copy the extracted datasets into a spreadsheet
    editor (e.g., Excel). Manually update the query names as required,
    and verify that no spurious commas exist within the query names or
    as ghost columns. Export the final sheet as a clean CSV file.

4.  Append data to the project: Once the initial `gcamreport`
    standardization process halts (which is expected to fail initially
    due to the missing queries), run the following line in your R
    console to inject your manually formatted `.csv` file:  
    `R prj <- rgcam::addMIBatchCSV(fn = "path/to/queries.csv", clobber = TRUE, saveProj = TRUE)`

5.  Reset R session: Clear your global environment variables and
    restart/reset your R session to ensure no cached data interferes
    with the next step.

6.  Generate the finalized report: Run the standardization on the
    updated project. When calling the
    [`generate_report()`](https://bc3lc.github.io/gcamreport/reference/generate_report.md)
    function, pass your updated project via the `prj_name` argument. Do
    not specify `db_path` or `db_name`.

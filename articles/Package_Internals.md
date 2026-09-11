# Package_Internals

Ever wondered how `gcamreport` operates under the hood? Whether you’re
looking to contribute, customize, or simply satisfy your curiosity,
let’s dive into the mechanics of this package.

## Key Concepts

- `gcamreport` is an R package designed to standardize outputs from
  GCAM. While its core functionality supports all GCAM versions, the
  mappings must be tailored to each version.
- Package Structure
  - Data Storage: Located in `gcamreport/inst/extdata`.
  - Codebase: Basically located in `gcamreport/R`.
    - Codebase to run the package: Found in `gcamreport/R/main.R`.
    - Codebase to compute each standardized variable: Found in
      `gcamreport/R/functions.R`.
    - Codebase to run the UI: Found in `gcamreport/inst/gcamreport_ui`.

## Workflow

**1. Running the Report**

The user calls the `generate_report` function, specifying at least:

- A project or a database path.
- The variables to standardize (`All` by default).
- The GCAM version (`v7.1` by default).

**2. Identifying Required Internal Variables**

The package determines which internal variables must be computed to
generate the desired standardized output. This is done by examining the
*Variable* and *Internal_Variable* columns in in the
`gcamreport/inst/extdata/template/GCAM_version/common-definitions-template.csv`
file. Notice that it is GCAM version dependent.

**3. Determining Necessary Queries and Required Dependent Variables**

The package automatically identifies all the required variables and
queries (in case it create the project from a database) by:

- [Recursively](https://www.geeksforgeeks.org/introduction-to-recursion-2/)
  searching the *Name* column in
  `gcamreport/inst/extdata/mappings/GCAMversion/variables_functions_mapping.csv`,
  which corresponds to previously identified *Internal_Variable* s.
- Extracting relevant queries from the *Queries* column.
- Resolving dependencies: Some variables depend on others (listed in the
  dependencies column), meaning their queries and dependencies must also
  be considered.

*Example*: To standardize the `Emissions` variable, the package
identifies that it requires both CO2 and non-CO2 emissions. Since these
are computed independently and rely on different queries, the package
automatically detects and loads all necessary components to ensure a
complete calculation.

**4. Loading or Creating the Project**

If the project already exists, it is loaded. Otherwise, a new project is
created, including all required queries.

**5. Computing Standardized Variables**

Once all the required variables (desired and dependent) are identified,
`gcamreport` systematically computes them one-by-one by following a
structured workflow:

- Step 0: Variable dependency - The package recursively determines if
  the variable depends on a previously uncomputed variable, which will
  be tackled first through the same following steps 1-6. Otherwise, the
  standardization of the selected variable starts.
- Step 1: Function Trigger - The package determines which function to
  use based on the *Fun* column in
  `gcamreport/inst/extdata/mappings/GCAMversion/variables_functions_mapping.csv`.
  All functions are defined in `gcamreport/R/functions.R`.
- Step 2: Query Verification – Before proceeding, the function ensures
  that all required queries for computation are present. If any are
  missing, the process halts.
- Step 3: Query Loading and Validation – The function loads the
  necessary queries (if needed) and checks for infinite values. If any
  are detected, a warning message is displayed.
- Step 4: Loading Mapping Files – The package retrieves mapping files
  stored in `gcamreport/inst/extdata/mappings/GCAMversion/`. During this
  step:
  - Items categorized as *NoReported* are removed.
  - Items specified in the `ignore` parameter of `generate_report` are
    also excluded.
- Step 5: Variable Computation – The function executes the required
  transformations to generate the standardized variable.
- Step 6: Storing and Sharing – Finally, the computed standardized
  variable is made available in the global environment for further use.

**6. Computing Global (World) Values**

For variables where the global value is the sum of regional values, this
step is performed after regional calculations. For other cases (e.g.,
weighted means), the global value is computed within the variable’s
calculation process.

**7. Checking NAs or Inf values**

The package automatically reports if there are NAs or infinite values in
the standardized output. It displays an informative message.

**8. Finalizing the Report**

The standardized dataset is stored. If selected by the user, the user
interface is launched for further exploration.

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
  - Codebase: Found in `gcamreport/R`.
    - Codebase to run the package: Found in `gcamreport/R/main.R`.
    - Codebase to compute each standardized variable: Found in
      `gcamreport/R/functions.R`.

## Workflow

**1. Running the Report**

The user calls the `generate_report` function, specifying at least:

- A project or database.
- The variables to standardize.
- The GCAM version.

**2. Identifying Required Internal Variables**

The package determines which internal variables must be computed to
generate the desired output. This is done by examining the *Variable*
and *Internal_Variable* columns in:
`gcamreport/inst/extdata/template/GCAMversion/common-definitions-template.xlsx`
(Variable sheet).

**3. Determining Necessary Queries**

If the specified project needs to be created from the database, the
package identifies the required queries by:

- [Recursively](https://www.geeksforgeeks.org/introduction-to-recursion-2/)
  searching the *Name* column in
  `gcamreport/inst/extdata/mappings/GCAMversion/variables_functions_mapping.csv`,
  which corresponds to previously identified *Internal_Variable* s.
- Extracting relevant queries from the *Queries* column.
- Resolving dependencies: Some variables depend on others (listed in the
  dependencies column), meaning their queries and dependencies must also
  be considered.

**4. Loading or Creating the Project**

If the project already exists, it is loaded. Otherwise, a new project is
created, including all required queries.

**5. Selecting Standardized Variables**

The package
[recursively](https://www.geeksforgeeks.org/introduction-to-recursion-2/)
identifies the required queries:

- By searching the *Fun* column in
  `gcamreport/inst/extdata/mappings/GCAMversion/variables_functions_mapping.csv`,
  which corresponds to the function to compute the previously identified
  *Internal_Variable* s.
- It attempts to compute each variable one by one.
- If a variable depends on a previously uncomputed variable, that
  variable is calculated first. This process may require additional
  variables to be loaded recursively.

**6. Computing Standardized Variables**

Once a variable is identified as required, `gcamreport` systematically
computes it by following a structured workflow:

- Function Trigger - The package determines which function to use based
  on the previous point. All functions are defined in
  `gcamreport/R/functions.R`.
- Query Verification – Before proceeding, the function ensures that all
  required queries for computation are present. If any are missing, the
  process halts.
- Query Loading and Validation – The function loads the necessary
  queries (if needed) and checks for infinite values. If any are
  detected, a warning message is displayed.
- Loading Mapping Files – The package retrieves mapping files stored in
  `gcamreport/inst/extdata/mappings/GCAMversion/`. During this step:
  - Items categorized as *NoReported* are removed.
  - Items specified in the `ignore` parameter of `generate_report` are
    also excluded.
- Variable Computation – The function executes the required
  transformations to generate the standardized variable.
- Storing and Sharing – Finally, the computed standardized variable is
  made available in the global environment for further use.

**7. Computing Global (World) Values**

For variables where the global value is the sum of regional values, this
step is performed after regional calculations. For other cases (e.g.,
weighted means), the global value is computed within the variable’s
calculation process.

**8. Checking NAs or Inf values**

The package automatically reports if there are NAs or infinite values in
the standardized output. It displays an informative message.

**9. Finalizing the Report**

The standardized dataset is stored. If selected by the user, the user
interface is launched for further exploration.

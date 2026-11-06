## Functions to assess the GCAM runs against the Scenario Compass Criteria.

#' scenariocompass_checker
#'
#' Assess scenario run against Scenario Compass Criteria.
#' Evaluates a standardized reporting dataset to determine feasibility and
#' sustainability outcomes based on the Scenario Compass frameworks (more info
#' at \url{https://scenario-evaluation-criteria.iamconsortium.org/}.)
#'
#' @param report A data frame containing the standardized report (e.g., output from
#'   `generate_report()`), or a character string representing the file path to a saved
#'   report. Supported file extensions: `.csv`, `.xlsx`, and `.RData`.
#' @param desired_scenarios A character vector of specific scenarios to assess.
#'   If `NULL` (the default), all scenarios present in `report` are evaluated.
#' @param GCAM_version A character string specifying the GCAM version used for
#'   regional mappings and reference datasets (e.g., `'v8.2'`). Run
#'   `available_GCAM_versions()` to see the complete list of supported options.
#' @param save_output Logical (`TRUE`/`FALSE`). Determines whether the
#'   resulting assessment should be saved to disk. Defaults to `TRUE`.
#' @param output_file A character string specifying the absolute or relative file path
#'   for the saved output. If `NULL` and `save_output` is enabled, it defaults to the
#'   `report` file path (if present) or is set to the current working directory.
#'
#' @return A data frame containing the evaluation outcomes (`ok`, `medium`, `high`,
#'   `failed`, or `not_assigned`) for the assessed variables and regions. If `save_output`
#'   is enabled, the dataset is also saved to disk.
#' @export
scenariocompass_checker <- function(report,
                                    desired_scenarios = NULL,
                                    GCAM_version = 'v8.2',
                                    save_output = TRUE,
                                    output_file = NULL) {

  #### `report` loading, format checker and dplyr::filtering ==========================
  # load `report` if a file path is provided
  report_path <- if (is.character(report) && length(report) == 1) report else NULL
  if (is.character(report) && length(report) == 1) {
    if (!file.exists(report)) {
      stop(sprintf("The file '%s' does not exist.", report))
    }

    ext <- tolower(tools::file_ext(report))

    report <- switch(ext,
                     "csv"   = read.csv(report),
                     "xlsx"  = readxl::read_excel(report),
                     "rdata" = get(load(report)),
                     stop("Unsupported file format. Please provide a CSV, XLSX, or RData file.", call. = FALSE)
    )
  }

  # assess that the `report` has the correct data format
  req_cols <- c("Model", "Scenario", "Region", "Variable", "Unit")
  if (!all(req_cols %in% colnames(report))) {
    stop("The 'report' object is not in the required format. Please provide the direct output from 'generate_report()'.", call. = FALSE)
  }

  # dplyr::filter the desired scenarios (if provided)
  if (!is.null(desired_scenarios)) {
    report <- report %>%
      dplyr::filter(Scenario %in% desired_scenarios)
  }

  # store the reported years
  all_report_years <- as.numeric(grep("^[0-9]+$", colnames(report), value = TRUE))


  #### saving options checker and refining =====================================
  # check and adapt save_output and output_file variables
  if (any(tolower(save_output) %in% c("csv", "xlsx"))) {
    save_output <- TRUE
  }
  if (save_output && is.null(output_file) &&
      !is.null(report_path)) {
    output_file <- tools::file_path_sans_ext(report_path)
    output_file <- paste0(output_file, "_scenariocompass_assessment")
  } else if (save_output && is.null(output_file)) {
    output_file <- paste0(here::here(), "dataset_scenariocompass_assessment")
  }

  #### required data loading and arragement ====================================
  # load scenariocompass reference data
  reference <- scenarioevaluationcriteria::load_criteria(c('reference-data'))

  # store ISO codes
  all_iso <- unique(reference$region)
  all_iso <- all_iso[nchar(all_iso) == 3]

  # load GCAMregion - iso mapping according to the GCAM version
  if (grep('Europe', GCAM_version)) {
    gcam_regions <- get(paste('iso_to_gcam',GCAM_version,sep='_'),
                        envir = asNamespace("gcamreport")) %>%
      dplyr::select(region = iso, GCAM_region = GCAMEU_region)
  } else {
    gcam_regions <- get(paste('iso_to_gcam',GCAM_version,sep='_'),
                        envir = asNamespace("gcamreport")) %>%
      dplyr::select(region = iso, GCAM_region)
  }


  criteria <- scenarioevaluationcriteria::load_criteria(c('criteria-thresholds')) %>%
    # remove the cummulative variables
    dplyr::filter(!grepl('cumulative',year)) %>%
    # STEP 0: update evaluation_outcome and variables' names =====================
    dplyr::mutate(evaluation_outcome = dplyr::if_else(evaluation_outcome == 'failed', 'medium', evaluation_outcome)) %>%
    dplyr::mutate(variable = dplyr::if_else(variable == 'Carbon Capture|Geological Storage, Carbon Capture',
                              'Carbon Capture|Geological Storage', variable)) %>%


    # STEP 1: expand years =======================================================
    # expand the comma-separated years into multiple rows
    tidyr::separate_rows(year, sep = ",\\s*") %>%
    dplyr::mutate(year = dplyr::na_if(trimws(year), ""),
           year = as.numeric(year)) %>%

    # expand the NA (empty) rows to all report available years
    dplyr::mutate(year = purrr::map(year, ~ if(is.na(.x)) all_report_years else .x)) %>%
    tidyr::unnest(year) %>%

    dplyr::mutate(year = as.integer(year)) %>%

    # STEP 2: expand regions =====================================================
    # expand comma-separated regions into multiple rows
    tidyr::separate_rows(region, sep = ",\\s*") %>%
    dplyr::mutate(region = trimws(region)) %>%

    # replace "All Countries" with the full vector of ISO codes
    dplyr::mutate(region = purrr::map(
      region,
      ~ if(tolower(.x) == "all countries") all_iso else .x
    )) %>%
    tidyr::unnest(region)

  # STEP 3: deal with dimensionless values =====================================
  criteria_dimless <- criteria %>%
    dplyr::filter(unit == 'dimensionless') %>%
    dplyr::select(-unit) %>%

    # extract the contents of range(...) or keep the single source
    dplyr::mutate(
      ref_extracted = dplyr::if_else(
        stringr::str_detect(reference_data, "^range\\("),
        stringr::str_replace_all(reference_data, "^range\\(|\\)$", ""), # Remove "range(" and ")"
        reference_data
      )
    ) %>%
    tidyr::separate_rows(ref_extracted, sep = ",\\s*") %>%

    # join with the reference dataset using the extracted single source names
    dplyr::left_join(
      reference,
      by = c('ref_extracted' = 'reference_data', 'region', 'year', 'variable')
    ) %>%

    # group back to the original reference_dataset criteria structure
    # extract the most permissive values (widest possible range)
    dplyr::summarise(
      value_min = if(all(is.na(value))) NA_real_ else min(value, na.rm = TRUE),
      value_max = if(all(is.na(value))) NA_real_ else max(value, na.rm = TRUE),
      .by = c(criterion, variable, region, year, reference_data, evaluation_outcome, unit, lower, upper)
    ) %>%
    # apply the multipliers to estimate the final thresholds (using the lowest
    # historical base for the lower bound, and highest for the upper)
    dplyr::mutate(
      lower = lower * value_min,
      upper = upper * value_max,
      region = tolower(region)
    ) %>%
    # clean and remove non-informative rows (both lower and upper bound are NA)
    # created spuriously when expanding for all countries
    dplyr::select(-value_min, -value_max) %>%
    dplyr::filter(!(is.na(lower) & is.na(upper))) %>%

    # STEP 4: aggregate GCAM regions =============================================
  dplyr::left_join(gcam_regions, by = 'region') %>%  # only "ant" (small), "rom" (=rou),
    # "chi"(small), "scg" (=srb+mne),
    # "pci" (small), do not have a pair
    dplyr::summarise(lower = sum(lower),
              upper = sum(upper),
              .by = c(criterion, variable, year, reference_data, evaluation_outcome, unit, GCAM_region)) %>%
    dplyr::rename(region = GCAM_region)


  criteria2 <- rbind(
    criteria %>%
      dplyr::filter(unit != 'dimensionless'),
    criteria_dimless
  )

  scencompass_checker <- criteria2 %>%
    tidyr::pivot_wider(
      names_from = evaluation_outcome,
      values_from = c(lower, upper)) %>%
    # dplyr::filter(variable == 'Emissions|CO2|AFOLU', region == 'Austria') %>%
    dplyr::left_join(report %>%
                tidyr::pivot_longer(cols = matches("^\\d{4}$"), names_to = 'year', values_to = 'value') %>%
                dplyr::mutate(year = as.integer(year)),
              by = c('variable' = 'Variable', 'unit' = 'Unit', 'region' = 'Region', 'year'),
              relationship = "many-to-many") %>% # in case there are multiple scenarios
    # evaluate the outcome
    evaluate_outcome(.) %>%
    # clean
    dplyr::rename_with(tolower)

  # save and return dataset
  if (save_output) {
    write.csv(scencompass_checker, file.path(paste0(output_file, "_scenariocompass_criteria.csv")), row.names = FALSE)
    rlang::inform(paste0("Scenariocompass evaluation saved in ",file.path(paste0(output_file, "_scenariocompass_criteria.csv")),
                         "and also available typing `scencompass_checker` in your console."))
  } else {
    rlang::inform(paste0("Scenariocompass evaluation done, available typing `scencompass_checker` in your console."))
  }

  scencompass_checker <<- scencompass_checker
  return(invisible(scencompass_checker))
}










#' evaluate_outcome
#'
#' Auxiliary function to assign an outcome considering the scenario_compass data range
#' @param data dataset to be assessed. Needs to have the refernce scenario_compass data
#' and the report value in different columns.
#' @return dataset with a new column, `outcome`, indicating if the value is in between
#' the `medium`, `high`, `ok` range, or either was `not_assigned` (if NA) or `failed` otherwise
#' @keywords internal, sci
evaluate_outcome <- function(data) {
  data %>%
    dplyr::mutate(
      outcome = dplyr::case_when(
        # 1. if no value exists, it's not assigned
        is.na(value) ~ "not_assigned",

        # 2. check MEDIUM: At least one bound must exist, AND value must be within bounds
        (!is.na(lower_medium) | !is.na(upper_medium)) &
          value >= dplyr::coalesce(lower_medium, -Inf) & value <= dplyr::coalesce(upper_medium, Inf) ~ "medium",

        # 3. check HIGH: At least one bound must exist, AND value must be within bounds
        (!is.na(lower_high) | !is.na(upper_high)) &
          value >= dplyr::coalesce(lower_high, -Inf) & value <= dplyr::coalesce(upper_high, Inf) ~ "high",

        # 4. if it fails all defined brackets, it fails
        TRUE ~ "failed"
      )
    )
}



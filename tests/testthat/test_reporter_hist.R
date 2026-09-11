library(gcamreport)
library(testthat)
library(magrittr)

test_that("Test_historical_values", {

  for (GCAMv in gcamreport::available_GCAM_versions) {
    print(GCAMv)

    prjtest_name <- switch(GCAMv,
                           "v7.0"              = "test7.dat",
                           "v7.1"              = "database_basexdb_hist_7p1.dat",
                           "v7.2"              = "test7p2.dat",
                           "v8.2"              = "gcamreport_onboard8p2_Ctax_260210",
                           "vScenarioMIPCMIP7" = "db_exp_scenarioMIPcmip7",
                           "vEurope7.2"        = "db_test_Europe7.2.dat",
                           "vEurope8.7"        = "db_test_Europe8.7.dat",
                           stop(sprintf(
                             "Unrecognized GCAM version: '%s'. To proceed, add this version to `test_reporter_hist` and provide a corresponding GCAM output sample.",
                             GCAMv
                           ))) # TODO add this info to the documentation site

    generate_report(prj_name = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs", sub("^v", "v_", GCAMv), prjtest_name),
                    final_year = 2030, launch_ui = FALSE, GCAM_version = GCAMv, save_output = FALSE,
                    ignore = c('bio-ceiling','coal-elec-constraint','Worldbiolim',
                               'CO2_NearTerm','wind_offshore-trial-supply',
                               'biolim','biolim CCS','Referenceining',
                               'CO2_LTG','globalCO2_LTG','enh_wew_ceiling'))


    # load expected output
    base_dir <- file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs", "historical_data")

    if (grepl('Europe',GCAMv)) {
      file_list <- list.files(
        path = base_dir,
        pattern = "^hist_data_EUR.*\\.RData$",
        full.names = TRUE
      )
    } else if ('Ukraine' %in% unique(report$Region)) {
      file_list <- list.files(
        path = base_dir,
        pattern = "^hist_data_wUkraine.*\\.RData$",
        full.names = TRUE
      )
    } else {
      file_list <- list.files(
        path = base_dir,
        pattern = "^hist_data_woUkraine.*\\.RData$",
        full.names = TRUE
      )
    }
    # bind all datasets
    report_expected <- dplyr::bind_rows(lapply(file_list, function(file) {
      # temporary environment so loaded objects don't overwrite each other
      temp_env <- new.env()

      # load() and extract the data
      obj_name <- load(file, envir = temp_env)
      get(obj_name[1], envir = temp_env)
    }))

    # reshape to estimate the confidence interval by each Region-Variable
    cols_to_check <- c("Region", "Variable", "2005", "2010", "2015")
    if ("2021" %in% colnames(report)) cols_to_check <- c(cols_to_check, "2021")

    report_expected <- report_expected %>%
      dplyr::select(dplyr::all_of(cols_to_check),'Model','Unit') %>%
      dplyr::filter(Variable %in% intersect(unique(report$Variable), unique(report_expected$Variable)),
                    Region %in% intersect(unique(report$Region), unique(report_expected$Region))) %>%
      dplyr::distinct() %>%
      tidyr::pivot_longer(
        cols = tidyselect::where(is.numeric),
        names_to = "Year",
        values_to = "expected_val"
      ) %>%
      dplyr::group_by(Region,Variable,Unit,Year) %>%
      dplyr::summarise(
        expected_floor = floor(min(expected_val) - 0.2 * abs(min(expected_val))),
        expected_ceil  = ceiling(max(expected_val) + 0.2 * abs(max(expected_val))),
        expected_val = round(mean(expected_val), 2)
      ) %>%
      dplyr::ungroup()

    # actual data
    actual_subset <- report %>%
      dplyr::select(dplyr::all_of(cols_to_check),'Scenario') %>%
      dplyr::filter(Variable %in% intersect(unique(report$Variable), unique(report_expected$Variable)),
                    Region %in% intersect(unique(report$Region), unique(report_expected$Region)))

    # rearrange data
    actual_long <- actual_subset %>%
      tidyr::pivot_longer(
        cols = tidyselect::where(is.numeric),
        names_to = "Year",
        values_to = "actual_val"
      )

    # join and find any rows that fall OUTSIDE the bracket
    mismatches <- actual_long %>%
      dplyr::inner_join(report_expected, by = c("Region", "Variable", "Year")) %>%
      dplyr::filter(actual_val < expected_floor | actual_val > expected_ceil)

    # run the test
    testthat::expect_equal(
      nrow(mismatches),
      0,
      info = paste(
        "Historical values fall outside the expected [floor, ceiling] bracket.",
        "Run `print(mismatches)` in your console to see the exact failing rows."
      )
    )


    rm(list = ls()); gc()
  }
})











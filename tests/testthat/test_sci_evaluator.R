library(gcamreport)
library(testthat)
library(magrittr)

test_that("Test_scenario_compass_evaluator_XLSX", {

  scenariocompass_checker(report = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs", "v_7.0", "test7_standardized.xlsx"),
                          GCAM_version = 'v7.0', save_output = FALSE)

  # run the test
  testthat::expect(exists('scencompass_checker'), '`scencompass_checker` does not exist.')
})

test_that("Test_scenario_compass_evaluator_CSV", {

  scenariocompass_checker(report = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs", "v_7.0", "test7_standardized.csv"),
                          GCAM_version = 'v7.0', save_output = FALSE)

  # run the test
  testthat::expect(exists('scencompass_checker'), '`scencompass_checker` does not exist.')
})


test_that("Test_scenario_compass_evaluator_7p1", {

  generate_report(prj_name = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs", "v_7.1", "database_basexdb_hist_7p1.dat"),
                  final_year = 2050, launch_ui = FALSE, GCAM_version = "v7.1", save_output = FALSE,
                  ignore = c('bio-ceiling','coal-elec-constraint','Worldbiolim',
                             'CO2_NearTerm','wind_offshore-trial-supply',
                             'biolim','biolim CCS','Referenceining',
                             'CO2_LTG','globalCO2_LTG','enh_wew_ceiling'),
                  scenariocompass_evaluation = T)

  # run the test
  testthat::expect(exists('scencompass_checker'), '`scencompass_checker` does not exist.')

})


test_that("Test_scenario_compass_evaluator_RData", {

    generate_report(prj_name = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs", "v_Europe8.7", "db_test_Europe8.7.dat"),
                    final_year = 2050, launch_ui = FALSE, GCAM_version = "vEurope8.7", save_output = FALSE,
                    ignore = c('bio-ceiling','coal-elec-constraint','Worldbiolim',
                               'CO2_NearTerm','wind_offshore-trial-supply',
                               'biolim','biolim CCS','Referenceining',
                               'CO2_LTG','globalCO2_LTG','enh_wew_ceiling'),
                    scenariocompass_evaluation = T,
                    scenariocompass_regions = 'World')

    # run the test
    testthat::expect(exists('scencompass_checker'), '`scencompass_checker` does not exist.')
    testthat::expect(unique(scencompass_checker$region) == 'World', '`scencompass_evalator` regional filters do not work.')
})



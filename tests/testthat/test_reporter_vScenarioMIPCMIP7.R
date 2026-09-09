library(gcamreport)
library(testthat)
library(magrittr)

test_that("Test_vScenarioMIPCMIP7", {
  GCAMv = 'vScenarioMIPCMIP7'

  generate_report(prj_name = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_ScenarioMIPCMIP7/db_exp_scenarioMIPcmip7.dat"),
                            desired_variables = 'All',
                            launch_ui = FALSE, GCAM_version = GCAMv, save_output = FALSE,
                            ignore = c('bio-ceiling','coal-elec-constraint',
                                       'CO2_NearTerm','wind_offshore-trial-supply',
                                       'CO2_LTG','globalCO2_LTG','enh_wew_ceiling'))

  reported_variables <- gcamreport::template_vScenarioMIPCMIP7 %>%
    dplyr::filter(!is.null(Internal_variable)) %>%
    dplyr::pull(Variable) %>%
    sub("\\|.*", "", .) %>%
    unique()

  testExpect <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat),
                                   "testOutputs/v_ScenarioMIPCMIP7/test_report.RData")))

  for (var in reported_variables) {
    print(var)

    report_expected <- testExpect %>%
      dplyr::filter(startsWith(Variable,var))

    report_filtered <- report %>%
      dplyr::filter(startsWith(Variable,var))

    testthat::expect_equal(report_expected, report_filtered,
                           info = paste0("The reported '",var, "' variable data does not match the expected data."))
  }

})


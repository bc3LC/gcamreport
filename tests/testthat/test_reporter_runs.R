library(gcamreport)
library(testthat)
library(magrittr)

test_that("Test_generate_report_runs", {

  for (GCAMv in gcamreport::available_GCAM_versions) {
    print(GCAMv)

    prjtest_name <- switch(GCAMv,
                           "v7.0"              = "test7.dat",
                           "v7.1"              = "database_basexdb_hist_7p1.dat",
                           "v7.2"              = "test7p2.dat",
                           "v8.2"              = "gcamreport_onboard8p2_Ctax_260210.dat",
                           "v9.1"              = "test9p1.dat",
                           "vScenarioMIPCMIP7" = "db_exp_scenarioMIPcmip7.dat",
                           "vEurope7.2"        = "db_test_Europe7.2.dat",
                           "vEurope8.7"        = "db_test_Europe8.7.dat",
                           stop(sprintf(
                             "Unrecognized GCAM version: '%s'. To proceed, add this version to `test_reporter_hist` and provide a corresponding GCAM output sample.",
                             GCAMv
                           )))

    # test that generate_report runs without throwing any errors
    testthat::expect_error({
      generate_report(prj_name = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs", sub("^v", "v_", GCAMv), prjtest_name),
                      final_year = 2030, launch_ui = FALSE, GCAM_version = GCAMv, save_output = FALSE,
                      ignore = c('bio-ceiling','coal-elec-constraint','Worldbiolim',
                                 'CO2_NearTerm','wind_offshore-trial-supply',
                                 'biolim','biolim CCS','Referenceining',
                                 'CO2_LTG','globalCO2_LTG','enh_wew_ceiling'))
    }, NA)

    # clean memory
    rm(prjtest_name); gc()
  }
})

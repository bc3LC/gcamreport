library(gcamreport); library(testthat); library(magrittr); library(rprojroot)

test_that("Test1. load project function test", {
  testResult = as.numeric(length(load_project(paste0(rprojroot::find_root(rprojroot::is_testthat),'/testInputs/test6.dat'))))
  testthat::expect(!is.null(testResult), 'Null project. Check if the path exists or the "load_project" function works correctly.')
})

test_that("Test2. run function test: dataset created", {
  run(project_path = paste0(rprojroot::find_root(rprojroot::is_testthat),'/testInputs/test6.dat'), launch_ui = FALSE)
  testthat::expect(!is.null(final_data) & dplyr::n_distinct(final_data) > 0, 'Empty dataset. Check if the project path exists or the "run" function works correctly.')
})

test_that("Test3. run function test: dataset saved with file_name specified", {
  if (!dir.exists(paste0(rprojroot::find_root(rprojroot::is_testthat), "/testOutputs/"))){
    dir.create(paste0(rprojroot::find_root(rprojroot::is_testthat), "/testOutputs/"))
  }
  run(project_path = paste0(rprojroot::find_root(rprojroot::is_testthat),'/testInputs/test6.dat'), launch_ui = FALSE,
      file_name = paste0(rprojroot::find_root(rprojroot::is_testthat),'/testOutputs/test6_output'))

  testResult = read.csv(paste0(rprojroot::find_root(rprojroot::is_testthat),'/testOutputs/test6_output.csv'))
  testthat::expect(dplyr::n_distinct(testResult) > 0, 'Dataset not saved. Check if the project path exists or the "run" function works correctly.')

  testResult = readxl::read_excel(paste0(rprojroot::find_root(rprojroot::is_testthat),'/testOutputs/test6_output.xlsx'))
  testthat::expect(dplyr::n_distinct(testResult) > 0, 'Dataset not saved. Check if the project path exists or the "run" function works correctly.')
})

test_that("Test4. run function test: dataset saved with default file_name", {
  # in principle, data should be already saved due to the Test2
  testResult = read.csv(paste0(rprojroot::find_root(rprojroot::is_testthat),'/testInputs/test6_ipcc_report.csv'))

  testthat::expect(dplyr::n_distinct(testResult) > 0, 'Dataset not saved. Check if the project path exists or the "run" function works correctly.')
})

test_that("Test5. load variable test", {
  vv = get(load(paste0(rprojroot::find_root(rprojroot::is_testthat),'/testOutputs/results_test5.RData')))

  load_variable(vv[3,])

  testthat::expect(exists("ag_prices_wld"), 'Loading variables function is broken.')
})

test_that("Test6. get functions", {
  get_elec_capacity_tot()
  testthat::expect(exists("elec_capacity_tot_clean"), 'get_elec_capacity_tot() function is broken.')
  testResult = get(load(paste0(rprojroot::find_root(rprojroot::is_testthat),'/testOutputs/result_test6.1.RData')))
  testthat::expect_equal(elec_capacity_tot_clean, testResult)

  get_elec_capital()
  testthat::expect(exists("elec_capital_clean"), 'get_elec_capital() function is broken.')
  testResult = get(load(paste0(rprojroot::find_root(rprojroot::is_testthat),'/testOutputs/result_test6.2.RData')))
  testthat::expect_equal(elec_capital_clean, testResult)
})

# test_that("Test7. create project and run", {
#   db_path <<- paste0(rprojroot::find_root(rprojroot::is_testthat),'/testInputs')
#   db_name <<- "database_basexdb_gcamreport"
#   prj_name <<- "test_prj.dat"
#   scenarios <<- 'Reference'
#
#   create_project(db_path, db_name, prj_name, scenarios)
#   testResult = get(load(paste0(rprojroot::find_root(rprojroot::is_testthat),'/testOutputs/database_basexdb_gcamreport_test_prj.dat')))
#   testthat::expect_equal(prj$Reference$`nonCO2 emissions by region`, testResult$Reference$`nonCO2 emissions by region`)
#   testthat::expect_equal(prj$Reference$`nonCO2 emissions by sector`, testResult$Reference$`nonCO2 emissions by sector`)
#   testthat::expect_equal(prj$Reference$`CO2 prices`, testResult$Reference$`CO2 prices`)
#
#   dt_sec = data_query('nonCO2 emissions by sector')
#   testResult = get(load(paste0(rprojroot::find_root(rprojroot::is_testthat),'/testOutputs/result_test7.RData')))
#   testthat::expect_equal(dt_sec, testResult)
#
# })

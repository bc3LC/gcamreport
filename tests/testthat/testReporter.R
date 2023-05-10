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
      file_name = paste0(rprojroot::find_root(rprojroot::is_testthat),'/testOutputs/test6_output.csv'))
  testResult = read.csv(paste0(rprojroot::find_root(rprojroot::is_testthat),'/testOutputs/test6_output.csv'))

  testthat::expect(dplyr::n_distinct(testResult) > 0, 'Dataset not saved. Check if the project path exists or the "run" function works correctly.')
})

test_that("Test4. run function test: dataset saved with default file_name", {
  # in principle, data should be already saved due to the Test2
  testResult = read.csv(paste0(rprojroot::find_root(rprojroot::is_testthat),'/testInputs/test6_ipcc_report.csv'))

  testthat::expect(dplyr::n_distinct(testResult) > 0, 'Dataset not saved. Check if the project path exists or the "run" function works correctly.')
})

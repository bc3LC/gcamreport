library(gcamreport); library(testthat); library(magrittr); library(rprojroot); library(rpackageutils)

test_that("Test1_v7. download db, create project, and run", {
  # load a reference GCAM db form a Zenodo repository
  db_path = paste0(rprojroot::find_root(rprojroot::is_testthat),'/testInputs/v_7.0')
  rpackageutils::download_unpack_zip(data_directory = db_path,
                                     url = "https://zenodo.org/record/10258919/files/database_basexdb_ref.zip?download=1")
  testthat::expect_equal(1, 1)

  # create the prj
  db_name = "database_basexdb_ref"
  prj_name = "gcamv7.0_test.dat"
  scenarios = 'Reference'

  create_project(db_path, db_name, prj_name, scenarios)
  prj_tmp = prj
  testResult = get(load(paste0(rprojroot::find_root(rprojroot::is_testthat),'/testOutputs/v_7.0/test7.dat')))
  prj = prj_tmp
  testthat::expect_equal(prj$Reference$`nonCO2 emissions by region`, testResult$Reference$`nonCO2 emissions by region`)
  testthat::expect_equal(prj$Reference$`nonCO2 emissions by sector`, testResult$Reference$`nonCO2 emissions by sector (excluding resource production)`)
  testthat::expect_equal(prj$Reference$`CO2 prices`, testResult$Reference$`CO2 prices`)

  # check nonCO2 emissions query
  dt_sec = data_query('nonCO2 emissions by sector (excluding resource production)', db_path, db_name, prj_name, scenarios)
  testResult = get(load(paste0(rprojroot::find_root(rprojroot::is_testthat),'/testOutputs/v_7.0/result_test1.RData')))
  testthat::expect_equal(dt_sec, testResult)

  # clean environment
  rm(list = ls())
})

test_that("Test2_v7. load project", {
  testResult = as.numeric(length(load_project(paste0(rprojroot::find_root(rprojroot::is_testthat),'/testInputs/v_7.0/test7.dat'))))
  testthat::expect(!is.null(testResult), 'Null project. Check if the path exists or the "load_project" function works correctly.')

  # clean environment
  rm(list = ls())
})

test_that("Test3_v7. run - dataset created", {
  run(project_path = paste0(rprojroot::find_root(rprojroot::is_testthat),'/testInputs/v_7.0/test7.dat'), launch_ui = FALSE)
  testthat::expect(!is.null(final_data) & dplyr::n_distinct(final_data) > 0, 'Empty dataset. Check if the project path exists or the "run" function works correctly.')

  # clean environment
  rm(list = ls())
})

test_that("Test4_v7. run - dataset saved with output_file specified", {
  run(project_path = paste0(rprojroot::find_root(rprojroot::is_testthat),'/testInputs/v_7.0/test7.dat'), launch_ui = FALSE,
      output_file = paste0(rprojroot::find_root(rprojroot::is_testthat),'/testOutputs/v_7.0/test7_output'))

  testResult1 = read.csv(paste0(rprojroot::find_root(rprojroot::is_testthat),'/testOutputs/v_7.0/test7_output.csv'))
  testthat::expect(dplyr::n_distinct(testResult1) > 0, 'Dataset not saved. Check if the project path exists or the "run" function works correctly.')
  testResult2 = read.csv(paste0(rprojroot::find_root(rprojroot::is_testthat),'/testOutputs/v_7.0/result_test4.1.csv'))
  testthat::expect_equal(testResult1 %>%
                           dplyr::select(-Unit),
                         testResult2 %>%
                           dplyr::select(-Unit))

  testResult1 = readxl::read_excel(paste0(rprojroot::find_root(rprojroot::is_testthat),'/testOutputs/v_7.0/test7_output.xlsx'))
  testthat::expect(dplyr::n_distinct(testResult1) > 0, 'Dataset not saved. Check if the project path exists or the "run" function works correctly.')
  testResult2 = readxl::read_excel(paste0(rprojroot::find_root(rprojroot::is_testthat),'/testOutputs/v_7.0/result_test4.2.xlsx'))
  testthat::expect_equal(testResult1 %>%
                           dplyr::select(-Unit),
                         testResult2 %>%
                           dplyr::select(-Unit))

  # clean environment
  rm(list = ls())
})

test_that("Test5_v7. run - dataset saved with default output_file", {
  run(project_path = paste0(rprojroot::find_root(rprojroot::is_testthat),'/testInputs/v_7.0/test7.dat'), launch_ui = FALSE)

  testResult = read.csv(paste0(rprojroot::find_root(rprojroot::is_testthat),'/testInputs/v_7.0/test7_standardized.csv'))
  testthat::expect(dplyr::n_distinct(testResult) > 0, 'Dataset not saved. Check if the project path exists or the "run" function works correctly.')

  testResult = readxl::read_excel(paste0(rprojroot::find_root(rprojroot::is_testthat),'/testInputs/v_7.0/test7_standardized.xlsx'))
  testthat::expect(dplyr::n_distinct(testResult) > 0, 'Dataset not saved. Check if the project path exists or the "run" function works correctly.')

  # do not clean environment
})

test_that("Test6_v7. load variable and get function", {
  vv = get(load(paste0(rprojroot::find_root(rprojroot::is_testthat),'/testOutputs/v_7.0/results_test6.RData')))

  loaded_internal_variables <<- c()
  desired_regions <<- 'All'
  desired_variables <<- 'All'
  variables_base <- data.frame('name' = unique(template$Internal_variable)[!is.na(unique(template$Internal_variable)) & unique(template$Internal_variable) != ""],
                               'required' = TRUE,
                               stringsAsFactors = FALSE)
  variables <<- merge(variables_base,var_fun_map, by = 'name', all = TRUE) %>%
    tidyr::replace_na(list(required = FALSE))

  load_variable(vv)

  testthat::expect(exists("ag_prices_wld"), 'Loading variables function is broken.')

  get_elec_capital()
  testthat::expect(exists("elec_capital_clean"), 'get_elec_capital() function is broken.')
  testResult = get(load(paste0(rprojroot::find_root(rprojroot::is_testthat),'/testOutputs/v_7.0/result_test6.1.RData')))
  testthat::expect_equal(elec_capital_clean, testResult)

  # clean environment
  rm(list = ls())
})

test_that("Test7_v7. specify variables, regions, continents", {
  test_regions = available_regions(T)
  testResult_regions = c("Africa_Eastern","Africa_Northern","Africa_Southern",
                         "Africa_Western","Argentina","Australia_NZ",
                         "Brazil","Canada","Central America and Caribbean",
                         "Central Asia","China","Colombia",
                         "European Free Trade Association","EU-12","EU-15",
                         "Europe_Eastern","Europe_Non_EU","India",
                         "Indonesia","Japan","Mexico",
                         "Middle East","Pakistan","Russia",
                         "South Africa","South America_Northern","South America_Southern",
                         "South Asia","South Korea","Southeast Asia",
                         "Taiwan","USA","World")
  testthat::expect_equal(test_regions, testResult_regions)

  test_continents = available_continents(T)
  testResult_continents = c("MAF","LAM","OECD90","REF","ASIA","World")
  testthat::expect_equal(test_continents, testResult_continents)

  test_variables = available_variables(T)
  testResult_variables = get(load(paste0(rprojroot::find_root(rprojroot::is_testthat),'/testOutputs/v_7.0/result_test7.1.RData')))
  testthat::expect_equal(test_variables, testResult_variables)

  rm(list = ls())
  run(db_path = paste0(rprojroot::find_root(rprojroot::is_testthat),'/testInputs/v_7.0'),
      db_name = 'database_basexdb_ref',
      prj_name = 'gcamv7.0_test.dat',
      scenarios = 'Reference',
      final_year = 2050,
      desired_continents = 'OECD90',
      desired_variables = available_variables(F)[c(1,3,10,11)],
      launch_ui = FALSE)
  testthat::expect_equal(unique(final_data$Variable), c("Agricultural Demand",
                                                        "Agricultural Demand|Crops|Energy",
                                                        "Agricultural Production",
                                                        "Capacity Additions|Electricity|Biomass"))
  testthat::expect_equal(unique(final_data$Region), c("Australia_NZ","Canada","EU-12","EU-15",
                                                      "Europe_Non_EU","European Free Trade Association",
                                                      "Japan","USA","World"))

  rm(list = ls())
  run(db_path = paste0(rprojroot::find_root(rprojroot::is_testthat),'/testInputs/v_7.0'),
      db_name = 'database_basexdb_ref',
      prj_name = 'gcamv7.0_test.dat',
      scenarios = 'Reference',
      final_year = 2050,
      desired_continents = 'OECD90',
      desired_variables = 'Emissions*',
      launch_ui = FALSE)
  testResult = get(load(paste0(rprojroot::find_root(rprojroot::is_testthat),'/testOutputs/v_7.0/result_test7.2.RData')))
  testthat::expect_equal(unique(final_data$Variable), testResult)

  rm(list = ls())
  run(db_path = paste0(rprojroot::find_root(rprojroot::is_testthat),'/testInputs/v_7.0'),
      db_name = 'database_basexdb_ref',
      prj_name = 'gcamv7.0_test.dat',
      scenarios = 'Reference',
      final_year = 2050,
      desired_regions = 'USA',
      desired_variables = 'Price|Carbon',
      launch_ui = FALSE)
  testResult = get(load(paste0(rprojroot::find_root(rprojroot::is_testthat),'/testOutputs/v_7.0/result_test7.3.RData')))
  testthat::expect_equal(unique(final_data$Variable), testResult)

  rm(list = ls())
  run(db_path = paste0(rprojroot::find_root(rprojroot::is_testthat),'/testInputs/v_7.0'),
      db_name = 'database_basexdb_ref',
      prj_name = 'gcamv7.0_test.dat',
      scenarios = 'Reference',
      final_year = 2050,
      desired_regions = 'USA',
      desired_variables = 'Price|Carbon*',
      launch_ui = FALSE)
  testResult = get(load(paste0(rprojroot::find_root(rprojroot::is_testthat),'/testOutputs/v_7.0/result_test7.4.RData')))
  testthat::expect_equal(unique(final_data$Variable), testResult)

})

test_that("Test8_v7. error messages", {

  expect_error(run(db_path = paste0(rprojroot::find_root(rprojroot::is_testthat),'/testInputs/v_7.0/'),
                   db_name = 'database_basexdb',
                   prj_name = 'gcamv7.0_noCreated.dat',
                   scenarios = 'Reference',
                   desired_variables = 'dummy variable',
                   launch_ui = FALSE),
               "ERROR: You specified the variable dummy variable which is not available for reporting.")

  expect_error(run(db_path = paste0(rprojroot::find_root(rprojroot::is_testthat),'/testInputs/v_7.0/'),
                   db_name = 'database_basexdb',
                   prj_name = 'gcamv7.0_noCreated.dat',
                   scenarios = 'Reference',
                   desired_variables = 'Final|Energy*',
                   launch_ui = FALSE),
               "ERROR: You specified the variable Final|Energy* which is not available for reporting.")

  expect_error(run(db_path = paste0(rprojroot::find_root(rprojroot::is_testthat),'/testInputs/v_7.0/'),
                   db_name = 'database_basexdb',
                   prj_name = 'gcamv7.0_noCreated.dat',
                   scenarios = 'Reference',
                   desired_region = 'dummy region',
                   launch_ui = FALSE),
               "ERROR: You specified the region dummy region which is not available for reporting.")

  expect_error(run(db_path = paste0(rprojroot::find_root(rprojroot::is_testthat),'/testInputs/v_7.0/'),
                   db_name = 'database_basexdb',
                   prj_name = 'gcamv7.0_p1.dat',
                   scenarios = 'Reference',
                   desired_continents = 'dummy continent',
                   launch_ui = FALSE),
               "ERROR: You specified the continent/regions' group dummy continent which is not available for reporting.")

  expect_error(run(db_path = paste0(rprojroot::find_root(rprojroot::is_testthat),'/testInputs/v_7.0/'),
                   prj_name = 'gcamv7.0_p1.dat',
                   scenarios = 'Reference',
                   launch_ui = FALSE),
               "If db_path, prj_name, scenarios are specified, db_name must also be specified.")

  expect_error(run(project_path = 'dummy name',
                   db_path = 'dummy name',
                   launch_ui = FALSE),
               "ERROR: Specify either a project or a database to extract the data from. Not both.")

})

test_that("Test9_v7. CO2 ETS", {

  rm(list = ls())
  run(db_path = paste0(rprojroot::find_root(rprojroot::is_testthat),'/testInputs/v_7.0'),
      db_name = 'database_basexdb_ref',
      prj_name = 'gcamv7.0_test.dat',
      scenarios = 'Reference',
      final_year = 2050,
      desired_regions = 'China',
      desired_variables = c('Price|Carbon*'),
      launch_ui = FALSE)

  testResult = get(load(paste0(rprojroot::find_root(rprojroot::is_testthat),'/testOutputs/v_7.0/result_test9.1.RData')))
  testthat::expect_equal(prj$Reference$`CO2 prices`, testResult)

  CO2_market_filteredReg = filter_data_regions(CO2_market)
  testResult = get(load(paste0(rprojroot::find_root(rprojroot::is_testthat),'/testOutputs/v_7.0/result_test9.2.RData')))
  testthat::expect_equal(CO2_market_filteredReg, testResult)

  # clean environment
  rm(list = ls())
})

test_that("Test10_v7. vetting", {

  run(db_path = paste0(rprojroot::find_root(rprojroot::is_testthat),'/testInputs/v_7.0'),
      db_name = 'database_basexdb_ref',
      prj_name = 'gcamv7.0_test.dat',
      scenarios = 'Reference',
      final_year = 2050,
      desired_regions = 'All',
      desired_variables = c('Emissions|CH4*'),
      launch_ui = FALSE)

  testResult = get(load(paste0(rprojroot::find_root(rprojroot::is_testthat),'/testOutputs/v_7.0/result_test10.1.RData')))
  testthat::expect_equal(vetting_summary, testResult)

  rm(list = ls())
  run(db_path = paste0(rprojroot::find_root(rprojroot::is_testthat),'/testInputs/v_7.0'),
      db_name = 'database_basexdb_ref',
      prj_name = 'gcamv7.0_test.dat',
      scenarios = 'Reference',
      final_year = 2050,
      desired_regions = 'All',
      desired_variables = c('Emissions|Sulfur*'),
      launch_ui = FALSE)

  testResult = get(load(paste0(rprojroot::find_root(rprojroot::is_testthat),'/testOutputs/v_7.0/result_test10.2.RData')))
  testthat::expect_equal(vetting_summary, testResult)

  rm(list = ls())
  run(db_path = paste0(rprojroot::find_root(rprojroot::is_testthat),'/testInputs/v_7.0'),
      db_name = 'database_basexdb_ref',
      prj_name = 'gcamv7.0_test.dat',
      scenarios = 'Reference',
      final_year = 2050,
      desired_regions = 'South Africa',
      desired_variables = c('Final Energy*'),
      launch_ui = FALSE)

  testthat::expect(exists('vetting_summary'), 'ERROR: vetting performed when not all regions were selected')

  # clean environment
  rm(list = ls())
})

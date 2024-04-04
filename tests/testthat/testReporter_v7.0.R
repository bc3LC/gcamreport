library(gcamreport)
library(testthat)
library(magrittr)

test_that("Test1_v7. download db, create project, and run", {
  # load a reference GCAM db form a Zenodo repository
  db_path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0")
  rpackageutils::download_unpack_zip(
    data_directory = db_path,
    url = "https://zenodo.org/record/10258919/files/database_basexdb_ref.zip?download=1"
  )
  testthat::expect_equal(1, 1)

  # create the prj
  db_name <- "database_basexdb_ref"
  prj_name <- "gcamv7.0_test.dat"
  scenarios <- "Reference"

  create_project(db_path, db_name, prj_name, scenarios)
  prj_tmp <- prj
  testResult <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/test7.dat")))
  prj <- prj_tmp
  testthat::expect_equal(prj$Reference$`nonCO2 emissions by region`, testResult$Reference$`nonCO2 emissions by region`)
  testthat::expect_equal(prj$Reference$`nonCO2 emissions by sector`, testResult$Reference$`nonCO2 emissions by sector (excluding resource production)`)
  testthat::expect_equal(prj$Reference$`CO2 prices`, testResult$Reference$`CO2 prices`)

  # check nonCO2 emissions query
  dt_sec <- data_query("nonCO2 emissions by sector (excluding resource production)", db_path, db_name, prj_name, scenarios)
  testResult <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/result_test1.RData")))
  testthat::expect_equal(dt_sec, testResult)
})

test_that("Test2_v7. load project", {
  testResult <- as.numeric(length(load_project(file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test7.dat"))))
  testthat::expect(!is.null(testResult), 'Null project. Check if the path exists or the "load_project" function works correctly.')
})

test_that("Test3_v7. run - dataset created", {
  generate_report(project_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test7.dat"), launch_ui = FALSE)
  testthat::expect(!is.null(report) & dplyr::n_distinct(report) > 0, 'Empty dataset. Check if the project path exists or the "run" function works correctly.')
})

test_that("Test4_v7. run - dataset saved with output_file specified", {
  generate_report(
    project_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test7.dat"), launch_ui = FALSE,
    output_file = file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/test7_output")
  )

  testResult1 <- read.csv(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/test7_output.csv"))
  testthat::expect(dplyr::n_distinct(testResult1) > 0, 'Dataset not saved. Check if the project path exists or the "run" function works correctly.')
  testResult2 <- read.csv(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/result_test4.1.csv"))
  testthat::expect_equal(
    testResult1 %>%
      dplyr::select(-Unit),
    testResult2 %>%
      dplyr::select(-Unit)
  )

  testResult1 <- readxl::read_excel(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/test7_output.xlsx"))
  testthat::expect(dplyr::n_distinct(testResult1) > 0, 'Dataset not saved. Check if the project path exists or the "run" function works correctly.')
  testResult2 <- readxl::read_excel(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/result_test4.2.xlsx"))
  testthat::expect_equal(
    testResult1 %>%
      dplyr::select(-Unit),
    testResult2 %>%
      dplyr::select(-Unit)
  )
})

test_that("Test5_v7. run - dataset saved with default output_file", {
  generate_report(project_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test7.dat"), launch_ui = FALSE)

  testResult <- read.csv(file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test7_standardized.csv"))
  testthat::expect(dplyr::n_distinct(testResult) > 0, 'Dataset not saved. Check if the project path exists or the "run" function works correctly.')

  testResult <- readxl::read_excel(file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test7_standardized.xlsx"))
  testthat::expect(dplyr::n_distinct(testResult) > 0, 'Dataset not saved. Check if the project path exists or the "run" function works correctly.')
})

test_that("Test6_v7. load variable and get function", {
  # load prj
  generate_report(project_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test7.dat"), launch_ui = FALSE)

  # load variables
  vv <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/results_test6.RData")))
  loaded_internal_variables.global <<- c()
  desired_regions <<- "All"
  desired_variables <<- "All"
  variables_base <- data.frame(
    "name" = unique(template$Internal_variable)[!is.na(unique(template$Internal_variable)) & unique(template$Internal_variable) != ""],
    "required" = TRUE,
    stringsAsFactors = FALSE
  )
  variables.global <<- merge(variables_base, var_fun_map, by = "name", all = TRUE) %>%
    tidyr::replace_na(list(required = FALSE))

  # test
  load_variable(vv)

  testthat::expect(exists("ag_prices_wld"), "Loading variables function is broken.")

  get_elec_capital()
  testthat::expect(exists("elec_capital_clean"), "get_elec_capital() function is broken.")
  testResult <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/result_test6.1.RData")))
  testthat::expect_equal(elec_capital_clean, testResult)
})

test_that("Test7_v7. specify variables, regions, continents", {
  test_regions <- available_regions(T)
  testResult_regions <- c(
    "Africa_Eastern", "Africa_Northern", "Africa_Southern",
    "Africa_Western", "Argentina", "Australia_NZ",
    "Brazil", "Canada", "Central America and Caribbean",
    "Central Asia", "China", "Colombia",
    "European Free Trade Association", "EU-12", "EU-15",
    "Europe_Eastern", "Europe_Non_EU", "India",
    "Indonesia", "Japan", "Mexico",
    "Middle East", "Pakistan", "Russia",
    "South Africa", "South America_Northern", "South America_Southern",
    "South Asia", "South Korea", "Southeast Asia",
    "Taiwan", "USA", "World"
  )
  testthat::expect_equal(test_regions, testResult_regions)

  test_continents <- available_continents(T)
  testResult_continents <- c("MAF", "LAM", "OECD90", "REF", "ASIA", "World")
  testthat::expect_equal(test_continents, testResult_continents)

  test_variables <- available_variables(T)
  testResult_variables <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/result_test7.1.RData")))
  testthat::expect_equal(test_variables, testResult_variables)

  rm(list = ls())
  generate_report(
    db_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0"),
    db_name = "database_basexdb_ref",
    prj_name = "gcamv7.0_test.dat",
    scenarios = "Reference",
    final_year = 2050,
    desired_continents = "OECD90",
    desired_variables = available_variables(F)[c(1, 3, 10, 11)],
    launch_ui = FALSE
  )
  testthat::expect_equal(unique(report$Variable), c(
    "Agricultural Demand",
    "Agricultural Demand|Crops|Energy",
    "Agricultural Production",
    "Capacity Additions|Electricity|Biomass"
  ))
  testthat::expect_equal(unique(report$Region), c(
    "Australia_NZ", "Canada", "EU-12", "EU-15",
    "Europe_Non_EU", "European Free Trade Association",
    "Japan", "USA", "World"
  ))

  rm(list = ls())
  generate_report(
    db_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0"),
    db_name = "database_basexdb_ref",
    prj_name = "gcamv7.0_test.dat",
    scenarios = "Reference",
    final_year = 2050,
    desired_continents = "OECD90",
    desired_variables = "Emissions*",
    launch_ui = FALSE
  )
  testResult <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/result_test7.2.RData")))
  testthat::expect_equal(unique(report$Variable), testResult)

  rm(list = ls())
  generate_report(
    db_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0"),
    db_name = "database_basexdb_ref",
    prj_name = "gcamv7.0_test.dat",
    scenarios = "Reference",
    final_year = 2050,
    desired_regions = "USA",
    desired_variables = "Price|Carbon",
    launch_ui = FALSE
  )
  testResult <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/result_test7.3.RData")))
  testthat::expect_equal(unique(report$Variable), testResult)

  rm(list = ls())
  generate_report(
    db_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0"),
    db_name = "database_basexdb_ref",
    prj_name = "gcamv7.0_test.dat",
    scenarios = "Reference",
    final_year = 2050,
    desired_regions = "USA",
    desired_variables = "Price|Carbon*",
    launch_ui = FALSE
  )
  testResult <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/result_test7.4.RData")))
  testthat::expect_equal(unique(report$Variable), testResult)
})

test_that("Test8_v7. error messages", {
  expect_error(
    generate_report(
      db_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/"),
      db_name = "database_basexdb_ref",
      prj_name = "gcamv7.0_noCreated.dat",
      scenarios = "Reference",
      desired_variables = "dummy variable",
      launch_ui = FALSE
    ),
    "The variable dummy variable is not available for reporting"
  )
  expect_error(
    generate_report(
      db_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/"),
      db_name = "database_basexdb_ref",
      prj_name = "gcamv7.0_noCreated.dat",
      scenarios = "Reference",
      desired_variables = c("dummy1", "dummy2"),
      launch_ui = FALSE
    ),
    "The variables dummy1, dummy2 are not available for reporting"
  )

  expect_error(
    generate_report(
      db_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/"),
      db_name = "database_basexdb_ref",
      prj_name = "gcamv7.0_noCreated.dat",
      scenarios = "Reference",
      desired_variables = "Final|Energy*",
      launch_ui = FALSE
    ),
    "There is no variable containing the pattern Final|Energy* available for reporting."
  )
  expect_error(
    generate_report(
      db_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/"),
      db_name = "database_basexdb_ref",
      prj_name = "gcamv7.0_noCreated.dat",
      scenarios = "Reference",
      desired_variables = c("Final|Energy*", "Emissions CH4*"),
      launch_ui = FALSE
    ),
    "There are no variables containing the patterns Final|Energy*, Emissions CH4* available for reporting"
  )

  expect_error(
    generate_report(
      db_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/"),
      db_name = "database_basexdb_ref",
      prj_name = "gcamv7.0_noCreated.dat",
      scenarios = "Reference",
      desired_regions = "dummy region",
      launch_ui = FALSE
    ),
    "The desired region dummy region is not available for reporting."
  )
  expect_error(
    generate_report(
      db_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/"),
      db_name = "database_basexdb_ref",
      prj_name = "gcamv7.0_noCreated.dat",
      scenarios = "Reference",
      desired_regions = c("dummy1", "dummy2"),
      launch_ui = FALSE
    ),
    "The desired regions dummy1, dummy2 are not available for reporting."
  )

  expect_error(
    generate_report(
      db_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/"),
      db_name = "database_basexdb_ref",
      prj_name = "gcamv7.0_p1.dat",
      scenarios = "Reference",
      desired_continents = "dummy continent",
      launch_ui = FALSE
    ),
    "The desired continent/regions' group dummy continent is not available for reporting."
  )
  expect_error(
    generate_report(
      db_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/"),
      db_name = "database_basexdb_ref",
      prj_name = "gcamv7.0_p1.dat",
      scenarios = "Reference",
      desired_continents = c("dummy1", "dummy2"),
      launch_ui = FALSE
    ),
    "The desired continent/regions' groups dummy1, dummy2 are not available for reporting."
  )

  expect_error(
    generate_report(
      db_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/"),
      prj_name = "gcamv7.0_p1.dat",
      scenarios = "Reference",
      launch_ui = FALSE
    ),
    "If db_path, prj_name are specified, db_name must also be specified."
  )

  expect_error(
    generate_report(
      project_path = "dummy name",
      db_path = "dummy name",
      launch_ui = FALSE
    ),
    "Specify either a project or a database to extract the data from. Not both."
  )

  expect_error(
    generate_report(
      db_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/"),
      db_name = "database_basexdb_ref",
      prj_name = "gcamv7.0_p1.dat",
      scenarios = "Reference",
      desired_regions = "dummy region",
      desired_continents = "dummy continent",
      launch_ui = FALSE
    ),
    "You specified both the desired_regions and the desired_continents parameters. Only one can be specified at a time."
  )
})

test_that("Test9_v7. CO2 ETS", {
  generate_report(
    db_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0"),
    db_name = "database_basexdb_ref",
    prj_name = "gcamv7.0_test.dat",
    scenarios = "Reference",
    final_year = 2050,
    desired_regions = "China",
    desired_variables = c("Price|Carbon*"),
    launch_ui = FALSE
  )

  testResult <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/result_test9.1.RData")))
  testthat::expect_equal(prj$Reference$`CO2 prices`, testResult)

  CO2_market_filteredReg <- filter_data_regions(CO2_market)
  testResult <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/result_test9.2.RData")))
  testthat::expect_equal(CO2_market_filteredReg, testResult)
})

test_that("Test10_v7. vetting", {
  generate_report(
    db_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0"),
    db_name = "database_basexdb_ref",
    prj_name = "gcamv7.0_test.dat",
    scenarios = "Reference",
    final_year = 2050,
    desired_regions = "All",
    desired_variables = c("Emissions|CH4*"),
    launch_ui = FALSE
  )

  testResult <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/result_test10.1.RData")))
  testthat::expect_equal(vetting_summary, testResult)

  rm(list = ls())
  generate_report(
    db_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0"),
    db_name = "database_basexdb_ref",
    prj_name = "gcamv7.0_test.dat",
    scenarios = "Reference",
    final_year = 2050,
    desired_regions = "All",
    desired_variables = c("Emissions|Sulfur*"),
    launch_ui = FALSE
  )

  testResult <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/result_test10.2.RData")))
  testthat::expect_equal(vetting_summary, testResult)

  rm(list = ls())
  generate_report(
    db_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0"),
    db_name = "database_basexdb_ref",
    prj_name = "gcamv7.0_test.dat",
    scenarios = "Reference",
    final_year = 2050,
    desired_regions = "South Africa",
    desired_variables = c("Final Energy*"),
    launch_ui = FALSE
  )

  testthat::expect(exists("vetting_summary"), "Vetting performed when not all regions were selected")
})

test_that("Test11_v7. scenarios", {
  # check when creating project
  expect_error(
    generate_report(
      db_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/"),
      db_name = "database_basexdb_ref",
      prj_name = "gcamv7.0_p1.dat",
      scenarios = c("dummy", "Reference"),
      launch_ui = FALSE
    ),
    "The desired scenario dummy is not present in the database."
  )

  expect_error(
    generate_report(
      db_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/"),
      db_name = "database_basexdb_ref",
      prj_name = "gcamv7.0_p1.dat",
      scenarios = c("dummy1", "dummy2", "Reference"),
      launch_ui = FALSE
    ),
    "The desired scenarios dummy1, dummy2 are not present in the database."
  )

  generate_report(
    db_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0"),
    db_name = "database_basexdb_ref",
    prj_name = "gcamv7.0_test_scenarios.dat",
    final_year = 2050,
    desired_regions = "All",
    desired_variables = c("Emissions|CH4*"),
    launch_ui = FALSE
  )

  testResult <- rgcam::listScenarios(prj)
  testthat::expect_equal("Reference", testResult)

  # check when loading project
  expect_error(
    generate_report(
      db_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/"),
      db_name = "database_basexdb_ref",
      prj_name = "gcamv7.0_test_scenarios.dat",
      scenarios = c("dummy", "Reference"),
      launch_ui = FALSE
    ),
    "The desired scenario dummy is not present in the loaded project."
  )

  expect_error(
    generate_report(
      db_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/"),
      db_name = "database_basexdb_ref",
      prj_name = "gcamv7.0_test_scenarios.dat",
      scenarios = c("dummy1", "dummy2", "Reference"),
      launch_ui = FALSE
    ),
    "The desired scenarios dummy1, dummy2 are not present in the loaded project."
  )

  generate_report(
    db_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0"),
    db_name = "database_basexdb_ref",
    prj_name = "gcamv7.0_test_scenarios.dat",
    final_year = 2050,
    desired_regions = "All",
    desired_variables = c("Emissions|CH4*"),
    launch_ui = FALSE
  )

  testResult <- rgcam::listScenarios(prj)
  testthat::expect_equal("Reference", testResult)

  generate_report(
    db_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0"),
    db_name = "database_basexdb",
    prj_name = "test_scenarios7.dat",
    final_year = 2050,
    scenarios = "CP_EI_recovery",
    desired_regions = "USA",
    desired_variables = "Emissions|CH4*",
    launch_ui = FALSE
  )

  testResult <- rgcam::listScenarios(prj)
  testthat::expect_equal("CP_EI_recovery", testResult)
})


test_that("Test12_v7. other functions", {
  # gather_map
  co2_sector_map <- read.csv(file.path(rprojroot::find_root(rprojroot::is_testthat), "inst/extdata/mappings", "CO2_sector_map.csv"),
                             skip = 1, na = "",
                             stringsAsFactors = FALSE
  ) %>% gather_map()

  testExpect <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/result_test12.1.RData")))
  testthat::expect_equal(co2_sector_map, testExpect)

  # approx_fun
  expect_error(
    approx_fun(2030, 3, rule = 3),
    "Use fill_exp_decay_extrapolate!"
  )

  # get_iron_steel_map & get_co2_iron_steel
  generate_report(
    prj_name = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test7.dat"),
    final_year = 2050,
    scenarios = "Reference",
    desired_regions = "USA",
    desired_variables = "Emissions|CO2|Energy|Demand|Industry|Steel",
    launch_ui = FALSE
  )

  testExpect <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/result_test12.2.RData")))
  testthat::expect_equal(report, testExpect)

  # get_ghg
  generate_report(
    prj_name = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test7.dat"),
    final_year = 2050,
    scenarios = "Reference",
    desired_regions = "USA",
    desired_variables = "Emissions|Kyoto Gases*",
    launch_ui = FALSE
  )

  testExpect <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/result_test12.3.RData")))
  testthat::expect_equal(report, testExpect)

  # get_regional_emission
  testResult <- get_regional_emission()
  testExpect <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/result_test12.4.RData")))
  testthat::expect_equal(testResult, testExpect)

})

test_that("Test13_v7. specify queries", {

  # transform_to_xml ancillary function
  testResult <- transform_to_xml(gcamreport::queries_nonCO2)
  testExpect <- xml2::read_xml(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/result_test13.1.xml"))
  testthat::expect_equal(testResult, testExpect)

  # generate standardize report specifying the query file
  db_path <- file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0")
  db_name <- "database_basexdb_ref"
  prj_name <- "gcamv7.0_test_specify_queries.dat"
  scenarios <- "Reference"
  generate_report(db_path = db_path, db_name = db_name, prj_name = prj_name,
                  scenarios = scenarios, final_year = 2050, desired_variables = c('Price|Carbon*'),
                  save_output = T, launch_ui = F,
                  queries_general_file = file.path(rprojroot::find_root(rprojroot::is_testthat), "inst/extdata/queries/queries_gcamreport_gcam7.0_general.xml"))
  testResult <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/gcamv7.0_test_specify_queries_standardized.RData")))
  testExpect <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/result_test13.2.RData")))
  testthat::expect_equal(testResult, testExpect)

})


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
  testthat::expect_equal(prj$Reference$`nonCO2 emissions by sector (excluding resource production)`, testResult$Reference$`nonCO2 emissions by sector (excluding resource production)`)
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
  generate_report(prj_name = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test7.dat"), launch_ui = FALSE)
  testthat::expect(!is.null(report) & dplyr::n_distinct(report) > 0, 'Empty dataset. Check if the project path exists or the "run" function works correctly.')
})

# test_that("Test4_v7. run - dataset saved with output_file specified - RUN MANUALLY", {
#   generate_report(
#     prj_name = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test7.dat"), launch_ui = FALSE,
#     output_file = file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/test7_output")
#   )
#
#   testResult1 <- read.csv(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/test7_output.csv"))
#   testthat::expect(dplyr::n_distinct(testResult1) > 0, 'Dataset not saved. Check if the project path exists or the "run" function works correctly.')
#   testResult2 <- read.csv(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/result_test4.1.csv"))
#   testthat::expect_equal(
#     testResult1 %>%
#       dplyr::select(-Unit),
#     testResult2 %>%
#       dplyr::select(-Unit)
#   )
#
#   testResult1 <- readxl::read_excel(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/test7_output.xlsx"))
#   testthat::expect(dplyr::n_distinct(testResult1) > 0, 'Dataset not saved. Check if the project path exists or the "run" function works correctly.')
#   testResult2 <- readxl::read_excel(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/result_test4.2.xlsx"))
#   testthat::expect_equal(
#     testResult1 %>%
#       dplyr::select(-Unit),
#     testResult2 %>%
#       dplyr::select(-Unit)
#   )
# })

test_that("Test5_v7. run - dataset saved with default output_file", {
  generate_report(prj_name = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test7.dat"), launch_ui = FALSE)

  testResult <- read.csv(file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test7_standardized.csv"))
  testthat::expect(dplyr::n_distinct(testResult) > 0, 'Dataset not saved. Check if the project path exists or the "run" function works correctly.')

  testResult <- readxl::read_excel(file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test7_standardized.xlsx"))
  testthat::expect(dplyr::n_distinct(testResult) > 0, 'Dataset not saved. Check if the project path exists or the "run" function works correctly.')
})

test_that("Test6_v7. load variable and get function", {
  # load prj
  generate_report(prj_name = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test7.dat"), launch_ui = FALSE)

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
    prj_name = "gcamv7.1_test.dat",
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
    prj_name = "gcamv7.2_test.dat",
    scenarios = "Reference",
    final_year = 2050,
    desired_continents = "OECD90",
    desired_variables = "Emissions*",
    launch_ui = FALSE
  )
  testResult <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/result_test7.2.RData")))
  testthat::expect_equal(unique(report$Variable), testResult)
  testthat::expect_equal(unique(report$Model), 'GCAM 7.0')

  rm(list = ls())
  generate_report(
    db_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0"),
    db_name = "database_basexdb_ref",
    prj_name = "gcamv7.3_test.dat",
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
    prj_name = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test7.dat"),
    scenarios = "Reference",
    final_year = 2050,
    desired_regions = "USA",
    desired_variables = "Price|Carbon",
    launch_ui = FALSE,
    save_output = FALSE
  )
  testthat::expect_equal(unique(report$Region), c("USA", "World"))

  rm(list = ls())
  generate_report(
    db_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0"),
    db_name = "database_basexdb_ref",
    prj_name = "gcamv7.4_test.dat",
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
      prj_name = "gcamv7.8_noCreated.dat",
      scenarios = "Reference",
      desired_variables = "dummy variable",
      launch_ui = FALSE
    ),
    "The variable dummy variable is not available for reporting."
  )
  expect_error(
    generate_report(
      db_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/"),
      db_name = "database_basexdb_ref",
      prj_name = "gcamv7.8_noCreated.dat",
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
      prj_name = "gcamv7.8_noCreated.dat",
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
      prj_name = "gcamv7.8_noCreated.dat",
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
      prj_name = "gcamv7.8_noCreated.dat",
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
      prj_name = "gcamv7.8_noCreated.dat",
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
      prj_name = "gcamv7.8_p1.dat",
      scenarios = "Reference",
      desired_continents = "dummy continent",
      launch_ui = FALSE
    ),
    "The desired continent/region group dummy continent is not available for reporting."
  )
  expect_error(
    generate_report(
      db_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/"),
      db_name = "database_basexdb_ref",
      prj_name = "gcamv7.8_p1.dat",
      scenarios = "Reference",
      desired_continents = c("dummy1", "dummy2"),
      launch_ui = FALSE
    ),
    "The desired continent/region groups dummy1, dummy2 are not available for reporting."
  )

  expect_error(
    generate_report(
      db_name = "dummy_db_name",
      prj_name = "gcamv7.8_p1.dat",
      scenarios = "Reference",
      launch_ui = FALSE
    ),
    "The 'db_path' parameter is required to create a GCAM project but was not specified."
  )

  expect_error(
    generate_report(
      db_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/"),
      prj_name = "gcamv7.8_p1.dat",
      scenarios = "Reference",
      launch_ui = FALSE
    ),
    "The 'db_name' parameter is required to create a GCAM project but was not specified."
  )

  expect_error(
    generate_report(
      db_path = "dummy name",
      launch_ui = FALSE
    ),
    'argument "prj_name" is missing, with no default'
  )

  expect_error(
    generate_report(
      db_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/"),
      db_name = "database_basexdb_ref",
      prj_name = "gcamv7.8_p1.dat",
      scenarios = "Reference",
      desired_regions = "dummy region",
      desired_continents = "dummy continent",
      launch_ui = FALSE
    ),
    "You specified both 'desired_regions' and 'desired_continents'. Only one can be specified at a time."
  )

  expect_error(
    generate_report(
      prj_name = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test7.dat"),
      scenarios = "Reference3",
      desired_regions = "EU-12",
      launch_ui = FALSE
    ),
    "The desired scenario Reference3 is not present in the loaded project."
  )

  expect_error(
    generate_report(
      prj_name = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test7.dat"),
      scenarios = c("Reference3", "Reference4"),
      desired_regions = "EU-12",
      launch_ui = FALSE
    ),
    "The desired scenarios Reference3, Reference4 are not present in the loaded project."
  )

  expect_error(
    generate_report(
      db_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/"),
      db_name = "database_basexdb_ref",
      prj_name = "gcamv7.9_noCreated.dat",
      scenarios = "Reference",
      final_year = 2009,
      launch_ui = FALSE
    ),
    "'final_year' is set to '2009' but must be at least 2025. Please select a valid year: '2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060, 2065, 2070, 2075, 2080, 2085, 2090, 2095, 2100."
  )

  expect_error(
    generate_report(
      db_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/"),
      db_name = "database_basexdb_ref",
      prj_name = "gcamv7.9_noCreated.dat",
      scenarios = "Reference",
      final_year = 2031,
      launch_ui = FALSE
    ),
    "'final_year' is set to '2031' but must align with available 5-year intervals. Please select a valid year: '2025, 2030, 2035, 2040, 2045, 2050, 2055, 2060, 2065, 2070, 2075, 2080, 2085, 2090, 2095, 2100."
  )

})

test_that("Test9_v7. CO2 Price", {
  # World CO2 price
  generate_report(
    prj_name = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/database_basexdb_CO2price_test.dat"),
    desired_variables = c("Price|Carbon*"),
    launch_ui = FALSE
  )
  testResult <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/result_test9.1.RData")))
  testthat::expect_equal(report, testResult)
  rm(list = ls())

  # Regional CO2 price
  generate_report(
    prj_name = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/database_basexdb_policy.dat"),
    desired_variables = c("Price|Carbon*"),
    launch_ui = FALSE
  )
  testResult <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/result_test9.2.RData")))
  testthat::expect_equal(report, testResult)
})

test_that("Test10_v7. vetting", {
  generate_report(
    db_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0"),
    db_name = "database_basexdb_ref",
    prj_name = "gcamv7.10.1_test.dat",
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
    prj_name = "gcamv7.10.2_test.dat",
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
    prj_name = "gcamv7.10.3_test.dat",
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
      prj_name = "gcamv7.11.1_p1.dat",
      scenarios = c("dummy", "Reference"),
      launch_ui = FALSE
    ),
    "The desired scenario dummy is not present in the database."
  )

  expect_error(
    generate_report(
      db_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/"),
      db_name = "database_basexdb_ref",
      prj_name = "gcamv7.11.2_p1.dat",
      scenarios = c("dummy1", "dummy2", "Reference"),
      launch_ui = FALSE
    ),
    "The desired scenarios dummy1, dummy2 are not present in the database."
  )

  generate_report(
    db_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0"),
    db_name = "database_basexdb_ref",
    prj_name = "gcamv7.11.3_test_scenarios.dat",
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
      prj_name = "gcamv7.11.4_test_scenarios.dat",
      scenarios = c("dummy", "Reference"),
      launch_ui = FALSE
    ),
    "The desired scenario dummy is not present in the database"
  )

  expect_error(
    generate_report(
      db_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/"),
      db_name = "database_basexdb_ref",
      prj_name = "gcamv7.11.5_test_scenarios.dat",
      scenarios = c("dummy1", "dummy2", "Reference"),
      launch_ui = FALSE
    ),
    "The desired scenarios dummy1, dummy2 are not present in the database"
  )

  expect_error(
    generate_report(
      db_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/"),
      db_name = "database_basexdb_ref",
      prj_name = "gcamv7.11.5_test.dat",
      scenarios = c("dummy1", "dummy2", "Reference"),
      launch_ui = FALSE
    ),
    "The desired scenarios dummy1, dummy2 are not present in the database"
  )

  generate_report(
    db_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0"),
    db_name = "database_basexdb_ref",
    prj_name = "gcamv7.11.6_test_scenarios.dat",
    final_year = 2050,
    desired_regions = "All",
    desired_variables = c("Emissions|CH4*"),
    launch_ui = FALSE
  )

  testResult <- rgcam::listScenarios(prj)
  testthat::expect_equal("Reference", testResult)


  generate_report(
    db_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0"),
    db_name = "database_basexdb_ref",
    prj_name = "gcamv7.11.7_test_scenarios",
    final_year = 2050,
    desired_regions = "All",
    desired_variables = c("Emissions|CH4*"),
    launch_ui = FALSE
  )

  testResult <- rgcam::listScenarios(prj)
  testthat::expect_equal("Reference", testResult)

  generate_report(
    prj_name = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/database_basexdb_test_scenarios7.dat"),
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
  co2_sector_map <- read.csv(file.path(rprojroot::find_root(rprojroot::is_testthat), "inst/extdata/mappings/GCAM7.0", "CO2_sector_map.csv"),
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
  testResult <- transform_to_xml(gcamreport::queries_nonCO2_v7.0)
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
                  queries_general_file = file.path(rprojroot::find_root(rprojroot::is_testthat), "inst/extdata/queries/GCAM7.0/queries_gcamreport_general.xml"))
  testResult <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/gcamv7.0_test_specify_queries_standardized.RData")))
  testExpect <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/result_test13.2.RData")))
  testthat::expect_equal(testResult, testExpect)

})


test_that("Test14_v7. ghg GWP", {

  generate_report(
    prj_name = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test7.dat"),
    final_year = 2050,
    scenarios = "Reference",
    desired_variables = "Emissions*",
    launch_ui = FALSE,
    GWP_version = 'AR4'
  )
  testExpect <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/result_test14.1.RData")))
  testthat::expect_equal(report, testExpect)

  rm(list = ls())
  generate_report(
    prj_name = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test7.dat"),
    final_year = 2050,
    scenarios = "Reference",
    desired_variables = "Emissions*",
    launch_ui = FALSE,
    GWP_version = 'AR5'
  )
  testExpect <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/result_test14.2.RData")))
  testthat::expect_equal(report, testExpect)

  rm(list = ls())
  generate_report(
    prj_name = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test7.dat"),
    final_year = 2050,
    scenarios = "Reference",
    desired_variables = "Emissions*",
    launch_ui = FALSE,
    GWP_version = 'AR6'
  )
  testExpect <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/result_test14.3.RData")))
  testthat::expect_equal(report, testExpect)

  expect_error(
    generate_report(
      db_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/"),
      db_name = "database_basexdb_ref",
      prj_name = "gcamv7.8_noCreated.dat",
      scenarios = "Reference",
      desired_variables = c("dummy1", "dummy2"),
      launch_ui = FALSE,
      GWP_version = 4
    ),
    "GWP_version must be a character string, but you provided a value of type 'numeric'. Please specify the GWP_version as a string, e.g., GWP_version = 'AR5'."
  )

  expect_error(
    generate_report(
      db_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/"),
      db_name = "database_basexdb_ref",
      prj_name = "gcamv7.8_noCreated.dat",
      scenarios = "Reference",
      desired_variables = c("dummy1", "dummy2"),
      launch_ui = FALSE,
      GWP_version = '4'
    ),
    "Invalid GWP_version '4'. Available versions are: AR4, AR5, AR6. Please choose one of these versions."
  )

  expect_error(
    generate_report(
      db_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/"),
      db_name = "database_basexdb_ref",
      prj_name = "gcamv7.8_noCreated.dat",
      scenarios = "Reference",
      desired_variables = c("dummy1", "dummy2"),
      launch_ui = FALSE,
      GCAM_version = 4
    ),
    "GCAM_version must be a character string, but you provided a value of type 'numeric'. Please specify the GCAM_version as a string, e.g., GCAM_version = 'v7.0'."
  )

  expect_error(
    generate_report(
      db_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/"),
      db_name = "database_basexdb_ref",
      prj_name = "gcamv7.8_noCreated.dat",
      scenarios = "Reference",
      desired_variables = c("dummy1", "dummy2"),
      launch_ui = FALSE,
      GCAM_version = '4'
    ),
    "Invalid GCAM_version '4'. Available versions are: v6.0, v7.0, v7.1. Please choose one of these versions."
  )


})


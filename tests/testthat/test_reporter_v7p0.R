library(gcamreport)
library(testthat)
library(magrittr)


test_that("Test2_v7. load project", {
  testResult <- as.numeric(length(load_project(file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test7.dat"))))
  testthat::expect(!is.null(testResult), 'Null project. Check if the path exists or the "load_project" function works correctly.')
})


test_that("Test5_v7. run - dataset saved with default output_file", {
  generate_report(prj_name = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test7.dat"),
                  desired_variables = c('Agricultural Production*'),
                  launch_ui = FALSE, GCAM_version = "v7.0")

  testResult <- read.csv(file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test7_standardized.csv"))
  testthat::expect(dplyr::n_distinct(testResult) > 0, 'Dataset not saved. Check if the project path exists or the "run" function works correctly.')

  testResult <- readxl::read_excel(file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test7_standardized.xlsx"))
  testthat::expect(dplyr::n_distinct(testResult) > 0, 'Dataset not saved. Check if the project path exists or the "run" function works correctly.')
})


test_that("Test7_v7.0 specify variables, regions, continents", {
  test_regions <- available_regions(T, GCAM_version = 'v7.0')
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

  test_variables <- available_variables(T, GCAM_version = 'v7.0')
  testResult_variables <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/result_test7.1.RData")))
  testthat::expect_equal(test_variables, testResult_variables)

  rm(list = ls())
  generate_report(
    prj_name = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test7.dat"),
    scenarios = "Reference",
    final_year = 2050,
    desired_continents = "OECD90",
    desired_variables = available_variables(F, GCAM_version = 'v7.0')[c(28)],
    launch_ui = FALSE,
    GCAM_version = 'v7.0'
  )
  testResult_variables = unique(report$Variable)
  testthat::expect_equal(sort(unique(report$Variable)), 'Agricultural Production|Crops|Cereals')
  testthat::expect_equal(unique(report$Region), c(
    "Australia_NZ", "Canada", "EU-12", "EU-15",
    "Europe_Non_EU", "European Free Trade Association",
    "Japan", "USA", "World"
  ))

  rm(list = ls())
  generate_report(
    prj_name = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test7.dat"),
    scenarios = "Reference",
    final_year = 2050,
    desired_continents = "OECD90",
    desired_variables = "Emissions*",
    launch_ui = FALSE,
    GCAM_version = 'v7.0'
  )
  testResult_variables <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/result_test7.3.RData")))
  testthat::expect_equal(sort(unique(report$Variable)), sort(testResult_variables))
  testthat::expect_equal(unique(report$Model), 'GCAM 7.0')

  rm(list = ls())
  generate_report(
    prj_name = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test7.dat"),
    scenarios = "Reference",
    final_year = 2050,
    desired_regions = "USA",
    desired_variables = c("Price|Carbon"),
    launch_ui = FALSE,
    save_output = FALSE,
    GCAM_version = 'v7.0'
  )
  testthat::expect_equal(unique(report$Region), c("USA", "World"))

})

test_that("Test8_v7.0 error messages", {
  expect_error(
    generate_report(
      db_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/"),
      db_name = "database_basexdb_ref",
      prj_name = "gcamv7.8_noCreated.dat",
      scenarios = "Reference",
      desired_variables = "dummy variable",
      launch_ui = FALSE,
      GCAM_version = 'v7.0'
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
      launch_ui = FALSE,
      GCAM_version = 'v7.0'
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
      launch_ui = FALSE,
      GCAM_version = 'v7.0'
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
      launch_ui = FALSE,
      GCAM_version = 'v7.0'
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
      launch_ui = FALSE,
      GCAM_version = 'v7.0'
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
      launch_ui = FALSE,
      GCAM_version = 'v7.0'
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
      launch_ui = FALSE,
      GCAM_version = 'v7.0'
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
      launch_ui = FALSE,
      GCAM_version = 'v7.0'
    ),
    "The desired continent/region groups dummy1, dummy2 are not available for reporting."
  )

  expect_error(
    generate_report(
      db_name = "dummy_db_name",
      prj_name = "gcamv7.8_p1.dat",
      scenarios = "Reference",
      launch_ui = FALSE,
      GCAM_version = 'v7.0'
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
      launch_ui = FALSE,
      GCAM_version = 'v7.0'
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
      launch_ui = FALSE,
      GCAM_version = 'v7.0'
    ),
    "The desired scenario Reference3 is not present in the loaded project."
  )

  expect_error(
    generate_report(
      prj_name = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test7.dat"),
      scenarios = c("Reference3", "Reference4"),
      desired_regions = "EU-12",
      launch_ui = FALSE,
      GCAM_version = 'v7.0'
    ),
    "The desired scenarios Reference3, Reference4 are not present in the loaded project."
  )

  expect_error(
    generate_report(
      prj_name = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test7.dat"),
      desired_variables = 'GDP*',
      scenarios = "Reference",
      final_year = 2009,
      launch_ui = FALSE,
      GCAM_version = 'v7.0'
    ),
    "'final_year' is set to '2009' but must be at least 2025. Please select a valid year: 2025, 2030, 2035, 2040, 2045, 2050."
  )

  expect_error(
    generate_report(
      prj_name = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test7.dat"),
      desired_variables = 'GDP*',
      scenarios = "Reference",
      final_year = 2031,
      launch_ui = FALSE,
      GCAM_version = 'v7.0'
    ),
    "'final_year' is set to '2031' but must align with the available years in your project data. Please select a valid year: 2025, 2030, 2035, 2040, 2045, 2050."
  )

})

test_that("Test10v_7.0 vetting", {
  generate_report(
    prj_name = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test7.dat"),
    scenarios = "Reference",
    final_year = 2050,
    desired_regions = "All",
    desired_variables = c("Emissions|CH4*"),
    launch_ui = FALSE,
    GCAM_version = 'v7.0'
  )
  testResult <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/result_test10.1.RData")))
  testthat::expect_equal(vetting_summary, testResult)

  rm(list = ls())
  generate_report(
    prj_name = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test7.dat"),
    scenarios = "Reference",
    final_year = 2050,
    desired_regions = "All",
    desired_variables = c("Emissions|Sulfur*"),
    launch_ui = FALSE,
    GCAM_version = 'v7.0'
  )
  testResult <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/result_test10.2.RData")))
  testthat::expect_equal(vetting_summary, testResult)

  rm(list = ls())
  generate_report(
    prj_name = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test7.dat"),
    scenarios = "Reference",
    final_year = 2050,
    desired_regions = "South Africa",
    desired_variables = c("Final Energy*"),
    launch_ui = FALSE,
    GCAM_version = 'v7.0'
  )
  testthat::expect(!exists("vetting_summary$`Vetting variables`"), "Vetting performed when not all regions were selected")
})

test_that("Test11v_7.0 scenarios", {
  expect_error(
    generate_report(
      prj_name = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/database_basexdb_test_scenarios7.dat"),
      scenarios = c("dummy", "Reference"),
      launch_ui = FALSE,
      GCAM_version = 'v7.0'
    ),
    "The desired scenarios dummy, Reference are not present in the loaded project"
  )


})

test_that("Test12v_7.0 other functions", {
  # gather_map
  co2_tech_map <- read.csv(file.path(rprojroot::find_root(rprojroot::is_testthat), "inst/extdata/mappings/GCAM7.0", "CO2_tech_map.csv"),
                             skip = 1, na = "",
                             stringsAsFactors = FALSE
  ) %>% gather_map()
  testthat::expect_equal(data.frame(co2_tech_map), data.frame(gcamreport::co2_tech_map_v7.0))


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
    desired_variables = "Emissions|CO2|Energy|Demand|Industry|Iron and Steel",
    launch_ui = FALSE,
    GCAM_version = 'v7.0'
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
    launch_ui = FALSE,
    GCAM_version = 'v7.0'
  )
  testExpect <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/result_test12.3.RData")))
  testthat::expect_equal(report, testExpect)


  # get_regional_emission
  testResult <- get_regional_emission(GCAM_version = 'v7.0')
  testExpect <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/result_test12.4.RData")))
  testthat::expect_equal(data.frame(testResult), data.frame(testExpect))

})

test_that("Test14v_7.0 ghg GWP", {

  generate_report(
    prj_name = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test7.dat"),
    final_year = 2050,
    scenarios = "Reference",
    desired_variables = "Emissions*",
    launch_ui = FALSE,
    GWP_version = 'AR4',
    GCAM_version = 'v7.0'
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
    GWP_version = 'AR5',
    GCAM_version = 'v7.0'
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
    GWP_version = 'AR6',
    GCAM_version = 'v7.0'
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
      GWP_version = 4,
      GCAM_version = 'v7.0'
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
      GWP_version = '4',
      GCAM_version = 'v7.0'
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
    "Invalid GCAM_version '4'. Available versions are: v7.0, v7.1, v7.2, v8.2, vScenarioMIPCMIP7, vEurope7.2, vEurope8.7. Please choose one of these versions."
  )


})



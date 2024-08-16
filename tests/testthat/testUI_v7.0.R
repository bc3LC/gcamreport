library(gcamreport)
library(testthat)
library(magrittr)

test_that("Test1. test tree functions", {
  generate_report(prj_name = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test7.dat"), launch_ui = FALSE)
  # load data
  data <- report
  # define the dataset for launching the ui
  sdata <<- suppressWarnings(
    data %>%
      tidyr::separate(Variable, into = c("col1", "col2", "col3", "col4", "col5", "col6", "col7"), sep = "([\\|])", extra = "merge", remove = FALSE)
  )
  # create vector of available years for launching the ui
  available_years <<- as.numeric(names(sdata)[13:length(names(sdata))])
  # develop a nested list of the variables and regions for launching the ui
  cols.global <<- unique(sdata[, grepl("col", names(sdata))])
  tree_vars <<- do_mount_tree(cols.global, names(cols.global), selec = TRUE)
  tree_reg <<- do_mount_tree(reg_cont, names(reg_cont), selec = TRUE)
  # save a list of all variables
  all_varss <<- do_collapse_df(cols.global)

  # do_mount_tree with regions
  testResult1 <- do_mount_tree(reg_cont, names(reg_cont), selec = TRUE)
  testExpect1 <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/test_ui_1.RData")))
  testthat::expect_equal(testResult1, testExpect1)

  # do_unmount_tree with regions
  testResult1.2 <- do_unmount_tree(get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test_unmount_tree_regions.RData"))), "regions")
  testExpect1.2 <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/test_ui_1.2.RData")))
  testthat::expect_equal(testResult1.2, testExpect1.2)

  # do_mount_tree with variables
  testResult2 <- do_mount_tree(cols.global, names(cols.global), selec = TRUE)
  testExpect2 <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/test_ui_2.RData")))
  testthat::expect_equal(testResult2, testExpect2)

  # do_unmount_tree with variables
  testResult2.2 <- do_unmount_tree(get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test_unmount_tree_variables.RData"))), "variables")
  testExpect2.2 <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/test_ui_2.2.RData")))
  testthat::expect_equal(testResult2.2, testExpect2.2)

  # do_unmount_tree NULL
  testResult2.3 <- do_unmount_tree(NULL, "variables")
  testthat::expect_equal(testResult2.3, NULL)

  # do_collapse_df
  testResult3 <- do_collapse_df(cols.global)
  testExpect3 <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/test_ui_3.RData")))
  testthat::expect_equal(testResult3, testExpect3)

  # change_style
  test4_pre <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/test_ui_4_pre.RData")))
  testExpect4 <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/test_ui_4_post.RData")))
  testResult4 <- change_style(test4_pre, "regions")
  testthat::expect_equal(testResult4, testExpect4)

  # check_user_choices_plot
  testResult5 <- check_user_choices_plot(c("var1", "var2"), c("scen1", "scen2"), c("year1", "year2"), NULL, grouped = TRUE)
  testExpect5 <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/test_ui_5.RData")))
  testthat::expect_equal(testResult5, testExpect5)

  testResult6 <- check_user_choices_plot(c("var1", "var2"), c("scen1", "scen2"), c("year1", "year2"), NULL, grouped = FALSE)
  testExpect6 <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/test_ui_6.RData")))
  testthat::expect_equal(testResult6, testExpect6)

  testResult7 <- check_user_choices_plot(c("var1"), c("scen1", "scen2"), c("year1", "year2"), "reg1", grouped = FALSE)
  testExpect7 <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/test_ui_7.RData")))
  testthat::expect_equal(testResult7, testExpect7)

  # compute_height
  testResult8 <- compute_height(c(reg_cont[, 2], "World"))
  testExpect8 <- 960
  testthat::expect_equal(testResult8, testExpect8)

  testResult9 <- compute_height(c("MAF", "LAM"))
  testExpect9 <- 325
  testthat::expect_equal(testResult9, testExpect9)
})

test_that("Test2. error messages", {
  expect_error(
    launch_gcamreport_ui(),
    "Error: Neither 'data_path' nor 'data' has been provided. Please specify at least one of these: 'data_path' to point to the location of the dataset file or 'data' to provide the dataset directly."
  )

  expect_error(
    launch_gcamreport_ui("dummy1", "dummy2"),
    "Error: Both 'data_path' and 'data' have been provided. Please specify only one: either 'data_path' to point to the dataset file or 'data' to provide the dataset directly. Providing both is not allowed."
  )

  errorMessage1 <- check_user_choices_plot(
    vars = "var1", scen = c("scen1", "scen2"),
    years = NULL, reg = "Africa", grouped = TRUE
  )
  errorExpect1 <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/test_ui_error1.RData")))
  testthat::expect_equal(errorMessage1, errorExpect1)

  errorMessage2 <- check_user_choices_plot(
    vars = "var1", scen = NULL,
    years = "year", reg = "Africa", grouped = TRUE
  )
  errorExpect2 <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/test_ui_error2.RData")))
  testthat::expect_equal(errorMessage2, errorExpect2)

  errorMessage3 <- check_user_choices_plot(
    vars = NULL, scen = "scen1",
    years = "year", reg = "Africa", grouped = TRUE
  )
  errorExpect3 <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/test_ui_error3.RData")))
  testthat::expect_equal(errorMessage3, errorExpect3)
})

test_that("Test3. reset", {
  sdata <<- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test_ui_3.RData")))
  reset_first_load()

  testthat::expect_equal(firstLoad, TRUE)
  testthat::expect_equal(firstReg, TRUE)
  testthat::expect_equal(firstVars, TRUE)
  testthat::expect_equal(noReg, FALSE)
  testthat::expect_equal(noVars, FALSE)
  testthat::expect_equal(updatedVars, FALSE)
  testthat::expect_equal(
    tree_reg,
    get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/test_ui_3.a.RData")))
  )
  testthat::expect_equal(
    cols.global,
    get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/test_ui_3.b.RData")))
  )
  testthat::expect_equal(
    all_varss,
    get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/test_ui_3.c.RData")))
  )
})

test_that("Test5. do_data_sample", {

  # do_data_sample
  sdata <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test_ui_5.sdata.RData")))
  sel_cols <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test_ui_5.sel_cols.RData")))
  sel_reg <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test_ui_5.sel_reg.RData")))
  sel_scen <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test_ui_5.sel_scen.RData")))
  sel_vars <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test_ui_5.sel_vars.RData")))
  sel_years <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test_ui_5.sel_years.RData")))
  testResult <- do_data_sample(sdata, sel_scen, sel_years, sel_cols, sel_vars, sel_reg, 0, 0)
  testExpect <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/test_ui_5.1.RData")))
  testthat::expect_equal(testResult, testExpect)

  testResult <- do_data_sample(sdata, sel_scen, sel_years, sel_cols, sel_vars, sel_reg, 1, 0)
  testExpect <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/test_ui_5.2.RData")))
  testthat::expect_equal(testResult, testExpect)

  testResult <- do_data_sample(sdata, sel_scen, sel_years, sel_cols, sel_vars, sel_reg, 1, 1)
  testExpect <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/test_ui_5.3.RData")))
  testthat::expect_equal(testResult, testExpect)

  testResult <- do_data_sample(sdata, sel_scen, sel_years, sel_cols, sel_vars, sel_reg, 2, 1)
  testExpect <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/test_ui_5.4.RData")))
  testthat::expect_equal(testResult, testExpect)

  testResult <- do_data_sample(sdata, sel_scen, sel_years, sel_cols, sel_vars, sel_reg, 1, 2)
  testExpect <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/test_ui_5.5.RData")))
  testthat::expect_equal(testResult, testExpect)
})


test_that("Test6. update_user_choices_plot", {

  # update_user_choices_plot
  firstReg <- firstVars <- T
  noReg <- noVars <- F
  selected_scen <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test_ui_6.selected_scen.RData")))
  selected_years <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test_ui_6.selected_years.RData")))
  sidebarItemExpanded <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test_ui_6.sidebarItemExpanded.RData")))
  tree_regions <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test_ui_6.tree_regions.RData")))
  tree_variables <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test_ui_6.tree_variables.RData")))
  testResult <- update_user_choices_plot(selected_scen, selected_years, tree_regions, tree_variables, sidebarItemExpanded)
  testExpect <- get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/test_ui_6.1.RData")))
  testthat::expect_equal(testResult, testExpect)

})


#
# test_that("Test4. launch ui", {
#   launch_gcamreport_ui(data_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test_launch_ui.RData"))
#
#   testthat::expect_equal(available_years, c(2005, 2010, 2015, 2020, 2025, 2030, 2035, 2040, 2045, 2050))
#   testthat::expect_equal(sdata,
#                          get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/test_ui_4.a.RData"))))
#   testthat::expect_equal(cols.global,
#                          get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/test_ui_4.b.RData"))))
#   testthat::expect_equal(tree_vars,
#                          get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/test_ui_4.c.RData"))))
#   testthat::expect_equal(tree_reg,
#                          get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/test_ui_4.d.RData"))))
#   testthat::expect_equal(all_varss,
#                          get(load(file.path(rprojroot::find_root(rprojroot::is_testthat), "testOutputs/v_7.0/test_ui_4.e.RData"))))
#
# })

library(gcamreport); library(testthat); library(magrittr); library(rprojroot)

test_that("Test1. test tree functions", {
  run(project_path = paste0(rprojroot::find_root(rprojroot::is_testthat),'/testInputs/test6.dat'), launch_app = FALSE)
  # do_mount_tree with regions
  testResult1 = do_mount_tree(reg_cont,names(reg_cont),selec=TRUE)
  testExpect1 = get(load(paste0(rprojroot::find_root(rprojroot::is_testthat),'/testOutputs/tree_reg_test.RData')))
  testthat::expect_equal(testResult1, testExpect1)

  # do_mount_tree with variables
  testResult2 = do_mount_tree(cols,names(cols),selec=TRUE)
  testExpect2 = get(load(paste0(rprojroot::find_root(rprojroot::is_testthat),'/testOutputs/tree_vars_test.RData')))
  testthat::expect_equal(testResult2, testExpect2)

  # do_collapse_df
  testResult3 = do_collapse_df(cols)
  testExpect3 = get(load(paste0(rprojroot::find_root(rprojroot::is_testthat),'/testOutputs/all_vars_test.RData')))
  testthat::expect_equal(testResult3, testExpect3)

  # change_style
  test4_pre = get(load(paste0(rprojroot::find_root(rprojroot::is_testthat),'/testOutputs/pre_tree_regions.RData')))
  testExpect4 = get(load(paste0(rprojroot::find_root(rprojroot::is_testthat),'/testOutputs/post_tree_regions.RData')))
  testResult4 = change_style(test4_pre, 'regions')
  testthat::expect_equal(testResult4, testExpect4)

  # check_user_choices_plot
  testResult5 = check_user_choices_plot(c('var1','var2'), c('scen1','scen2'), c('year1','year2'), NULL, grouped = TRUE)
  testExpect5 = get(load(paste0(rprojroot::find_root(rprojroot::is_testthat),'/testOutputs/message5.RData')))
  testthat::expect_equal(testResult5, testExpect5)

  testResult6 = check_user_choices_plot(c('var1','var2'), c('scen1','scen2'), c('year1','year2'), NULL, grouped = FALSE)
  testExpect6 = get(load(paste0(rprojroot::find_root(rprojroot::is_testthat),'/testOutputs/message6.RData')))
  testthat::expect_equal(testResult6, testExpect6)

  testResult7 = check_user_choices_plot(c('var1'), c('scen1','scen2'), c('year1','year2'), 'reg1', grouped = FALSE)
  testExpect7 = get(load(paste0(rprojroot::find_root(rprojroot::is_testthat),'/testOutputs/message7.RData')))
  testthat::expect_equal(testResult7, testExpect7)
})

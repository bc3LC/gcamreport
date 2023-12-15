library(gcamreport); library(testthat); library(magrittr); library(rprojroot)

test_that("Test1. test tree functions", {

  generate_report(project_path = file.path(rprojroot::find_root(rprojroot::is_testthat),'testInputs/v_7.0/test7.dat'), launch_ui = FALSE)
  # load data
  data <- report
  # define the dataset for launching the ui
  sdata <<- suppressWarnings(
    data %>%
      tidyr::separate(Variable, into = c('col1','col2','col3','col4','col5','col6','col7'), sep = "([\\|])", extra = 'merge', remove = FALSE)
    )
  # create vector of available years for launching the ui
  available_years <<- as.numeric(names(sdata)[13:length(names(sdata))])
  # develop a nested list of the variables and regions for launching the ui
  cols <<- unique(sdata[, grepl('col', names(sdata))])
  tree_vars <<- do_mount_tree(cols,names(cols),selec=TRUE)
  tree_reg <<- do_mount_tree(reg_cont,names(reg_cont),selec=TRUE)
  # save a list of all variables
  all_varss <<- do_collapse_df(cols)


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

  # compute_height
  testResult8 = compute_height(c(reg_cont[,2],'World'))
  testExpect8 = 960
  testthat::expect_equal(testResult8, testExpect8)

  testResult9 = compute_height(c('MAF','LAM'))
  testExpect9 = 325
  testthat::expect_equal(testResult9, testExpect9)
})

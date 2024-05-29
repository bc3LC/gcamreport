
prj_name = 'C:/Users/claudia.rodes/Documents/IAM_COMPACT/gcamreport/examples/database_basexdb_2100_prj_study1_rep.dat'
generate_report(prj_name=prj_name,launch_ui=FALSE,
                desired_variables = c('Price|Agriculture*'))


db_path = NULL; db_name = NULL; prj_name; scenarios = NULL; final_year = 2100
desired_variables = "All"; desired_regions = "All"; desired_continents = "All"
save_output = TRUE; output_file = NULL; launch_ui = TRUE
queries_general_file = gcamreport::queries_general
queries_nonCO2_file = gcamreport::queries_nonCO2

db_path = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0")
db_name = "database_basexdb_ref"
prj_name = "debug2.dat"
scenarios = "Reference"
final_year = 2050
desired_regions = "USA"
desired_variables = "Price|Carbon"
launch_ui = FALSE


prj_name <- file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/database_basexdb_ref/gcamv7.0_test.dat")

prj_name = file.path(rprojroot::find_root(rprojroot::is_testthat), "testInputs/v_7.0/test7.dat")
scenarios = "Reference3"
desired_regions = "EU-12"
launch_ui = FALSE

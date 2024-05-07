# rename template items
db_path = NULL
db_name = NULL
# prj_name = 'database_basexdb_2100_prj_study1_rep.dat'
prj_name = 'database_basexdb_2100_prj_renamevar3.dat'
scenarios = NULL
final_year = 2100
desired_variables = "All"
desired_regions = "All"
desired_continents = "All"
save_output = TRUE
output_file = NULL
launch_ui = TRUE
queries_general_file = gcamreport::queries_general
queries_nonCO2_file = gcamreport::queries_nonCO2
verbose = FALSE

generate_report(
  prj_name = 'database_basexdb_2100_prj_renamevar3.dat',
  desired_regions = "China"
)




prjname = 'C:/Users/claudia.rodes/Documents/gcam-v7.0-Windows-Release-Package/output/database_basexdb_policy_debug1.dat'
generate_report(prj_name=prjname,launch_ui=FALSE,
                desired_variables = c('Price|Carbon'))




##############################################################################
generate_report(db_path = "examples",
                db_name = 'database_basexdb_ref',
                scenarios = 'Reference',
                prj_name = 'dev2.dat',
                final_year = 2050,
                desired_variables = c('Price|Carbon*'),
                save_output = TRUE,
                launch_ui = F)
db_path = NULL
db_name = NULL
prj_name
scenarios = NULL
final_year = 2100
desired_variables = "All"
desired_regions = "All"
desired_continents = "All"
save_output = TRUE
output_file = NULL
launch_ui = TRUE
queries_general_file = gcamreport::queries_general
queries_nonCO2_file = gcamreport::queries_nonCO2


## -- load gcamreport library.
devtools::load_all()

## -- store the project path and name in a variable.
datapath <- "examples/example3.RData"

## -- launch the user interface
launch_gcamreport_ui(data_path = datapath)






## -- generate the reporting dataset until 2050 for EU-12 and EU-15 for all the
## -- Agricultural variables, save the output in .RData, .csv and .xlsx format,
## -- and lunch the user interface
generate_report(db_path = dbpath, db_name = dbname, scenarios = scen,
                prj_name = prjname, final_year = 2050,
                desired_regions = c('EU-12', 'EU-15', 'Canada', 'Brazil', 'USA'),
                desired_variables = c('Final*'),
                save_output = TRUE, launch_ui = TRUE)




## -- load gcamreport library.
devtools::load_all()
# library(gcamreport)

## -- store the database path, name, and scenarios in a variable.
dbpath <- "examples"
dbname <- "database_basexdb_ref"
scen <- "Reference"

## -- choose a project name
prjname <- "example1.dat"

## -- generate the reporting dataset until 2050 for EU-12 and EU-15 for all the
## -- Agricultural variables, save the output in .RData, .csv and .xlsx format,
## -- and lunch the user interface
generate_report(db_path = dbpath, db_name = dbname, scenarios = scen,
                prj_name = prjname, final_year = 2050,
                desired_regions = c('EU-12', 'EU-15'),
                desired_variables = c('Agricultural*'),
                save_output = TRUE, launch_ui = TRUE)




launch_gcamreport_ui("examples/example3.RData")

datapath <- "examples/example3.RData"

## -- launch the user interface
launch_gcamreport_ui(data_path = datapath)


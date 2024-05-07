
################################################################################
## -- example 1 - generate project

rm(list = ls())
devtools::load_all()

## -- store the database path and name
dbpath <- "C:/Users/claudia.rodes/Documents/gcam-v7.0-Windows-Release-Package/output"
dbname <- "database_basexdb_ssp"

## -- choose a project name
prjname <- "example1.dat"

## -- generate the reporting dataset
generate_report(db_path = dbpath, db_name = dbname, prj_name = prjname,
                final_year = 2050,
                scenarios = c('GCAM_SSP4', 'GCAM_SSP5', 'GCAM_SSP1'),
                desired_regions = c('USA', 'Brazil', 'EU-15'),
                desired_variables = c('Agricultural*'))


available_regions()
available_variables()




################################################################################
## -- example 2 - vetting

rm(list = ls())
devtools::load_all()

## -- choose the project name
prjname <- "examples/prj_example_vetting.dat"

## -- generate the reporting dataset
generate_report(prj_name = prjname, final_year = 2050)




################################################################################
## -- example 3 - launch UI


## A) if "report" variable is in the environment
launch_gcamreport_ui(data = report)


## B) from an RData dataset
rm(list = ls())
devtools::load_all()

## -- set the Rproj path
RDataName <- "examples/prj_example_vetting_standardized.RData"

## -- generate the reporting dataset
launch_gcamreport_ui(data_path = RDataName)




################################################################################
## -- example 4 - saving options - launching UI options - error messages

rm(list = ls())
devtools::load_all()

## -- set the project name
prjname <- "examples/example2.dat"

## -- generate the reporting dataset
generate_report(prj_name = prjname,
                final_year = 2050,
                desired_regions = c('Argentina'),
                desired_variables = c('Agricultral*'),
                save_output = FALSE,
                launch_ui = FALSE)



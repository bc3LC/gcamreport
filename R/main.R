## Main functions of the package
library(usethis)
library(magrittr)

#' load_project
#'
#' Load specified project into the global environment
#' @param prj_name: name of the project
#' @return loaded project into global environment
#' @export
load_project = function(prj_name) {
  # load the project
  prj <<- rgcam::loadProject(prj_name)

  Scenarios <<- rgcam::listScenarios(prj)
}


#' create_project
#'
#' Create specified project and load it into the global environment
#' @param db_path: path of the database
#' @param query_path: path of the query
#' @param db_name: name of the database
#' @param prj_name: name of the project
#' @param scenarios: name of the scenarios to be considered
#' @return loaded project into global environment
#' @export
create_project = function(db_path, query_path, db_name, prj_name, scenarios) {
  # create the project
  conn <- rgcam::localDBConn(db_path,
                             db_name,migabble = FALSE)
  prj <- rgcam::addScenario(conn,
                            prj_name,
                            scenarios,
                            paste0(query_path,"/",queries))

  # load the project
  print('Loading project...')
  prj <<- rgcam::loadProject(prj)

  Scenarios <<- rgcam::listScenarios(prj)
}


#' load_variable
#'
#' Recursive function to load the desired variable and its dependent variables
#' @param var: variable to be loaded
#' @keywords internal
#' @return load variable
#' @export
load_variable = function(var){

  # base case: if variable already loaded, return
  if (exists(var$name)) {
    return()
  }

  # if the variable has dependencies, load them
  if (!is.na(var$dependencies)) {
    for (d in var$dependencies[[1]]) {
      load_variable(variables[which(variables$name == d),])
    }
  }

  # print the variable's name
  print(var$name)

  # load the variable
  get(var$fun)()
}


#' run
#'
#' Main function. Interacts with the user to select the desired variables for the report, loads
#' them, saves them in an external output, runs the verifications, and informs the user about the
#' success of the whole process. Either the `project_path` should be specified, or the `db_path`
#' with all the related items, being `query_path`, `db_name`, `prj_name`, and `scenarios`.
#' @param project_path: full path of the project with the project name. Possible extensions: .dat and .proj.
#' @param db_path: full path of the database.
#' @param query_path: full path of the query.
#' @param db_name: name of the database.
#' @param prj_name: name of the project.
#' @param scenarios: name of the scenarios to be considered.
#' @param final_year: final year of the data. By default = 2100.
#' @param desired_variables: desired variables to have in the report. Considered 'All' by default.
#' Otherwise, specify a vector with all the desired options, being population_clean, GDP_MER_clean, GDP_PPP_clean,
#' global_temp_clean, forcing_clean, co2_concentration_clean, co2_emissions_clean, tot_co2_clean, co2_sequestration_clean,
#' ag_demand_clean, land_clean, primary_energy_clean, energy_trade_clean, elec_gen_tech_clean, elec_capacity_tot_clean,
#' elec_capacity_add_clean, se_gen_tech_clean, fe_sector_clean, energy_service_transportation_clean, energy_service_buildings_clean,
#' ag_prices_clean, industry_production_clean, elec_capital_clean, elec_investment_clean, transmission_invest_clean,
#' CCS_invest_clean, resource_investment_clean, nonco2_clean, co2_price_clean.
#' @param save_output: if TRUE, save reporting data in CSV and XLSX formats. If FALSE, do not save data. If equals 'CSV' or 'XLSX',
#' data saved only in the specified format.
#' @param file_name: file path and name of the saved data. Not used if data not saved. By default, saved in the same directory and with
#' the same name than the specified project_path, with 'ipcc_report' tag. CSV and XLSX output. In case of specifiing the path, do not
#' introduce the extension, it will be automatically added.
#' @param launch_ui: if TRUE, launch UI, Do not launch UI otherwise.
#' @return saved? CSV and XLSX datafile with the desired variables & launched? user interface.
#' @export
run = function(project_path = NULL, db_path = NULL, query_path = NULL, db_name = NULL, prj_name = NULL, scenarios = NULL,
               final_year = 2100, desired_variables = 'All', save_output = TRUE, file_name = NULL, launch_ui = TRUE) {

  # check that the paths are correctly specified
  if (!is.null(project_path) && (!is.null(db_path) || !is.null(query_path) || !is.null(db_name) || !is.null(prj_name) || !is.null(scenarios))) {
    # stop and display error
    stop('ERROR: Specify either a project or a database to extract the data from. Not both.')

  } else if (!is.null(project_path)) {
    # load project
    print('Loading project...')
    load_project(project_path)

  } else if (!is.null(db_path) || !is.null(query_path) || !is.null(db_name) || !is.null(prj_name) || !is.null(scenarios)) {
    # create project if checks ok
    print('Creating project...')

    # check that all the paths are specified
    if (is.null(db_path) || is.null(query_path) || is.null(db_name) || is.null(prj_name) || is.null(scenarios)) {
      null_items = c()
      not_null_items = c()
      for (item in c('db_path','query_path','db_name','prj_name','scenarios')) {
        if (is.null(eval(parse(text=item)))) {
          null_items = c(null_items, item)
        } else {
          not_null_items = c(not_null_items, item)
        }
      }

      # stop and display error
      if (length(not_null_items) > 1) {
        stop("If ", paste(not_null_items, collapse = ', '), " are specified, ", paste(null_items, collapse = ', '), " must also be specified.")
      } else {
        stop("If ", paste(not_null_items, collapse = ', '), " is specified, ", paste(null_items, collapse = ', '), " must also be specified.")
      }
    } else {
      # create project
      create_project(db_path, query_path, db_name, prj_name, scenarios)
    }

  } else {
    # stop and display error
    stop('ERROR: Specify either a project or a database to extract the data from.')
  }

  # make final_db_year as a global variable
  final_db_year <<- final_year

  # final reporting columns:
  reporting_columns_fin <<- append(c("Model", "Scenario", "Region", "Variable", "Unit"), as.character(seq(2005, final_db_year, by = 5)))

  # desired variables to have in the report
  variables_base <<- data.frame('name' =
                                  c('population_clean', 'GDP_MER_clean', 'GDP_PPP_clean',
                                    'global_temp_clean', 'forcing_clean', 'co2_concentration_clean',
                                    'co2_emissions_clean', 'tot_co2_clean', 'co2_sequestration_clean',
                                    'ag_demand_clean', 'land_clean',
                                    'primary_energy_clean', 'energy_trade_clean',
                                    'elec_gen_tech_clean', 'elec_capacity_tot_clean', 'elec_capacity_add_clean',
                                    'se_gen_tech_clean', 'fe_sector_clean',
                                    'energy_service_transportation_clean',
                                    'energy_service_buildings_clean',
                                    'ag_prices_clean', 'industry_production_clean',
                                    'elec_capital_clean',
                                    'elec_investment_clean', 'transmission_invest_clean', 'CCS_invest_clean', 'resource_investment_clean',
                                    'nonco2_clean',
                                    'co2_price_clean'),
                                'required' = TRUE,
                                stringsAsFactors = FALSE)

  # consider only the desired variables
  if (desired_variables == 'All') {
    variables <<- variables_base
  } else {
    variables <<- dplyr::anti_join(variables_base, desired_variables, by = 'name')
  }

  print('Loading data, performing checks, and saving output...')

  # consider the dependencies and checking functions
  variables <<- merge(variables,var_fun_map, by = 'name', all = TRUE) %>%
    tidyr::replace_na(list(required = FALSE))

  # for all desired variables, load the corresponding data
  for (i in 1:nrow(variables)) {
    if (variables$required[i]) {
      load_variable(variables[i,])
    }
  }

  # set the default file_name based on the project_path or the db_path & db_name
  if (is.null(file_name)) {
    if (!is.null(project_path)) {
      file_name = gsub("\\.dat$", "", project_path)
      file_name = paste0(file_name,'_ipcc_report')
    } else {
      file_name = gsub("\\.dat$", "", project_path)
      file_name = paste0(db_path, "/", db_name, '_ipcc_report')
    }
  }

  # bind and save results
  do_bind_results()
  if (save_output == TRUE || save_output %in% c('CSV','XLSX')) {
    if (!dir.exists(paste0(here::here(), "/output/datasets/"))){
      if (!dir.exists(paste0(here::here(), "/output/"))){
        dir.create(paste0(here::here(), "/output/"))
      }
      dir.create(paste0(here::here(), "/output/datasets/"))
    }
    if (save_output == TRUE || 'CSV' %in% save_output) {
      write.csv(final_data, file.path(paste0(file_name,'.csv')), row.names = FALSE)
    }
    if (save_output == TRUE || 'XLSX' %in% save_output) {
      writexl::write_xlsx(final_data, file.path(paste0(file_name,'.xlsx')))
    }
  }

  # checks, vetting, and errors summary
  errors <<- c()

  for (ch in variables$checks) {
    if (!is.na(ch)) {
      for (d in ch[[1]]) {
        out = get(variables$fun[which(variables$name == d)])()
        errors <<- append(errors,out)
      }
    }
  }
  vet = do_check_vetting()
  print('The following checks have been performed:')
  errors <<- append(errors,vet)
  for (e in errors) {
    print(e)
  }

  # define the dataset for launching the ui
  sdata <<- final_data %>%
    tidyr::separate(Variable, into = c('col1','col2','col3','col4','col5','col6','col7'), sep = "([\\|])", extra = 'merge', remove = FALSE)

  # create vector of available years for launching the ui
  available_years <<- as.numeric(names(sdata)[13:length(names(sdata))])

  # develop a nested list of the variables and regions for launching the ui
  cols <<- unique(sdata[, grepl('col', names(sdata))])
  tree_vars <<- do_mount_tree(cols,names(cols),selec=TRUE)

  tree_reg <<- do_mount_tree(reg_cont,names(reg_cont),selec=TRUE)

  # save a list of all variables
  all_vars <<- do_collapse_df(cols)

  if (launch_ui) {
    print('Launching UI...')

    # launch ui
    launch_gcamreport_ui()
  }

}


#' launch_gcamreport_ui
#'
#' Launch shiny interactive ui
#' @return launch shiny interactive ui
#' @export
launch_gcamreport_ui <- function(){
  shiny::runApp('inst/gcamreport_ui')
}

## Main functions of the package
## TODO: 'create project' function
library(usethis)
library(magrittr)

#' load_project
#'
#' Load specified project into the global environment
#' @param prj_name: name of the project
#' @return loaded project into global environment
#' @export
load_project = function(prj_name) {
  # Load data, once the project file has been created (no need to create it again!!)
  prj <<- rgcam::loadProject(paste0(iamc_dir, "/", prj_name, ".dat"))


  Scenarios <<- rgcam::listScenarios(prj)
  rgcam::listQueries(prj)
}


#' load_variable
#'
#' Recursive function to load desired variable and its dependent variables
#' @param var: variable to be loaded
#' @keywords internal
#' @return load variable
#' @export
load_variable = function(var){
  if (exists(var$name)) {
    return()
  }

  if (!is.na(var$dependencies)) {
    for (d in var$dependencies[[1]]) {
      load_variable(variables[which(variables$name == d),])
    }
  }
  print(var$name)
  get(var$fun)()
}


#' read_queries
#'
#' Main function. Interacts with the user to select the desired variables for the report,
#' loads them, saves them in an external output, runs the verifications, and informs the
#' user about the success of the whole process.
#' @param project_name: name of the project. By default = gas_fin_updated.
#' @param final_db_year: final year of the database. By default = 2100.
#' @param desired_variables: desired variables to have in the report.
#' @importFrom magrittr %>%
#' @keywords internal
#' @return load variable
#' @export
read_queries = function(project_name = 'gas_fin_updated', final_db_year = 2100, desired_variables = 'All', save = TRUE, file_name = 'final_data', launch_app = TRUE) {
  # load project
  print('Creating project...')
  load_project(project_name)

  # make final_db_year as a global variable
  final_db_year <<- final_db_year

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
                                'required' = TRUE)

  # consider only the desired variables
  if (desired_variables == 'All') {
    variables <<- variables_base
  } else {
    variables <<- dplyr::anti_join(variables_base, desired_variables, by = 'name')
  }

  removed = setdiff(variables_base$name, variables$name)
  if (length(removed) > 0) {
    print('The following variables have been removed:')
    for (r in removed) {
      print(r)
    }
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

  # bind and save results
  do_bind_results()
  if (save) {
    write.csv(final_data, file.path(out_dir, paste0(file_name,".csv")), row.names = FALSE)
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

  sdata <<- final_data %>%
    tidyr::separate(Variable, into = c('col1','col2','col3','col4','col5','col6','col7'), sep = "([\\|])", extra = 'merge', remove = FALSE)

  # create vector of available years
  available_years <<- as.numeric(names(sdata)[13:length(names(sdata))])

  # develop a nested list for the variables
  cols <<- unique(sdata[, grepl('col', names(sdata))])
  # tree_vars <<- do_mount_tree(cols,names(cols),selec=TRUE)
  cols <<- do_codes(cols)

  # reg_cont <<- read.csv(paste0(map_dir, "/regions_continents_map.csv"), header = TRUE, sep = ",", encoding = "UTF-8")
  reg_cont <<- read.csv(paste0(map_dir, "/regions_continents_map.csv"), skip = 1)
  # tree_reg <<- do_mount_tree(reg_cont,names(reg_cont),selec=TRUE)
  reg_cont <<- do_codes(reg_cont)

  if (launch_app) {
    print('Launching app...')
    runExample()
  }

}

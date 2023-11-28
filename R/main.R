## Main functions of the package
library(usethis)
library(magrittr)

#' data_query
#'
#' Add nonCO2 large queries
#' @param db_path: path of the database
#' @param db_name: name of the database
#' @param prj_name: name of the project
#' @param scenarios: name of the scenarios to be considered
#' @param type: either 'nonCO2 emissions by region' or 'nonCO2 emissions by sector'
#' @param desired_regions: desired regions to consider. By default, 'All' (written as NULL). Otherwise, specify a vector
#' with all the considered regions, being USA,Africa_Eastern,Africa_Northern,Africa_Southern,Africa_Western,Australia_NZ,Brazil,Canada,
#' Central Asia,China,EU-12,EU-15,Europe_Eastern,Europe_Non_EU,European Free Trade Association,India,Indonesia,Japan,
#' Mexico,Middle East,Pakistan,Russia,South Africa,South America_Northern,South America_Southern,South Asia,South Korea,
#' Southeast Asia,Taiwan,Argentina,Colombia,Central America and Caribbean. ATTENCION: the considered regions will make up "World".
#' In case the project dataset needs to be created, it will be produced with only the specified regions.
#' @return dataframe with the data from the query
#' @export
data_query = function(type, db_path, db_name, prj_name, scenarios, desired_regions = 'All') {
  if (length(desired_regions) == 1 && desired_regions == 'All') {
    desired_regions = NULL
  }

  dt = data.frame()
  xml <- xml2::read_xml('inst/extdata/queries/queries_gcamreport_gcam7.0_nonCO2.xml')
  qq <- xml2::xml_find_first(xml, paste0("//*[@title='", type, "']"))

  for (sc in scenarios) {
    emiss_list = emissions_list
    while (length(emiss_list) > 0) {
      current_emis = emiss_list[1:min(21,length(emiss_list))]
      qq_sec = gsub("current_emis", paste0("(@name = '", paste(current_emis, collapse = "' or @name = '"), "')"), qq)

      prj_tmp = rgcam::addSingleQuery(
        conn = rgcam::localDBConn(db_path,
                                  db_name,migabble = FALSE),
        proj = prj_name,
        qn = type,
        query = qq_sec,
        scenario = sc,
        regions = desired_regions,
        clobber = TRUE,
        transformations = NULL,
        saveProj = FALSE,
        warn.empty = FALSE
      )

      tmp = data.frame(prj_tmp[[sc]][type])
      if (nrow(tmp) > 0) {
        dt = dplyr::bind_rows(dt,tmp)
      }
      rm(prj_tmp)

      if (length(emiss_list) > 21) {
        emiss_list <- emiss_list[(21 + 1):length(emiss_list)]
      } else {
        emiss_list = c()
      }
    }
  }
  # Rename columns
  new_colnames <- sub(".*\\.(.*)", "\\1", names(dt))
  names(dt) <- new_colnames

  return(dt)
}


#' fill_queries
#'
#' Create a folder to save the datasets and file, in case it does not exist
#' @param db_path: path of the database
#' @param db_name: name of the database
#' @param prj_name: name of the project
#' @param scenarios: name of the scenarios to be considered
#' @param desired_regions: desired regions to consider. By default, 'All'. Otherwise, specify a vector
#' with all the considered regions, being USA,Africa_Eastern,Africa_Northern,Africa_Southern,Africa_Western,Australia_NZ,Brazil,Canada,
#' Central Asia,China,EU-12,EU-15,Europe_Eastern,Europe_Non_EU,European Free Trade Association,India,Indonesia,Japan,
#' Mexico,Middle East,Pakistan,Russia,South Africa,South America_Northern,South America_Southern,South Asia,South Korea,
#' Southeast Asia,Taiwan,Argentina,Colombia,Central America and Caribbean. ATTENCION: the considered regions will make up "World".
#' In case the project dataset needs to be created, it will be produced with only the specified regions.
#' @return empty dataframes on the void queries of the project
#' @export
fill_queries = function(db_path, db_name, prj_name, scenarios, desired_regions = 'All') {
  # add nonCO2 queries manually (they are too big to use the usual method)
  if (!'nonCO2 emissions by sector (excluding resource production)' %in% rgcam::listQueries(prj)) {
    print('nonCO2 emissions by sector (excluding resource production)')
    dt_sec = data_query('nonCO2 emissions by sector (excluding resource production)', db_path, db_name, prj_name, scenarios, desired_regions)
    prj_tmp <- rgcam::addQueryTable(project = prj_name, qdata = dt_sec,
                                    queryname = 'nonCO2 emissions by sector (excluding resource production)', clobber = FALSE)
    prj <<- rgcam::mergeProjects(prj_name, list(prj,prj_tmp), clobber = TRUE, saveProj = FALSE)

  }
  if (!'nonCO2 emissions by region' %in% rgcam::listQueries(prj)) {
    print('nonCO2 emissions by region')
    dt_reg = data_query('nonCO2 emissions by region', db_path, db_name, prj_name, scenarios, desired_regions)
    prj_tmp <- rgcam::addQueryTable(project = prj_name, qdata = dt_reg,
                                    queryname = 'nonCO2 emissions by region', clobber = FALSE)
    prj <<- rgcam::mergeProjects(prj_name, list(prj,prj_tmp), clobber = TRUE, saveProj = FALSE)
  }

  # fix CO2 prices if needed
  if (!'CO2 prices' %in% rgcam::listQueries(prj)) {
    l = length(rgcam::listScenarios(prj))
    dt = data.frame(Units = rep(NA,l),
                    scenario = rgcam::listScenarios(prj),
                    year = rep(NA,l),
                    market = rep(NA,l),
                    value = rep(NA,l))
    prj_tmp <- rgcam::addQueryTable(project = prj_name, qdata = dt,
                                    queryname = 'CO2 prices', clobber = TRUE)
    prj <<- rgcam::mergeProjects(prj_name, list(prj,prj_tmp), clobber = TRUE, saveProj = FALSE)
  }
}


#' load_project
#'
#' Load specified project into the global environment
#' @param prj_name: name of the project
#' @param desired_regions: desired regions to consider. By default, 'All'. Otherwise, specify a vector with all the considered regions,
#' being USA,Africa_Eastern,Africa_Northern,Africa_Southern,Africa_Western,Australia_NZ,Brazil,Canada,
#' Central Asia,China,EU-12,EU-15,Europe_Eastern,Europe_Non_EU,European Free Trade Association,India,Indonesia,Japan,
#' Mexico,Middle East,Pakistan,Russia,South Africa,South America_Northern,South America_Southern,South Asia,South Korea,
#' Southeast Asia,Taiwan,Argentina,Colombia,Central America and Caribbean. ATTENCION: the considered regions will make up "World".
#' In case the project dataset needs to be created, it will be produced with only the specified regions.
#' @return loaded project into global environment
#' @export
load_project = function(prj_name, desired_regions = 'All') {
  # load the project
  prj <<- rgcam::loadProject(prj_name)

  # filter the regions if not all of them are considered (desired_regions != 'All')
  if (!(length(desired_regions) == 1 && desired_regions == 'All')) {
    # for all scenarios in prj
    for (s in names(prj)) {
      # for all variables in prj
      for (v in names(prj[[s]])) {
        print(v)
        prj[[s]][[v]] = filter_regions(prj[[s]][[v]], desired_regions, v)
      }
    }
  }

  Scenarios <<- rgcam::listScenarios(prj)
}


#' create_project
#'
#' Create specified project and load it into the global environment
#' @param db_path: path of the database
#' @param db_name: name of the database
#' @param prj_name: name of the project
#' @param scenarios: name of the scenarios to be considered
#' @param desired_regions: desired regions to consider. By default, 'All'. Otherwise, specify a vector with all the considered regions,
#' being USA,Africa_Eastern,Africa_Northern,Africa_Southern,Africa_Western,Australia_NZ,Brazil,Canada,
#' Central Asia,China,EU-12,EU-15,Europe_Eastern,Europe_Non_EU,European Free Trade Association,India,Indonesia,Japan,
#' Mexico,Middle East,Pakistan,Russia,South Africa,South America_Northern,South America_Southern,South Asia,South Korea,
#' Southeast Asia,Taiwan,Argentina,Colombia,Central America and Caribbean. ATTENCION: the considered regions will make up "World".
#' In case the project dataset needs to be created, it will be produced with only the specified regions.
#' @return loaded project into global environment
#' @export
create_project = function(db_path, db_name, prj_name, scenarios, desired_regions = 'All') {
  # create the project
  conn <- rgcam::localDBConn(db_path,
                             db_name,migabble = FALSE)

  # read the queries file
  queryFile = paste0('inst/extdata/queries/','queries_gcamreport_gcam7.0_complete.xml')
  queries <- rgcam::parse_batch_query(queryFile)

  # load all queries for all desired scenarios informing the user
  for (sc in scenarios) {
    print(paste('Start reading queries for',sc,'scenario'))

    for(qn in names(queries)) {
      print(paste('Read', qn, 'query'))

      bq <- queries[[qn]]

      # subset regions if necessary
      if (!(length(desired_regions) == 1 && desired_regions == 'All')) {
        bq$regions = desired_regions
      }

      table <- rgcam::runQuery(conn, bq$query, sc, bq$regions, warn.empty = FALSE)
      if(nrow(table) > 0) {
        prj_tmp <- rgcam::addQueryTable(project = prj_name, qdata = table,
                                        queryname = qn, clobber = FALSE,
                                        saveProj = FALSE, show_col_types = FALSE)
        if (exists('prj')) {
          prj = rgcam::mergeProjects(prj_name, list(prj,prj_tmp), clobber = FALSE, saveProj = FALSE)
        } else {
          prj = prj_tmp
        }

      } else {
        warning(paste(qn, 'query is empty!'))
      }
    }
  }
  prj <<- prj

  # fill with empty datatable the possible 'CO2 price' query and add 'nonCO2' large queries
  fill_queries(db_path, db_name, prj_name, scenarios, desired_regions)

  # save the project
  rgcam::saveProject(prj, file = paste0(db_path, "/", db_name, '_', prj_name))

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
#' with all the related items, being `db_name`, `prj_name`, and `scenarios`.
#' @param project_path: full path of the project with the project name. Possible extensions: .dat and .proj.
#' @param db_path: full path of the database.
#' @param db_name: name of the database.
#' @param prj_name: name of the project.
#' @param scenarios: name of the scenarios to be considered.
#' @param final_year: final year of the data. By default = 2100. ATENTION: final_year must be at least 2025.
#' @param desired_variables: desired variables to have in the report. Considered 'All' by default.
#' Otherwise, specify a vector with all the desired options, being population_clean, GDP_MER_clean, GDP_PPP_clean,
#' global_temp_clean, forcing_clean, co2_concentration_clean, co2_emissions_clean, tot_co2_clean, co2_sequestration_clean,
#' ag_demand_clean, land_clean, primary_energy_clean, energy_trade_clean, elec_gen_tech_clean, elec_capacity_tot_clean,
#' elec_capacity_add_clean, se_gen_tech_clean, fe_sector_clean, energy_service_transportation_clean, energy_service_buildings_clean,
#' ag_prices_clean, industry_production_clean, elec_capital_clean, elec_investment_clean, transmission_invest_clean,
#' CCS_invest_clean, resource_investment_clean, nonco2_clean, co2_price_clean.
#' @param desired_regions: desired regions to consider. By default, 'All'. Otherwise, specify a vector with all the considered regions,
#' being USA,Africa_Eastern,Africa_Northern,Africa_Southern,Africa_Western,Australia_NZ,Brazil,Canada,
#' Central Asia,China,EU-12,EU-15,Europe_Eastern,Europe_Non_EU,European Free Trade Association,India,Indonesia,Japan,
#' Mexico,Middle East,Pakistan,Russia,South Africa,South America_Northern,South America_Southern,South Asia,South Korea,
#' Southeast Asia,Taiwan,Argentina,Colombia,Central America and Caribbean. ATTENCION: the considered regions will make up "World".
#' In case the project dataset needs to be created, it will be produced with only the specified regions.
#' @param save_output: if TRUE, save reporting data in CSV and XLSX formats. If FALSE, do not save data. If equals 'CSV' or 'XLSX',
#' data saved only in the specified format.
#' @param file_name: file path and name of the saved data. Not used if data not saved. By default, saved in the same directory and with
#' the same name than the specified project_path, with 'iamc_report' tag. CSV and XLSX output. In case of specifing the path, do not
#' introduce the extension, it will be automatically added.
#' @param launch_ui: if TRUE, launch UI, Do not launch UI otherwise.
#' @return saved? CSV and XLSX datafile with the desired variables & launched? user interface.
#' @export
run = function(project_path = NULL, db_path = NULL, db_name = NULL, prj_name = NULL, scenarios = NULL, final_year = 2100,
               desired_variables = 'All', desired_regions = 'All', save_output = TRUE, file_name = NULL, launch_ui = TRUE) {

  # check that the desired_regions do exist in GCAM7 IAM COMPACT
  if (!(length(desired_regions) == 1 && desired_regions == 'All')) {
    check_reg = setdiff(desired_regions, reg_cont$region)
    if (length(check_reg) > 0) {
      stop(paste0('ERROR: You specified regions ',check_reg, ' which are not present in the GCAM 7 IAM-COMPACT configuration.'))
    }
    # desired_regions special case: if some "EU" region is present, consider the
    # "EU" region to compute CO2 prices
    if (!(length(desired_regions) == 1 && desired_regions == 'All')) {
      desired_regions = c(desired_regions, 'EU')
    }
  }

  # check that the paths are correctly specified
  if (!is.null(project_path) && (!is.null(db_path) || !is.null(db_name) || !is.null(prj_name) || !is.null(scenarios))) {
    # stop and display error
    stop('ERROR: Specify either a project or a database to extract the data from. Not both.')

  } else if (!is.null(project_path)) {
    # load project
    print('Loading project...')
    load_project(project_path, desired_regions)

  } else if (!is.null(db_path) || !is.null(db_name) || !is.null(prj_name) || !is.null(scenarios)) {
    # create project if checks ok
    print('Creating project...')

    # check that all the paths are specified
    if (is.null(db_path) || is.null(db_name) || is.null(prj_name) || is.null(scenarios)) {
      null_items = c()
      not_null_items = c()
      for (item in c('db_path','db_name','prj_name','scenarios')) {
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
      create_project(db_path, db_name, prj_name, scenarios, desired_regions)
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
                                  c('co2_price_clean', 'population_clean', 'GDP_MER_clean', 'GDP_PPP_clean',
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
                                    'nonco2_clean'),
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
  desired_regions <<- desired_regions
  for (i in 1:nrow(variables)) {
    if (variables$required[i]) {
      load_variable(variables[i,])
    }
  }

  # set the default file_name based on the project_path or the db_path & db_name
  if (is.null(file_name)) {
    if (!is.null(project_path)) {
      file_name = gsub("\\.dat$", "", project_path)
      file_name = paste0(file_name,'_iamc_report')
    } else {
      file_name = paste0(db_path, "/", gsub("\\.dat$", "", prj_name), '_iamc_report')
    }
  }

  # bind and save results
  do_bind_results()
  save(final_data, file = paste0(file_name,'.RData'))

  if (save_output == TRUE || save_output %in% c('CSV','XLSX')) {
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

#' data_query
#'
#' Add nonCO2 large queries
#' @param db_path path of the database.
#' @param db_name name of the database.
#' @param prj_name name of the project.
#' @param scenarios name of the scenarios to be considered.
#' @param type either 'nonCO2 emissions by region' or 'nonCO2 emissions by sector'.
#' @param desired_regions desired regions to consider. By default, 'All'. Otherwise, specify a vector with all the considered regions.
#' To know all possible regions, run `available_regions()`. ATTENTION: the considered regions will make up "World".
#' In case the project dataset needs to be created, it will be produced with only the specified regions.
#' @param queries_nonCO2_file full path to an xml query file (including file name and extension) designed to load long nonCO2 queries:
#' "nonCO2 emissions by sector (excluding resource production)" and "nonCO2 emissions by region". By default it points to the
#' gcamreport nonCO2 query file, compatible with the latest GCAM version and necessary to report some of the standardized variables.
#' @return dataframe with the data from the query.
#' @importFrom rgcam addSingleQuery localDBConn
#' @importFrom xml2 read_xml xml_find_first
#' @importFrom dplyr bind_rows
#' @export
data_query <- function(type, db_path, db_name, prj_name, scenarios, desired_regions = "All",
                       queries_nonCO2_file = gcamreport::queries_nonCO2) {
  if (identical(desired_regions, "All")) {
    desired_regions <- NULL
  }

  dt <- data.frame()
  if (is.list(queries_nonCO2_file)) {
    xml <- transform_to_xml(queries_nonCO2_file)
  } else {
    xml <- read_xml(queries_nonCO2_file)
  }
  qq <- xml_find_first(xml, paste0("//*[@title='", type, "']"))

  for (sc in scenarios) {
    emiss_list <- gcamreport::emissions_list
    while (length(emiss_list) > 0) {
      current_emis <- emiss_list[1:min(21, length(emiss_list))]
      qq_sec <- gsub("current_emis", paste0("(@name = '", paste(current_emis, collapse = "' or @name = '"), "')"), qq)

      prj_tmp <- addSingleQuery(
        conn = localDBConn(db_path,
                           db_name,
                           migabble = FALSE
        ),
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

      tmp <- data.frame(prj_tmp[[sc]][type])
      if (nrow(tmp) > 0) {
        dt <- bind_rows(dt, tmp)
      }
      rm(prj_tmp)

      if (length(emiss_list) > 21) {
        emiss_list <- emiss_list[(21 + 1):length(emiss_list)]
      } else {
        emiss_list <- c()
      }
    }
  }
  # Rename columns
  new_colnames <- sub(".*\\.(.*)", "\\1", names(dt))
  names(dt) <- new_colnames

  return(dt)
}


#' load_project
#'
#' Load specified project into the global environment
#' @param project_path path of the project (including project name and extension).
#' @param desired_regions desired regions to consider. By default, 'All'. Otherwise, specify a vector with all the considered regions.
#' To know all possible regions, run `available_regions()`. ATTENTION: the considered regions will make up "World".
#' In case the project dataset needs to be created, it will be produced with only the specified regions.
#' @param scenarios name of the scenarios to be considered. By default, all the scenarios in the project are considered.
#' @return loaded project into global environment.
#' @importFrom rgcam addSingleQuery localDBConn listScenarios loadProject dropScenarios
#' @importFrom dplyr intersect setdiff if_else
#' @export
load_project <- function(project_path, desired_regions = "All", scenarios = NULL) {
  # rm variable "prj" from the environment if exists
  if (exists("prj")) rm(prj, envir = .GlobalEnv)

  rlang::inform("Loading project...")

  # load the project
  prj <- loadProject(project_path)

  # filter the regions if not all of them are considered (desired_regions != 'All')
  if (!(identical(desired_regions, "All"))) {
    # for all scenarios in prj
    for (s in names(prj)) {
      # for all variables in prj
      for (v in names(prj[[s]])) {
        prj[[s]][[v]] <- filter_loading_regions(prj[[s]][[v]], desired_regions, v)
      }
    }
  }
  if (is.null(scenarios)) {
    scenarios.global <<- listScenarios(prj)
  } else {
    scenarios.global <<- intersect(scenarios, listScenarios(prj))
    # check the user input
    if (length(scenarios) > length(scenarios.global)) {
      check_scen <- setdiff(scenarios, listScenarios(prj))
      tmp <- paste(check_scen, collapse = ", ")
      if (length(check_scen) > 1) stop("The desired scenarios ", tmp, " are not present in the loaded project.\n")
      if (length(check_scen) == 1) stop("The desired scenario ", tmp, " is not present in the loaded project.\n")
    }
    # drop unnecessary scenarios
    for (i in listScenarios(prj)[!listScenarios(prj) %in% scenarios]) {
      prj <- dropScenarios(prj, i)
    }
  }

  prj <<- prj
}


#' create_project
#'
#' Create specified project and load it into the global environment
#' @param db_path path of the database.
#' @param db_name name of the database.
#' @param prj_name name of the project.
#' @param scenarios name of the scenarios to be considered. By default, all the scenarios in the database are considered.
#' @param desired_regions desired regions to consider. By default, 'All'. Otherwise, specify a vector with all the considered regions.
#' To know all possible regions, run `available_regions()`. ATTENTION: the considered regions will make up "World".
#' In case the project dataset needs to be created, it will be produced with only the specified regions.
#' @param desired_variables desired variables to have in the report. Considered 'All' by default.
#' Otherwise, specify a vector with all the desired options. To know all possible options, run `available_variables()`.
#' In case the project dataset needs to be created, it will be produced with only the specified variables. ATTENTION:
#' the global variables such as "Emissions" will be computed considering only the selected variables, for instance "Emissions|CO2",
#' and will no account for other variables, such as "Emissions|CH4" or "Emissions|NH3".
#' @param queries_general_file full path to a general xml query file (including file name and extension). By default it points to the
#' gcamreport general query file, compatible with the latest GCAM version and able to report all standardized variables.
#' @param queries_nonCO2_file full path to an xml query file (including file name and extension) designed to load long nonCO2 queries:
#' "nonCO2 emissions by sector (excluding resource production)" and "nonCO2 emissions by region". By default it points to the
#' gcamreport nonCO2 query file, compatible with the latest GCAM version and necessary to report some of the standardized variables.
#' @return loaded project into global environment and local saved project.
#' @import rgcam
#' @import dplyr
#' @export
create_project <- function(db_path, db_name, prj_name, scenarios = NULL,
                           desired_regions = "All", desired_variables = "All",
                           queries_general_file = gcamreport::queries_general,
                           queries_nonCO2_file = gcamreport::queries_nonCO2) {
  Internal_variable <- Variable <- required <- available_scenarios <- name <- NULL

  # rm variable "prj" from the environment if exists
  if (exists("prj")) rm(prj, envir = .GlobalEnv)

  rlang::inform("Project does not exists. Creating project...")

  # create the project
  conn <- localDBConn(db_path,
                      db_name,
                      migabble = FALSE
  )

  available_scenarios <- listScenariosInDB(conn) %>%
    pull(name)
  # check user input
  if (is.null(scenarios)) {
    scenarios <- available_scenarios
  } else {
    check_scen <- setdiff(scenarios, available_scenarios)
    tmp <- paste(check_scen, collapse = ", ")
    if (length(check_scen) > 1) stop("The desired scenarios ", tmp, " are not present in the database.\n")
    if (length(check_scen) == 1) stop("The desired scenario ", tmp, " is not present in the database.\n")
  }

  # read the query file
  if (is.list(queries_general_file)) {
    queries_short <- queries_general_file
  } else {
    queries_short <- parse_batch_query(queries_general_file)
  }
  if (is.list(queries_nonCO2_file)) {
    queries_large <- queries_nonCO2_file
  } else {
    queries_large <- parse_batch_query(queries_nonCO2_file)
  }

  # subset the queries necessary for the selected variables
  if (!(length(desired_variables) == 1 && desired_variables == "All")) {
    # create a mapping with the Variables, Internal variables, functions to load
    # them, and the dependencies
    required_internal_variables <- gcamreport::var_fun_map %>%
      rename("Internal_variable" = "name") %>%
      left_join(
        gcamreport::template %>%
          filter(!is.na(Internal_variable)),
        by = "Internal_variable",
        multiple = "all"
      ) %>%
      mutate(required = if_else(Variable %in% desired_variables, TRUE, FALSE))

    # create a vector with the required queries for the desired variables
    required_queries <- c()
    for (req_int_var in unique(required_internal_variables %>%
                               filter(required == TRUE) %>%
                               pull(Variable))) {
      required_queries <- c(
        required_queries,
        load_query(
          required_internal_variables[which(required_internal_variables$Variable == req_int_var), ],
          required_internal_variables,
          c()
        )
      )
    }
    required_queries <- unique(required_queries[!is.na(required_queries)])

    # save the read-to-use queries in a vector
    queries_touse_short <- queries_short[names(queries_short) %in% required_queries]
    queries_touse_large <- queries_large[names(queries_large) %in% required_queries]
  } else {
    # save the read-to-use queries in a vector. These are all the possible queries
    queries_touse_short <- queries_short
    queries_touse_large <- queries_large
  }

  # load all queries for all desired scenarios informing the user
  for (sc in scenarios) {
    rlang::inform(paste("Start reading queries for", sc, "scenario"))

    for (qn in names(queries_touse_short)) {
      rlang::inform(paste("Read", qn, "query"))

      bq <- queries_touse_short[[qn]]

      # subset regions if necessary
      if (!(identical(desired_regions, "All"))) {
        bq$regions <- desired_regions
      }

      table <- suppressMessages({
        runQuery(conn, bq$query, sc, bq$regions, warn.empty = FALSE)
      })
      if (nrow(table) > 0) {
        prj_tmp <- addQueryTable(
          project = prj_name, qdata = table,
          queryname = qn, clobber = FALSE,
          saveProj = FALSE, show_col_types = FALSE
        )
        if (exists("prj")) {
          prj <- mergeProjects(prj_name, list(prj, prj_tmp), clobber = FALSE, saveProj = FALSE)
        } else {
          prj <- prj_tmp
        }
        rm(prj_tmp)
      } else {
        warning(paste(qn, "query is empty!"))
      }
    }
  }

  # Add 'nonCO2' large queries manually (they are too big to use the usual method)
  if (!"nonCO2 emissions by sector (excluding resource production)" %in% listQueries(prj) &&
      "nonCO2 emissions by sector (excluding resource production)" %in% names(queries_touse_large)) {
    rlang::inform("nonCO2 emissions by sector (excluding resource production)")

    # rm variable "prj_tmp" from the environment if exists
    if (exists("prj_tmp")) rm(prj_tmp)

    dt_sec <- data_query("nonCO2 emissions by sector (excluding resource production)", db_path, db_name, prj_name, scenarios, desired_regions)
    prj_tmp <- addQueryTable(
      project = prj_name, qdata = dt_sec, saveProj = FALSE,
      queryname = "nonCO2 emissions by sector (excluding resource production)", clobber = FALSE
    )
    prj <- mergeProjects(prj_name, list(prj, prj_tmp), clobber = FALSE, saveProj = FALSE)
    rm(prj_tmp)
  }
  if (!"nonCO2 emissions by region" %in% listQueries(prj) &&
      "nonCO2 emissions by region" %in% names(queries_touse_large)) {
    rlang::inform("nonCO2 emissions by region")

    # rm variable "prj_tmp" from the environment if exists
    if (exists("prj_tmp")) rm(prj_tmp)

    dt_reg <- data_query("nonCO2 emissions by region", db_path, db_name, prj_name, scenarios, desired_regions)
    prj_tmp <- addQueryTable(
      project = prj_name, qdata = dt_reg, saveProj = FALSE,
      queryname = "nonCO2 emissions by region", clobber = FALSE
    )
    prj <- mergeProjects(prj_name, list(prj, prj_tmp), clobber = FALSE, saveProj = FALSE)
    rm(prj_tmp)
  }

  # Fill with an empty datatable the possible 'CO2 price' query if necessary
  if (!"CO2 prices" %in% listQueries(prj) &&
      "CO2 prices" %in% names(queries_touse_short)) {
    # rm variable "prj_tmp" from the environment if exists
    if (exists("prj_tmp")) rm(prj_tmp)

    l <- length(listScenarios(prj))
    dt <- data.frame(
      Units = rep(NA, l),
      scenario = listScenarios(prj),
      year = rep(NA, l),
      market = rep(NA, l),
      value = rep(NA, l)
    )
    prj_tmp <- addQueryTable(
      project = prj_name, qdata = dt,
      queryname = "CO2 prices", clobber = TRUE
    )
    prj <- mergeProjects(prj_name, list(prj, prj_tmp), clobber = TRUE, saveProj = FALSE)
    rm(prj_tmp)
  }

  # save the project
  saveProject(prj, file = file.path(db_path, paste(db_name, prj_name, sep = "_")))


  scenarios.global <<- listScenarios(prj)

  prj <<- prj
}


#' load_variable
#'
#' Recursive function to load the desired variable and its dependent variables
#' @param var variable to be loaded.
#' @param verbose prints the variable being loaded. FALSE by default. Helps when debugging.
#' @keywords internal
#' @return load variable.
#' @export
load_variable <- function(var, verbose = F) {
  # base case: if variable already loaded, return
  if (exists(var$name)) {
    return()
  }

  # if the variable has dependencies, load them
  if (!is.na(var$dependencies)) {
    for (d in var$dependencies[[1]]) {
      load_variable(variables.global[which(variables.global$name == d), ], verbose)
    }
  }

  # print the variable's name
  if (verbose) print(var$name)

  # load the variable
  get(var$fun)()

  # keep record of the loaded variables
  loaded_internal_variables.global <<- c(loaded_internal_variables.global, var$name)
}


#' load_query
#'
#' Recursive function to load the necessary queries for the desired variables
#' @param var variable to be loaded.
#' @param base_data dataframe with the required internal variables.
#' @param final_queries vector of the queries to be loaded.
#' @keywords internal
#' @return query name to be loaded.
#' @export
load_query <- function(var, base_data, final_queries) {
  # if the variable has dependencies, load them
  if (!is.na(var$dependencies)) {
    for (d in var$dependencies[[1]]) {
      tmp <- load_query(
        base_data[which(base_data$Internal_variable == d), ][1, ],
        base_data,
        final_queries
      )
      final_queries <- c(final_queries, tmp)
    }
  }

  # record the required query
  final_queries <- c(final_queries, var$queries[[1]])
  return(final_queries)
}


#' available_regions
#'
#' @param print if TRUE, prints all available regions. TRUE by default.
#' @return Prints a list of all the available regions for the IAMC reporting dataset.
#' It also returns them as a vector.
#' @importFrom dplyr mutate if_else
#' @export
available_regions <- function(print = TRUE) {
  continent <- region <- NULL

  av_reg <- gcamreport::reg_cont %>%
    mutate(region = if_else(continent == "World", "World", region))

  if (print) {
    for (it in unique(av_reg$region)) {
      print(it)
    }
  }

  return(invisible(unique(av_reg$region)))
}


#' available_continents
#'
#' @param print if TRUE, prints all available continents/regions' groups. TRUE by default.
#' @return Prints a list of all the available continents/regions' groups for the
#' IAMC reporting dataset. It also returns them as a vector.
#' @export
available_continents <- function(print = TRUE) {
  continent <- region <- NULL

  av_cont <- unique(gcamreport::reg_cont$continent)

  if (print) {
    for (it in av_cont) {
      print(it)
    }
  }

  return(invisible(av_cont))
}


#' available_variables
#'
#' @param print if TRUE, prints all available variables. TRUE by default.
#' @return Prints a list of all the available variables for the IAMC reporting dataset.
#' It also returns them as a vector.
#' @importFrom dplyr filter
#' @export
available_variables <- function(print = TRUE) {
  Internal_variable <- NULL

  av_var <- gcamreport::template %>%
    filter(!is.na(Internal_variable) & Internal_variable != "")

  if (print) {
    for (it in unique(av_var$Variable)) {
      print(it)
    }
  }

  return(invisible(unique(av_var$Variable)))
}



#' generate_report
#'
#' Main function. Creates/loads a GCAM project, standardizes the data and saves it
#' in several forms (RData, CSV, and XLSX), runs vetting verifications, and launches
#' the User Interface. You can specify the regions and/or variables to be reported and point
#' either the `db_path`, the `db_name`, the `prj_name`, and `scenarios` to create a new project
#' and produce the standardized report; or the `prj_name` and `scenarios` to produce the report
#' of an existing project.
#' The resulting RData output can be used to manually call `launch_gcamreport_ui`.
#' @param db_path full path of the GCAM database.
#' @param db_name name of the GCAM database.
#' @param prj_name name of the rgcam project. This can be an existing project name, in which case the project will be loaded, or a new project name,
#' in which case a new project will be created.. Possible extensions: .dat and .proj.
#' @param scenarios name of the scenarios to be considered. By default, all the scenarios in the project or the database are considered.
#' @param final_year final year of the data. By default = 2100. ATENTION: final_year must be at least 2025.
#' @param desired_variables desired variables to have in the report. Considered 'All' by default.
#' Otherwise, specify a vector with all the desired options. To know all possible options, run `available_variables()`.
#' In case the project dataset needs to be created, it will be created containing only the specified variables.
#' ATTENTION: global variables such as "Emissions" will be computed considering only the selected variables. As an example,
#' if you select "Emissions" and "Emissions|CO2", "Emissions" will only account for "Emissions|CO2", and will not
#' consider other variables, such as "Emissions|CH4" or "Emissions|NH3"
#' @param desired_regions desired regions to consider. By default, 'All'. Otherwise, specify a vector with all the considered regions.
#' To know all possible regions, run `available_regions()`. ATTENTION: the considered regions will make up "World".
#' In case the project dataset needs to be created, it will be produced with only the specified regions.
#' @param desired_continents desired continents/regions' groups to consider. By default, 'All'. Otherwise, specify a vector with all
#' the considered continents/regions' groups. To know all possibilities, run `available_continents()`. ATTENTION: the considered
#' continents/regions' groups will make up "World". In case the project dataset needs to be created, it will be produced with only the
#' specified continents/regions' groups.
#' @param save_output if TRUE, save reporting data in CSV and XLSX formats. If FALSE, do not save data. If 'save_output = CSV' or
#' 'save_output = XLSX', the data will be only saved in the specified format.
#' @param output_file file path and name of the saved data. Not used if data not saved. By default, saved in the same directory of the
#' database or the project file, and using a default name containing the project name with 'standardized' tag.
#' In case of specifying the `output_file`, introduce a whole path (e.g. /path/to/output/fileName) without extension tag, it will be automatically added.
#' @param launch_ui if TRUE, launch User Interface, Do not launch it otherwise.
#' @param queries_general_file full path to a general xml query file (including file name and extension). By default it points to the
#' gcamreport general query file, compatible with the latest GCAM version and able to report all standardized variables.
#' @param queries_nonCO2_file full path to an xml query file (including file name and extension) designed to load long nonCO2 queries:
#' "nonCO2 emissions by sector (excluding resource production)" and "nonCO2 emissions by region". By default it points to the
#' gcamreport nonCO2 query file, compatible with the latest GCAM version and necessary to report some of the standardized variables.
#' @param verbose prints extra messages that help when debugging. FALSE by default.
#' @return RData, CSV, and XLSX saved datafiles with the desired standardized variables, launches user interface, and save the rgcam
#' project file (if created).
#' @import dplyr
#' @importFrom tidyr replace_na
#' @importFrom utils write.csv
#' @importFrom writexl write_xlsx
#' @importFrom stringr str_sub str_locate
#' @export
generate_report <- function(db_path = NULL, db_name = NULL, prj_name, scenarios = NULL, final_year = 2100,
                            desired_variables = "All", desired_regions = "All", desired_continents = "All",
                            save_output = TRUE, output_file = NULL, launch_ui = TRUE,
                            queries_general_file = gcamreport::queries_general,
                            queries_nonCO2_file = gcamreport::queries_nonCO2,
                            verbose = FALSE) {
  continent <- region <- name <- Variable <- Internal_variable <- required <- prj_loaded <- NULL

  # boolean variable
  prj_loaded <- FALSE

  # check that desired_regions and desired_continents are not specified at the same time
  if (!(identical(desired_regions, "All"))) {
    if (!(identical(desired_continents, "All"))) {
      stop("You specified both the desired_regions and the desired_continents parameters. Only one can be specified at a time.\n")
    }
  }

  # check that the desired_regions are available
  if (!(identical(desired_regions, "All"))) {
    check_reg <- setdiff(desired_regions, available_regions(print = FALSE))
    if (length(check_reg) > 0) {
      tmp <- paste(check_reg, collapse = ", ")
      if (length(check_reg) > 1) stop(paste0("The desired regions ", tmp, " are not available for reporting.\n"))
      if (length(check_reg) == 1) stop(paste0("The desired region ", tmp, " is not available for reporting.\n"))
    }
  }
  # check that the desired_continents are available
  if (!(length(desired_continents) == 1 && desired_continents == "All")) {
    check_cont <- setdiff(desired_continents, available_continents(print = FALSE))
    if (length(check_cont) > 0) {
      tmp <- paste(check_cont, collapse = ", ")
      if (length(check_cont) > 1) stop(paste0("The desired continent/regions' groups ", tmp, " are not available for reporting.\n"))
      if (length(check_cont) == 1) stop(paste0("The desired continent/regions' group ", tmp, " is not available for reporting.\n"))
    }
    desired_regions <- gcamreport::reg_cont %>%
      filter(continent %in% desired_continents) %>%
      pull(region)
  }
  # check that the desired_variables are available
  if (!(length(desired_variables) == 1 && desired_variables == "All")) {
    original_desired_variables <- desired_variables

    # consider the * symbol
    contains_star <- grepl("\\*", desired_variables)
    if (sum(contains_star) > 0) {
      contains_star <- desired_variables[contains_star]
      avail_variables <- available_variables(F)

      no_pattern <- c()
      for (elem in contains_star) {
        pattern <- sub("\\*.*", "", elem)
        tmp <- c(desired_variables, start_with_pattern(avail_variables, pattern))
        # check user input
        if (length(desired_variables) == length(tmp)) no_pattern <- c(no_pattern, elem)
        desired_variables <- tmp
      }
      if (length(no_pattern) > 1) stop(paste0("There are no variables containing the patterns ", paste(no_pattern, collapse = ", "), " available for reporting.\n"))
      if (length(no_pattern) == 1) stop(paste0("There is no variable containing the pattern ", no_pattern, " available for reporting.\n"))

      # remove elements containing '*'
      contains_star <- grepl("\\*", desired_variables)
      desired_variables <- setdiff(desired_variables, desired_variables[contains_star])
    }

    # check the user input
    check_var <- setdiff(desired_variables, available_variables(print = FALSE))
    if (length(check_var) > 0) {
      tmp <- paste(check_var, collapse = ", ")
      if (length(check_var) > 1) stop(paste0("The variables ", tmp, " are not available for reporting.\n"))
      if (length(check_var) == 1) stop(paste0("The variable ", tmp, " is not available for reporting.\n"))
    }
    if (length(desired_variables) == 0) {
      tmp <- paste(original_desired_variables, collapse = ", ")
      if (length(original_desired_variables) > 1) stop(paste0("The variables ", tmp, " are not available for reporting.\n"))
      if (length(original_desired_variables) == 1) stop(paste0("The variable ", tmp, " is not available for reporting.\n"))
    }
  }

  # check that the prj_name is correctly defined
  if (!endsWith(prj_name, ".dat") && !endsWith(prj_name, ".prj")) {
    # check the prj_name extension and fix it if necessary
    prj_name <- paste0(prj_name, ".dat")
  }

  # load or create the GCAM prj
  if (file.exists(prj_name)) {
    # check if the project exists and load it if possible
    prj_loaded <- TRUE
    load_project(prj_name, desired_regions, scenarios)
    # update desired_regions with the regions present in the project
    desired_regions <- filter_desired_regions(desired_regions)
  } else {
    # create project
    # check that all the paths are specified
    if (is.null(db_path)) stop("gcamreport tried to create a GCAM project but db_path was not specified.")
    if (is.null(db_name)) stop("gcamreport tried to create a GCAM project but db_name was not specified.")

    # create project
    create_project(
      db_path, db_name, prj_name, scenarios,
      desired_regions, desired_variables,
      queries_general_file, queries_nonCO2_file
    )
  }


  # make final_year as a global variable
  final_year.global <<- final_year

  # final reporting columns
  reporting_columns.global <<- append(c("Model", "Scenario", "Region", "Variable", "Unit"), as.character(seq(2005, final_year.global, by = 5)))

  # desired variables to have in the report
  variables_base <- data.frame(
    "name" = unique(gcamreport::template$Internal_variable)[!is.na(unique(gcamreport::template$Internal_variable)) & unique(gcamreport::template$Internal_variable) != ""],
    "required" = TRUE,
    stringsAsFactors = FALSE
  )

  # consider only the desired variables
  if (length(desired_variables) == 1 && desired_variables == "All") {
    variables.global <<- variables_base
  } else {
    variables.global <<- variables_base %>%
      mutate(required = if_else(
        !name %in% unique(gcamreport::template %>%
                            filter(Variable %in% desired_variables) %>%
                            pull(Internal_variable)),
        FALSE, required
      ))
  }

  rlang::inform("Loading data, performing checks, and saving output...")

  # consider the dependencies and checking functions
  variables.global <<- merge(variables.global, gcamreport::var_fun_map, by = "name", all = TRUE) %>%
    replace_na(list(required = FALSE))

  # for all desired variables, load the corresponding data
  loaded_internal_variables.global <<- c()
  desired_regions <<- desired_regions
  desired_variables <<- desired_variables
  for (i in 1:nrow(variables.global)) {
    if (variables.global$required[i]) {
      load_variable(variables.global[i, ], verbose)
    }
  }

  # set the default output_file based on the prj_name or the db_path & db_name
  if (is.null(output_file)) {
    if (prj_loaded) {
      output_file <- gsub("\\.dat$", "", prj_name)
      output_file <- paste0(output_file, "_standardized")
    } else {
      output_file <- file.path(db_path, paste0(gsub("\\.dat$", "", prj_name), "_standardized"))
    }
  }

  # bind and save results
  do_bind_results()
  save(report, file = paste0(output_file, ".RData"))

  if (save_output == TRUE || save_output %in% c("CSV", "XLSX")) {
    if (save_output == TRUE || "CSV" %in% save_output) {
      write.csv(report, file.path(paste0(output_file, ".csv")), row.names = FALSE)
    }
    if (save_output == TRUE || "XLSX" %in% save_output) {
      write_xlsx(report, file.path(paste0(output_file, ".xlsx")))
    }
  }

  if (!(identical(desired_regions, "All"))) {
    rlang::inform("No checks or vetting performed since not all regions were selected.")
  } else {
    # checks, vetting, and errors summary
    vetting_summary <- list()
    for (ch in variables.global$checks) {
      if (!is.na(ch)) {
        for (d in ch[[1]]) {
          out <- get(variables.global$fun[which(variables.global$name == d)])()
          vetting_summary[[str_sub(as.character(out$message),
                                   end = str_locate(as.character(out$message), ":") - 1
          )[1]]] <- out
        }
      }
    }
    vet <- do_check_vetting()
    rlang::inform("Vetting summary:")
    vetting_summary[[str_sub(as.character(vet$message),
                             end = str_locate(as.character(vet$message), ":") - 1
    )[1]]] <- vet
    for (e in vetting_summary) {
      print(e$message)
    }
    vetting_summary <<- vetting_summary
    cat('Type "vetting_summary$`Trade flows`" or "vetting_summary$`Vetting variables`" to know the vetting summary details\n')
    cat("You can check the vetting figure in output/figure/vetting.tiff\n")
    cat("==============================================================\n")
  }

  # remove internal variables from the environment
  rm(list = loaded_internal_variables.global, envir = .GlobalEnv)
  rm(list = c("loaded_internal_variables.global", "variables.global"), envir = .GlobalEnv)
  gc()

  if (launch_ui) {
    rlang::inform("Launching UI...")

    # launch ui
    launch_gcamreport_ui(data = report)
  }
}


#' launch_gcamreport_ui
#'
#' Launch shiny interactive user interface.
#' @param data_path RData dataset path containing the standardized data. You can obtain
#' this dataset by using the function `gcamreport::generate_report`.
#' @param data dataset containing the standardized data. You can obtain
#' this dataset by using the function `gcamreport::generate_report`.
#' @return launch shiny interactive ui.
#' @importFrom tidyr separate
#' @importFrom shiny runApp
#' @export
launch_gcamreport_ui <- function(data_path = NULL, data = NULL) {
  # check the user input
  if (is.null(data_path) && is.null(data)) {
    stop("Specify either the dataset or the dataset path to be considered.")
  } else if (!is.null(data_path) && !is.null(data)) {
    stop("Specify either the dataset or the dataset path to be considered, not both.")
  }

  # load data
  if (!is.null(data_path)) {
    data <- assign("data", get(load(data_path)))
  }

  # define the dataset for launching the ui
  sdata <<- suppressWarnings(
    data %>%
      separate(Variable, into = c("col1", "col2", "col3", "col4", "col5", "col6", "col7"), sep = "([\\|])", extra = "merge", remove = FALSE)
  )

  # create vector of available years for launching the ui
  available_years <<- as.numeric(names(sdata)[13:length(names(sdata))])

  # develop a nested list of the variables and regions for launching the ui
  cols.global <<- unique(sdata[, grepl("col", names(sdata))])
  tree_vars <<- do_mount_tree(cols.global, names(cols.global), selec = TRUE)

  tree_reg <<- do_mount_tree(gcamreport::reg_cont, names(gcamreport::reg_cont), selec = TRUE)

  # save a list of all variables
  all_varss <<- do_collapse_df(cols.global)

  runApp("inst/gcamreport_ui")
}

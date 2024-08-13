#' data_query
#'
#' Retrieves non-CO2 emissions data based on large queries.
#'
#' This function allows you to specify and fetch non-CO2 emissions data from a GCAM project or database.
#'
#' @param db_path Path to the GCAM database. Required for accessing the database.
#' @param db_name Name of the GCAM database. Required for identifying the database.
#' @param prj_name Name of the GCAM project. Can be an existing project or a new one. Accepts extensions such as .dat and .proj.
#' @param scenarios Names of the scenarios to consider. Defaults to all scenarios available in the project or database.
#' @param type Type of non-CO2 emissions query. Must be one of 'nonCO2 emissions by region' or 'nonCO2 emissions by sector'.
#' @param desired_regions Regions to include in the report. Defaults to 'All'. Specify a vector for specific regions. To view available options, run `available_regions()`. Note: The dataset will include only the specified regions, which will make up "World".
#' @param GCAM_version GCAM version to use. Options are 'v7.0' (default) or 'v6.0'.
#' @param queries_nonCO2_file Full path to an XML query file (including file name and extension) for long non-CO2 queries: "nonCO2 emissions by sector (excluding resource production)" and "nonCO2 emissions by region". Defaults to the nonCO2 query file compatible with the specified `GCAM_version`.
#'
#' @return A dataframe containing the data retrieved from the specified non-CO2 emissions query.
#' @export
data_query <- function(type, db_path, db_name, prj_name, scenarios,
                       desired_regions = "All", GCAM_version = 'v7.0',
                       queries_nonCO2_file = NULL) {
  if (identical(desired_regions, "All")) {
    desired_regions <- NULL
  }

  dt <- data.frame()

  if(is.null(queries_nonCO2_file)) {
    xml <- transform_to_xml(get(paste('queries_general',GCAM_version,sep='_'), envir = asNamespace("gcamreport")))
  } else if (is.list(queries_nonCO2_file)) {
    xml <- transform_to_xml(queries_nonCO2_file)
  } else {
    xml <- xml2::read_xml(queries_nonCO2_file)
  }
  qq <- xml2::xml_find_first(xml, paste0("//*[@title='", type, "']"))

  for (sc in scenarios) {
    emiss_list <- get(paste('emissions_list',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))
    while (length(emiss_list) > 0) {
      current_emis <- emiss_list[1:min(21, length(emiss_list))]
      qq_sec <- gsub("current_emis", paste0("(@name = '", paste(current_emis, collapse = "' or @name = '"), "')"), qq)

      prj_tmp <- rgcam::addSingleQuery(
        conn = rgcam::localDBConn(db_path,
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
        dt <- dplyr::bind_rows(dt, tmp)
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
#' Loads a specified GCAM project into the global environment.
#'
#' This function imports a GCAM project from the specified path and makes it available in the global environment.
#'
#' @param project_path Full path to the GCAM project file, including the project name and extension (e.g., .dat, .proj). This file is loaded into the global environment.
#' @param desired_regions Regions to include in the project data. Defaults to 'All'. Specify a vector to include specific regions. To view available regions, run `available_regions()`. Note: Only the specified regions will be included in the dataset, forming the "World" for the project.
#' @param scenarios Names of the scenarios to consider from the project. Defaults to all scenarios available within the project. If specific scenarios are needed, provide a vector of scenario names.
#'
#' @return The function loads the specified GCAM project into the global environment. It does not return a value, but the project data becomes available for use in further analysis or reporting.
#' @export
load_project <- function(project_path, desired_regions = "All", scenarios = NULL) {
  # rm variable "prj" from the environment if exists
  if (exists("prj")) rm(prj, envir = .GlobalEnv)

  rlang::inform("Loading project...")

  # load the project
  prj <- rgcam::loadProject(project_path)

  # check the scnarios are present in the prj
  if (is.null(scenarios)) {
    scenarios.global <<- rgcam::listScenarios(prj)
  } else {
    scenarios.global <<- dplyr::intersect(scenarios, rgcam::listScenarios(prj))
    # check the user input
    if (length(scenarios) > length(scenarios.global)) {
      check_scen <- dplyr::setdiff(scenarios, rgcam::listScenarios(prj))
      tmp <- paste(check_scen, collapse = ", ")
      if (length(check_scen) > 1) stop("The desired scenarios ", tmp, " are not present in the loaded project.\n")
      if (length(check_scen) == 1) stop("The desired scenario ", tmp, " is not present in the loaded project.\n")
    }
    # drop unnecessary scenarios
    for (i in rgcam::listScenarios(prj)[!rgcam::listScenarios(prj) %in% scenarios]) {
      prj <- rgcam::dropScenarios(prj, i)
    }
  }

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

  prj <<- prj
}


#' create_project
#'
#' Creates and loads a specified GCAM project into the global environment.
#'
#' This function allows for the creation of a new GCAM project or loading an existing one. It sets up the project with specified scenarios, regions, and variables, and saves it locally if needed. The project is then made available in the global environment for further use.
#'
#' @param db_path Optional. Full path to the GCAM database. Required if creating a new project.
#' @param db_name Optional. Name of the GCAM database. Required if creating a new project.
#' @param prj_name Name of the GCAM project. Can be an existing project name (loads the project) or a new project name (creates a new project). Accepts extensions: .dat and .proj.
#' @param scenarios Names of the scenarios to include. Defaults to all scenarios available in the project or database.
#' @param desired_regions Regions to include in the report. Defaults to 'All'. Specify a vector for specific regions. To view available options, run `available_regions()`. Note: The dataset will include only the specified regions, forming the "World" for the project.
#' @param desired_variables Variables to include in the report. Defaults to 'All'. Specify a vector for specific variables. To view available options, run `available_variables()`. Note: Global variables like "Emissions" will only account for selected variables. For example, selecting "Emissions" and "Emissions|CO2" will make "Emissions" account only for "Emissions|CO2", excluding other variables such as "Emissions|CH4" or "Emissions|NH3".
#' @param GCAM_version GCAM version to use. Options are 'v7.0' (default) or 'v6.0'.
#' @param queries_general_file Optional. Full path to a general XML query file (including file name and extension). Defaults to a general query file compatible with the specified `GCAM_version` that reports all standardized variables.
#' @param queries_nonCO2_file Optional. Full path to an XML query file (including file name and extension) for non-CO2 queries, such as "nonCO2 emissions by sector (excluding resource production)" and "nonCO2 emissions by region". Defaults to a non-CO2 query file compatible with the specified `GCAM_version`.
#'
#' @return Loads the specified project into the global environment and saves the project locally if it is created. The function sets up the project with the given parameters and makes it available for further analysis and reporting.
#' @export
create_project <- function(db_path, db_name, prj_name, scenarios = NULL,
                           desired_regions = "All", desired_variables = "All",
                           GCAM_version = 'v7.0',
                           queries_general_file = NULL, queries_nonCO2_file = NULL) {
  Internal_variable <- Variable <- required <- available_scenarios <- name <- NULL

  # rm variable "prj" from the environment if exists
  if (exists("prj")) rm(prj, envir = .GlobalEnv)

  rlang::inform("Project does not exists. Creating project...")

  # create the project
  conn <- rgcam::localDBConn(db_path,
    db_name,
    migabble = FALSE
  )

  available_scenarios <- rgcam::listScenariosInDB(conn) %>%
    dplyr::pull(name)
  # check user input
  if (is.null(scenarios)) {
    scenarios <- available_scenarios
  } else {
    check_scen <- dplyr::setdiff(scenarios, available_scenarios)
    tmp <- paste(check_scen, collapse = ", ")
    if (length(check_scen) > 1) stop("The desired scenarios ", tmp, " are not present in the database.\n")
    if (length(check_scen) == 1) stop("The desired scenario ", tmp, " is not present in the database.\n")
  }

  # read the query file
  if(is.null(queries_general_file)) {
    queries_short <- get(paste('queries_general',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))
  } else {
    queries_short <- rgcam::parse_batch_query(queries_general_file)
  }

  if(is.null(queries_nonCO2_file)) {
    queries_nonCO2_file <- queries_large <- get(paste('queries_nonCO2',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))
  } else {
    queries_large <- rgcam::parse_batch_query(queries_nonCO2_file)
  }

  # subset the queries necessary for the selected variables
  if (!(length(desired_variables) == 1 && desired_variables == "All")) {
    # create a mapping with the Variables, Internal variables, functions to load
    # them, and the dependencies
    required_internal_variables <- gcamreport::var_fun_map %>%
      dplyr::rename("Internal_variable" = "name") %>%
      dplyr::left_join(
        get(paste('template',GCAM_version,sep='_'), envir = asNamespace("gcamreport")) %>%
          dplyr::filter(!is.na(Internal_variable)),
        by = "Internal_variable",
        multiple = "all"
      ) %>%
      dplyr::mutate(required = dplyr::if_else(Variable %in% desired_variables, TRUE, FALSE))

    # create a vector with the required queries for the desired variables
    required_queries <- c()
    for (req_int_var in unique(required_internal_variables %>%
      dplyr::filter(required == TRUE) %>%
      dplyr::pull(Variable))) {
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
        rgcam::runQuery(conn, bq$query, sc, bq$regions, warn.empty = FALSE)
      })
      if (nrow(table) > 0) {
        prj_tmp <- rgcam::addQueryTable(
          project = prj_name, qdata = table,
          queryname = qn, clobber = FALSE,
          saveProj = FALSE, show_col_types = FALSE
        )
        if (exists("prj")) {
          prj <- rgcam::mergeProjects(prj_name, list(prj, prj_tmp), clobber = FALSE, saveProj = FALSE)
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
  if (!"nonCO2 emissions by sector (excluding resource production)" %in% rgcam::listQueries(prj) &&
    "nonCO2 emissions by sector (excluding resource production)" %in% names(queries_touse_large)) {
    rlang::inform("nonCO2 emissions by sector (excluding resource production)")

    # rm variable "prj_tmp" from the environment if exists
    if (exists("prj_tmp")) rm(prj_tmp)

    dt_sec <- data_query("nonCO2 emissions by sector (excluding resource production)",
                         db_path, db_name, prj_name, scenarios, desired_regions, GCAM_version, queries_nonCO2_file)
    prj_tmp <- rgcam::addQueryTable(
      project = prj_name, qdata = dt_sec, saveProj = FALSE,
      queryname = "nonCO2 emissions by sector (excluding resource production)", clobber = FALSE
    )
    prj <- rgcam::mergeProjects(prj_name, list(prj, prj_tmp), clobber = FALSE, saveProj = FALSE)
    rm(prj_tmp)
  }
  if (!"nonCO2 emissions by region" %in% rgcam::listQueries(prj) &&
    "nonCO2 emissions by region" %in% names(queries_touse_large)) {
    rlang::inform("nonCO2 emissions by region")

    # rm variable "prj_tmp" from the environment if exists
    if (exists("prj_tmp")) rm(prj_tmp)

    dt_reg <- data_query("nonCO2 emissions by region",
                         db_path, db_name, prj_name, scenarios, desired_regions, GCAM_version, queries_nonCO2_file)
    prj_tmp <- rgcam::addQueryTable(
      project = prj_name, qdata = dt_reg, saveProj = FALSE,
      queryname = "nonCO2 emissions by region", clobber = FALSE
    )
    prj <- rgcam::mergeProjects(prj_name, list(prj, prj_tmp), clobber = FALSE, saveProj = FALSE)
    rm(prj_tmp)
  }

  # Fill with an empty datatable the possible 'CO2 price' query if necessary
  if (!"CO2 prices" %in% rgcam::listQueries(prj) &&
    "CO2 prices" %in% names(queries_touse_short)) {
    # rm variable "prj_tmp" from the environment if exists
    if (exists("prj_tmp")) rm(prj_tmp)

    l <- length(rgcam::listScenarios(prj))
    dt <- data.frame(
      Units = rep(NA, l),
      scenario = rgcam::listScenarios(prj),
      year = rep(NA, l),
      market = rep(NA, l),
      value = rep(NA, l)
    )
    prj_tmp <- rgcam::addQueryTable(
      project = prj_name, qdata = dt,
      queryname = "CO2 prices", clobber = TRUE
    )
    prj <- rgcam::mergeProjects(prj_name, list(prj, prj_tmp), clobber = TRUE, saveProj = FALSE)
    rm(prj_tmp)
  }

  # save the project
  rgcam::saveProject(prj, file = file.path(db_path, paste(db_name, prj_name, sep = "_")))
  rlang::inform(paste0("rgcam project saved in ",file.path(db_path, paste(db_name, prj_name, sep = "_"))))

  scenarios.global <<- rgcam::listScenarios(prj)

  prj <<- prj
}


#' load_variable
#'
#' Recursively loads the specified variable and its dependent variables.
#'
#' This internal function is used to load a given variable from the GCAM project along with any dependent variables that are required for its proper context and calculations.
#'
#' @param var The name of the variable to be loaded. This should be specified as a character string.
#' @param GCAM_version The version of GCAM to use. Options are 'v7.0' (default) or 'v6.0'.
#' @param GWP_version The version of Global Warming Potential (GWP) values to use. Options are 'AR5' (default), 'AR6', or 'AR4'.
#'
#' @return Loads the specified variable and its dependencies into the environment. This function does not return a value but ensures that the variable and its dependencies are available for further processing.
#'
#' @keywords internal
#' @export
load_variable <- function(var, GCAM_version = "v7.0", GWP_version = 'AR5') {
  # base case: if variable already loaded, return
  if (exists(var$name)) {
    return()
  }

  # if the variable has dependencies, load them
  if (!is.na(var$dependencies)) {
    for (d in var$dependencies[[1]]) {
      load_variable(variables.global[which(variables.global$name == d), ], GCAM_version, GWP_version)
    }
  }

  # print the variable's name
  print(var$name)

  # load the variable
  arguments <- formals(var$fun)
  if ('GCAM_version' %in% names(arguments) && 'GWP_version' %in% names(arguments)) {
    get(var$fun)(GCAM_version, GWP_version)
  } else if ('GCAM_version' %in% names(arguments) && !'GWP_version' %in% names(arguments)) {
    get(var$fun)(GCAM_version)
  } else if (!'GCAM_version' %in% names(arguments) && 'GWP_version' %in% names(arguments)) {
    get(var$fun)(GWP_version)
  } else {
    get(var$fun)()
  }

  # keep record of the loaded variables
  loaded_internal_variables.global <<- c(loaded_internal_variables.global, var$name)
}


#' load_query
#'
#' Recursively loads the necessary queries for the specified variables.
#'
#' This internal function is used to recursively load queries required for a given variable and its dependencies. It ensures that all necessary queries are available for processing the specified variables.
#'
#' @param var The name of the variable for which queries are to be loaded. This should be provided as a character string.
#' @param base_data A dataframe containing the internal variables required for the queries. This dataframe should include necessary context and metadata for query loading.
#' @param final_queries A vector of query names or identifiers that need to be loaded. This parameter specifies which queries are to be retrieved for the variable.
#'
#' @return The function ensures that the specified queries are loaded and available. It does not return a value directly but updates the internal state to include the necessary queries.
#'
#' @keywords internal
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
#' Retrieves and optionally prints a list of all available regions for the IAMC reporting dataset.
#'
#' This function provides a list of regions that are available for use in IAMC reporting. By default, it prints this list, but it can also be used to obtain the list programmatically.
#'
#' @param print Logical. If TRUE (default), prints the list of available regions to the console. If FALSE, suppresses the printing and only returns the list.
#'
#' @return A vector of character strings representing the names of all available regions. If `print` is TRUE, the function also prints this list to the console.
#'
#' @export
available_regions <- function(print = TRUE) {
  continent <- region <- NULL

  av_reg <- gcamreport::reg_cont %>%
    dplyr::mutate(region = dplyr::if_else(continent == "World", "World", region))

  if (print) {
    for (it in unique(av_reg$region)) {
      print(it)
    }
  }

  return(invisible(unique(av_reg$region)))
}


#' available_continents
#'
#' Retrieves and optionally prints a list of all available continents/regions' groups for the IAMC reporting dataset.
#'
#' This function provides a list of regions' groups that are available for use in IAMC reporting. By default, it prints this list, but it can also be used to obtain the list programmatically.
#'
#' @param print Logical. If TRUE (default), prints the list of available regions' groups. to the console. If FALSE, suppresses the printing and only returns the list.
#'
#' @return A vector of character strings representing the names of all available regions' groups. If `print` is TRUE, the function also prints this list to the console.
#'
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
#' Retrieves and optionally prints a list of all available variables. for the IAMC reporting dataset.
#'
#' This function provides a list of variables that are available for use in IAMC reporting. By default, it prints this list, but it can also be used to obtain the list programmatically.
#'
#' @param print Logical. If TRUE (default), prints the list of available variables to the console. If FALSE, suppresses the printing and only returns the list.
#'
#' @return A vector of character strings representing the names of all available variables. If `print` is TRUE, the function also prints this list to the console.
#'
#' @export
available_variables <- function(print = TRUE) {
  Internal_variable <- NULL

  av_var <- gcamreport::template %>%
    dplyr::filter(!is.na(Internal_variable) & Internal_variable != "")

  if (print) {
    for (it in unique(av_var$Variable)) {
      print(it)
    }
  }

  return(invisible(unique(av_var$Variable)))
}



#' generate_report
#'
#' Main function for generating a GCAM project report. This function handles:
#' - Creating or loading a GCAM project.
#' - Standardizing the data and saving it in RData, CSV, and XLSX formats.
#' - Running vetting verifications.
#' - Launching the User Interface (UI).
#'
#' You can specify the regions and/or variables to be reported and provide:
#' - Either `db_path`, `db_name`, `prj_name`, and `scenarios` to create a new project
#'   and generate the report.
#' - Or `prj_name` and `scenarios` to produce a report from an existing project.
#'
#' The resulting RData output can be used to manually call `launch_gcamreport_ui`.
#'
#' @param db_path Full path to the GCAM database. Required if creating a new project.
#' @param db_name Name of the GCAM database. Required if creating a new project.
#' @param prj_name Name of the GCAM project. Can be an existing project name (loads the project) or a new project name (creates a new project). Accepts extensions: .dat and .proj.
#' @param scenarios Names of the scenarios to consider. Defaults to all scenarios in the project or database.
#' @param final_year Final year of the data. Defaults to 2100. Note: `final_year` must be at least 2025 and must align with available 5-year intervals, such as 2025, 2030, 2035, 2040, etc.
#' @param desired_variables Variables to include in the report. Defaults to 'All'. Specify a vector for specific variables. To view available options, run `available_variables()`. Note: Global variables like "Emissions" will only account for selected variables. E.g., if you select "Emissions" and "Emissions|CO2", "Emissions" will only account for "Emissions|CO2", and will not account for other variables such as "Emissions|CH4" or "Emissions|NH3".
#' @param desired_regions Regions to include in the report. Defaults to 'All'. Specify a vector for specific regions. To view available options, run `available_regions()`. Note: The dataset will include only the specified regions, which will make up "World".
#' @param desired_continents Continent/region groups to include in the report. Defaults to 'All'. Specify a vector for specific groups. To view available options, run `available_continents()`. Note: The dataset will include only the specified groups, which will make up "World".
#' @param save_output If `TRUE` (default), saves reporting data in CSV and XLSX formats. If `FALSE`, data is not saved. If 'CSV' or 'XLSX', data will be saved only in the specified format.
#' @param output_file File path and name for saving the data. If not specified, defaults to the directory of the database or project file with a default name containing 'standardized'. Provide a full path without an extension, which will be automatically added.
#' @param launch_ui If `TRUE` (default), launches the User Interface. If `FALSE`, does not launch the UI.
#' @param GCAM_version GCAM version to use: 'v7.0' (default) or 'v6.0'.
#' @param GWP_version Global Warming Potential values version: 'AR5' (default), 'AR6', or 'AR4'.
#' @param queries_general_file Optional. Full path to a general XML query file (including file name and extension). Defaults to a general query file compatible with the specified `GCAM_version` that reports all standardized variables.
#' @param queries_nonCO2_file Optional. Full path to an XML query file (including file name and extension) for non-CO2 queries, such as "nonCO2 emissions by sector (excluding resource production)" and "nonCO2 emissions by region". Defaults to a non-CO2 query file compatible with the specified `GCAM_version`.
#'
#' @return Saves RData, CSV, and XLSX files with standardized variables, launches the user interface, and saves the GCAM project file if created.
#' @export
generate_report <- function(db_path = NULL, db_name = NULL, prj_name, scenarios = NULL, final_year = 2100,
                            desired_variables = "All", desired_regions = "All", desired_continents = "All",
                            save_output = TRUE, output_file = NULL, launch_ui = TRUE,
                            GCAM_version = 'v7.0', GWP_version = 'AR5',
                            queries_general_file = NULL, queries_nonCO2_file = NULL) {
  continent <- region <- name <- Variable <- Internal_variable <- required <- prj_loaded <- NULL

  # boolean variable
  prj_loaded <- FALSE

  # check that GCAM_version is available
  if (is.character(GCAM_version)) {
    if (!GCAM_version %in% gcamreport::available_GCAM_versions) {
      stop(sprintf(
        "Invalid GCAM_version '%s'. Available versions are: %s. Please choose one of these versions.",
        GCAM_version, paste(gcamreport::available_GCAM_versions, collapse = ", ")
      ))
    }
  } else {
    stop(sprintf(
      "GCAM_version must be a character string, but you provided a value of type '%s'. Please specify the GCAM_version as a string, e.g., GCAM_version = 'v7.0'.",
      class(GCAM_version)
    ))
  }

  # check that GWP_version is available
  if (is.character(GWP_version)) {
    if (!GWP_version %in% gcamreport::available_GWP_versions) {
      stop(sprintf(
        "Invalid GWP_version '%s'. Available versions are: %s. Please choose one of these versions.",
        GWP_version, paste(gcamreport::available_GWP_versions, collapse = ", ")
      ))
    }
  } else {
    stop(sprintf(
      "GWP_version must be a character string, but you provided a value of type '%s'. Please specify the GWP_version as a string, e.g., GWP_version = 'AR5'.",
      class(GWP_version)
    ))
  }

  # check that final_year is >= 2025
  if (final_year < 2025) {
    stop(sprintf(
      "'final_year' is set to '%s' but must be at least 2025. Please select a valid year: '%s.\n",
      final_year, paste(gcamreport::available_final_year, collapse = ", ")))
  }
  # check that final_year is availabe (5-year interval)
  if (!final_year %in% gcamreport::available_final_year) {
    stop(sprintf(
      "'final_year' is set to '%s' but must align with available 5-year intervals. Please select a valid year: '%s.\n",
      final_year, paste(gcamreport::available_final_year, collapse = ", ")))
  }

  # check that desired_regions and desired_continents are not specified at the same time
  if (!identical(desired_regions, "All") && !identical(desired_continents, "All")) {
    stop("You specified both 'desired_regions' and 'desired_continents'. Only one can be specified at a time.\n")
  }

  # check that the desired_regions are available
  if (!(identical(desired_regions, "All"))) {
    check_reg <- dplyr::setdiff(desired_regions, available_regions(print = FALSE))
    if (length(check_reg) > 0) {
      tmp <- paste(check_reg, collapse = ", ")
      if (length(check_reg) > 1) {
        stop(sprintf("The desired regions %s are not available for reporting. Please check the available regions by running `available_regions()` in your console.\nFurther information at https://bc3lc.github.io/gcamreport/articles/Dataset_Generation_Tutorial.html#example-4-specify-the-regions-or-regions-groups.", tmp))
      } else if (length(check_reg) == 1) {
        stop(sprintf("The desired region %s is not available for reporting. Please check the available regions by running `available_regions()` in your console.\nFurther information at https://bc3lc.github.io/gcamreport/articles/Dataset_Generation_Tutorial.html#example-4-specify-the-regions-or-regions-groups.", tmp))
      }
    }
  }
  # check that the desired_continents are available
  if (!(length(desired_continents) == 1 && desired_continents == "All")) {
    check_cont <- dplyr::setdiff(desired_continents, available_continents(print = FALSE))
    if (length(check_cont) > 0) {
      tmp <- paste(check_cont, collapse = ", ")
      if (length(check_cont) > 1) {
        stop(sprintf("The desired continent/region groups %s are not available for reporting. Please check the available continent/region groups by running `available_continents()` in your console.\n
                     Further information at https://bc3lc.github.io/gcamreport/articles/Dataset_Generation_Tutorial.html#example-4-specify-the-regions-or-regions-groups.", tmp))
      } else if (length(check_cont) == 1) {
        stop(sprintf("The desired continent/region group %s is not available for reporting. Please check the available continent/region groups by running `available_continents()` in your console.\n
                     Further information at https://bc3lc.github.io/gcamreport/articles/Dataset_Generation_Tutorial.html#example-4-specify-the-regions-or-regions-groups.", tmp))
      }
    }
    desired_regions <- gcamreport::reg_cont %>%
      dplyr::filter(continent %in% desired_continents) %>%
      dplyr::pull(region)
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
      if (length(no_pattern) > 1) {
        stop(sprintf(
          "There are no variables containing the patterns %s available for reporting. Please check the available variables by running `available_variables()` in your console./n
          Further information at https://bc3lc.github.io/gcamreport/articles/Dataset_Generation_Tutorial.html#example-5-specify-the-variables.",
          paste(no_pattern, collapse = ", ")
        ))
      } else if (length(no_pattern) == 1) {
        stop(sprintf(
          "There is no variable containing the pattern %s available for reporting. Please check the available variables by running `available_variables()` in your console./n
          Further information at https://bc3lc.github.io/gcamreport/articles/Dataset_Generation_Tutorial.html#example-5-specify-the-variables.",
          no_pattern
        ))
      }

      # remove elements containing '*'
      contains_star <- grepl("\\*", desired_variables)
      desired_variables <- dplyr::setdiff(desired_variables, desired_variables[contains_star])
    }

    # check the user input
    check_var <- dplyr::setdiff(desired_variables, available_variables(print = FALSE))
    if (length(check_var) > 0) {
      tmp <- paste(check_var, collapse = ", ")
      if (length(check_var) > 1) {
        stop(sprintf("The variables %s are not available for reporting. Please check the available variables by running `available_variables()` in your console./n
          Further information at https://bc3lc.github.io/gcamreport/articles/Dataset_Generation_Tutorial.html#example-5-specify-the-variables.", tmp))
      } else if (length(check_var) == 1) {
        stop(sprintf("The variable %s is not available for reporting. Please check the available variables by running `available_variables()` in your console./n
          Further information at https://bc3lc.github.io/gcamreport/articles/Dataset_Generation_Tutorial.html#example-5-specify-the-variables.", tmp))
      }
    }
    if (length(desired_variables) == 0) {
      tmp <- paste(original_desired_variables, collapse = ", ")
      if (length(original_desired_variables) > 1) {
        stop(sprintf("The variables %s are not available for reporting. Please check the available variables by running `available_variables()` in your console./n
          Further information at https://bc3lc.github.io/gcamreport/articles/Dataset_Generation_Tutorial.html#example-5-specify-the-variables.", tmp))
      } else if (length(original_desired_variables) == 1) {
        stop(sprintf("The variables %s are not available for reporting. Please check the available variables by running `available_variables()` in your console./n
          Further information at https://bc3lc.github.io/gcamreport/articles/Dataset_Generation_Tutorial.html#example-5-specify-the-variables.", tmp))
      }
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
    if (is.null(db_path)) stop("The 'db_path' parameter is required to create a GCAM project but was not specified.")
    if (is.null(db_name)) stop("The 'db_name' parameter is required to create a GCAM project but was not specified.")

    # create project
    create_project(
      db_path = db_path, db_name = db_name, prj_name = prj_name, scenarios = scenarios,
      desired_regions = desired_regions, desired_variables = desired_variables,
      GCAM_version = GCAM_version, queries_general_file = queries_general_file,
      queries_nonCO2_file = queries_nonCO2_file
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
      dplyr::mutate(required = dplyr::if_else(
        !name %in% unique(gcamreport::template %>%
                            dplyr::filter(Variable %in% desired_variables) %>%
                            dplyr::pull(Internal_variable)),
        FALSE, required
      ))
  }

  rlang::inform("Loading data, performing checks, and saving output...")

  # consider the dependencies and checking functions
  variables.global <<- merge(variables.global, gcamreport::var_fun_map, by = "name", all = TRUE) %>%
    tidyr::replace_na(list(required = FALSE))

  # for all desired variables, load the corresponding data
  loaded_internal_variables.global <<- c()
  desired_regions <<- desired_regions
  desired_variables <<- desired_variables
  for (i in 1:nrow(variables.global)) {
    if (variables.global$required[i]) {
      load_variable(variables.global[i, ], GCAM_version, GWP_version)
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
      rlang::inform(paste0("Standardized dataset saved in ",file.path(paste0(output_file, ".csv"))))
    }
    if (save_output == TRUE || "XLSX" %in% save_output) {
      writexl::write_xlsx(report, file.path(paste0(output_file, ".xlsx")))
      rlang::inform(paste0("Standardized dataset saved in ",file.path(paste0(output_file, ".xlsx"))))
    }
  }

  if (identical(desired_regions, "All") || length(desired_regions) == gcamreport::GCAM_regions_number) {
    # checks, vetting, and errors summary
    vetting_summary <- list()
    for (ch in variables.global$checks) {
      if (!is.na(ch)) {
        for (d in ch[[1]]) {
          out <- get(variables.global$fun[which(variables.global$name == d)])()
          vetting_summary[[stringr::str_sub(as.character(out$message),
            end = stringr::str_locate(as.character(out$message), ":") - 1
          )[1]]] <- out
        }
      }
    }
    vet <- do_check_vetting()
    rlang::inform("Vetting summary:")
    vetting_summary[[stringr::str_sub(as.character(vet$message),
      end = stringr::str_locate(as.character(vet$message), ":") - 1
    )[1]]] <- vet
    for (e in vetting_summary) {
      print(e$message)
    }
    vetting_summary <<- vetting_summary
    cat("To view the vetting summary details, type:\n")
    cat('  - `vetting_summary$`Trade flows``\n')
    cat('  - `vetting_summary$`Vetting variables` \n')
    cat("\nYou can find the vetting figure in: `output/figure/vetting.tiff`\n")
    cat("==============================================================\n")
  } else {
    rlang::inform("No checks or vetting were performed because no regions were selected.")
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
#' Launches the Shiny interactive user interface for exploring GCAM report data.
#'
#' This function starts a Shiny application that provides an interactive interface for exploring and analyzing the standardized data generated by the `gcamreport::generate_report` function.
#'
#' @param data_path Optional. Path to an RData file containing the standardized data. If provided, this file will be used to load the data into the Shiny application. You can obtain this dataset using `gcamreport::generate_report`.
#' @param data Optional. An R dataframe or list containing the standardized data. If provided, this data will be used directly in the Shiny application. You can obtain this dataset using `gcamreport::generate_report`.
#'
#' @return Launches the Shiny interactive UI. This function does not return a value but starts the Shiny application for user interaction.
#'
#' @export
launch_gcamreport_ui <- function(data_path = NULL, data = NULL) {
  # check the user input
  if (is.null(data_path) && is.null(data)) {
    stop("Error: Neither 'data_path' nor 'data' has been provided. Please specify at least one of these: 'data_path' to point to the location of the dataset file or 'data' to provide the dataset directly.")
  } else if (!is.null(data_path) && !is.null(data)) {
    stop("Error: Both 'data_path' and 'data' have been provided. Please specify only one: either 'data_path' to point to the dataset file or 'data' to provide the dataset directly. Providing both is not allowed.")
  }

  # load data
  if (!is.null(data_path)) {
    data <- assign("data", get(load(data_path)))
  }

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

  tree_reg <<- do_mount_tree(gcamreport::reg_cont, names(gcamreport::reg_cont), selec = TRUE)

  # save a list of all variables
  all_varss <<- do_collapse_df(cols.global)

  shiny::runApp("inst/gcamreport_ui")
}

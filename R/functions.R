options(summarise.inform = FALSE)

#########################################################################
#                           ANCILLARY FUNCTIONS                         #
#########################################################################


#' find_closest_values
#'
#' Find the two closest values (one smaller and one bigger) of values_vector to target_value.
#'
#' @param values_vector Vector of values.
#' @param target_value Target value.
#' @return The two closest values (one smaller and one bigger) in one vector.
#' @export
find_closest_values <- function(values_vector, target_value) {
  smaller <- max(values_vector[values_vector <= target_value], na.rm = TRUE)
  larger  <- min(values_vector[values_vector >= target_value], na.rm = TRUE)
  return(c(smaller, larger))
}


#' interpolateGCAMdata
#'
#' Interpolate GCAM data between the years 2015 2021.
#'
#' @param data The data set to interpolate on.
#' @param yearcol The year column name. By default, `year`.
#' @param valuecol The value column name. By default, `value`.
#' @param year_to_appear The year that must be present in the interpolated data. By default, `base_year`.
#' @return data with the year year_to_appear interpolated linearly.
#' @export
interpolateGCAMdata <- function (data, yearcol = 'year', valuecol = 'value',
                                 year_to_appear = base_year) {

  # interpolate GCAM data if needed to ensure yearly resolution presence
  for (ya in year_to_appear) {
    if (!ya %in% unique(data[[yearcol]])) {
      years_to_interp <- sort(find_closest_values(unique(data[[yearcol]]), ya))

      data <- data %>%
        dplyr::group_by(across(setdiff(names(.), c(yearcol, valuecol)))) %>%
        tidyr::complete(!!yearcol := sort(unique(c(.data[[yearcol]],tidyr::full_seq(years_to_interp[1]:years_to_interp[2], 1))))) %>%
        dplyr::arrange(.data[[yearcol]]) %>%
        dplyr::mutate(!!valuecol := approx(.data[[yearcol]], .data[[valuecol]], .data[[yearcol]], method = "linear", rule = 2)$y) %>%
        dplyr::ungroup()
    }
  }

  return(data)
}


#' listYears
#'
#' Return the years of the queries available for a scenario in a project data set.
#' This function requires the data set to have been previously loaded, so it cannot take a file name.
#'
#' @param projData The data set to report on.
#' @param scenarios The name(s) of the scenario(s) to report on. If NULL, report on all of them.
#' @param queries The name(s) of the queries(s) to report on. If NULL, report on all of them.
#' @param anyscen If TRUE, then list queries that are in any scenario. If FALSE, list queries that are in all scenarios.
#' @return list of years reported in the project/scenario/queries.
#' @export
listYears <- function (projData, scenarios = NULL, queries = NULL, anyscen = TRUE) {
  if (is.character(projData)) {
    stop("This function requires the data set to have been already loaded.")
  }
  if (is.null(scenarios)) {
    scenarios <- rgcam::listScenarios(projData)
  }
  if (is.null(queries)) {
    queries <- rgcam::listQueries(projData)
  }
  sqlist <- lapply(scenarios, function(scen) {
    lapply(queries, function(quer) {
      if ("year" %in% names(projData[[scen]][[quer]])) {
        yy = unique(projData[[scen]][[quer]][['year']])
        if (length(yy) > 100) {
          NULL
        } else {
          yy
        }
      } else {
        NULL
      }
    })
  })

  combine <- if (anyscen) union else intersect

  if (identical(combine, union)) {
    # Union case: count appearances and keep values appearing >10 times
    # (avoid problems with 2020 and 2021)
    all_years <- unlist(sqlist)
    all_years <- all_years[!is.na(all_years)]
    year_counts <- table(all_years)
    if (length(queries) == 1) {
      result <- sort(as.numeric(names(year_counts)))
    } else {
      result <- sort(as.numeric(names(year_counts[year_counts > (length(queries)/2 + 1)])))
    }
  } else {
    # Intersect case: just intersect all elements
    result <- Reduce(intersect, Reduce(intersect, sqlist))
  }

  return(result)
}



#' check_inf
#'
#' An internal function designed to assess if there exist Inf value in a queary.
#' If Inf found, warn with a message.
#'
#' @param dataset Dataset to be inspected.
#' @param value_var_name Column name containing the values. By default = 'value'.
#' @param dataset_name Dataset/Query name to display the warning message.
#' @return Warning message if necessary.
#' @export
check_inf <- function(dataset, value_var_name = 'value', dataset_name = NULL) {
  all_ok <- TRUE

  if (value_var_name %in% names(dataset)){
    if (any(is.infinite(unique(dataset[[value_var_name]])))) {
      all_ok <- FALSE
      warning(sprintf('ATTENTION: The query `%s` contains Inf values.', dataset_name))
    }
  }

  return(dataset)
}


#' check_queries
#'
#' An internal function designed to assess if all the necessary queries to compute
#' the desired variable are loaded in the project.
#'
#' @param var Variable name
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @return boolean indicating if all the required queries to compute the desired variable are available.
#' @export
check_queries <- function(var, GCAM_version = 'v8.2') {
  var_fun_map <- get(paste('var_fun_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))
  queryItems <- var_fun_map[var_fun_map$name == var, "queries"][[1]]

  it = 1; allOk = TRUE
  while (sum(is.na(queryItems)) == 0 & it <= as.numeric(length(queryItems)) & allOk) {
    qi <- var_fun_map[var_fun_map$name == var, "queries"][[1]][it]
    qi <- unlist(strsplit(qi, split = "|", fixed = TRUE))
    allOk <- any(qi %in% rgcam::listQueries(prj))
    it <- it + 1
  }

  if (!allOk) {
    stop(sprintf(
      "The '%s' query is unavailable in your project but necessary to standardize the output. Please, ensure the query is valid and not returning empty results.",
      qi)
    )
  }
}




#' Filter data by desired regions.
#'
#' This function filters a dataset based on the specified regions listed in the "regions" column.
#'
#' @keywords internal tmp process
#' @param data The dataset to be filtered.
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @return A subset of the original data containing only the specified regions.
#' @importFrom magrittr %>%
#' @export
filter_data_regions <- function(data, GCAM_version = 'v8.2') {
  region <- NULL

  if (!(identical(desired_regions.global, "All"))) {
    data <- data %>%
      dplyr::filter(region %in% desired_regions.global)
  }
  data <- data %>%
    dplyr::filter(region %in% c('All',na.omit(get(paste('reg_cont',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['region']])))


  return(data)
}

#' compute_reg_sec_weight
#'
#' An internal function designed to compute the regional weights of a set of variables. The World region
#' is considered as the annual unit.
#'
#' @param dt dataset with the following columns: scenario, region, var (reporting variable), year, value
#' @return dataset with `reg_sec_weight` column.
#' @export
compute_reg_sec_weight <- function(dt) {

  if (nrow(dt) == 0) return(dt)

  # function to create cumulative segments
  get_segments <- function(x) {
    parts <- unlist(strsplit(x, "\\|"))
    sapply(seq_along(parts), function(i) paste(parts[1:i], collapse = "|"))
  }


  # compute annual good demand weights by sector
  weight_pre <- dt
  # apply the function to each row of the strings column
  segments_list <- lapply(weight_pre$var, get_segments)
  # find the maximum length of the segments for padding
  max_length <- max(sapply(segments_list, length))
  # pad each list to ensure equal length by adding NAs
  segments_list_padded <- lapply(segments_list, function(x) c(x, rep(NA, max_length - length(x))))
  # convert the list of padded segments into a data frame
  df_segments <- do.call(rbind, segments_list_padded)
  # rename the columns based on the number of parts
  colnames(df_segments) <- paste0("col", seq_len(ncol(df_segments)))
  # combine with the original dataframe
  weight_pre <- cbind(weight_pre, df_segments) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(across(col1:paste0('col',max_length), ~ifelse(. == "NA", NA, .)))

  # regional-sectorial weights. The World region weights 1 for each year
  weight_dt <- weight_pre %>%
    dplyr::mutate(reg_sec_weight = as.numeric(NA))
  for (num in rev(seq(1,max_length,1))) {
    cc = paste0('col',num)

    tmp <- weight_pre %>%
      dplyr::filter(!is.na(get(cc)))
    if (num < max_length) {
      tmp <- tmp %>%
        dplyr::filter(is.na(get(paste0('col',num+1))))
    }

    tmp <- tmp %>%
      dplyr::group_by(scenario, year, !!rlang::sym(paste0('col', num))) %>%
      dplyr::mutate(total = sum(value)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(!!paste0('weights_', cc) := as.numeric(value / total)) %>%
      dplyr::select(scenario, region, year, cc, !!rlang::sym(paste0('weights_col', num)))

    weight_dt <- weight_dt %>%
      dplyr::left_join(tmp, by = c('scenario', 'region', 'year', cc)) %>%
      dplyr::mutate(reg_sec_weight = dplyr::if_else(is.na(reg_sec_weight), !!rlang::sym(paste0('weights_col', num)), reg_sec_weight))

  }

  weight_dt <- weight_dt %>%
    dplyr::select(scenario, region, year, var, reg_sec_weight) %>%
    tidyr::complete(tidyr::nesting(scenario, var), year = c(1975, 1990, available_reporting_years), region = unique(dt$region), fill = list(reg_sec_weight = 0))


  return(weight_dt)

}

#' compute_sec_prevsec_weight
#'
#' An internal function designed to compute the regional weights of a set of variables. The World region
#' is considered as the annual unit.
#'
#' @param dt dataset with the following columns: scenario, region, var (reporting variable), year, value
#' @return dataset with `sec_prevsec_weight` column.
#' @export
compute_sec_prevsec_weight <- function(dt) {

  # function to create cumulative segments
  get_segments <- function(x) {
    parts <- unlist(strsplit(x, "\\|"))
    sapply(seq_along(parts), function(i) paste(parts[1:i], collapse = "|"))
  }


  # compute annual good demand weights by sector
  weight_pre <- dt
  # apply the function to each row of the strings column
  segments_list <- lapply(weight_pre$var, get_segments)
  # find the maximum length of the segments for padding
  max_length <- max(sapply(segments_list, length))
  # pad each list to ensure equal length by adding NAs
  segments_list_padded <- lapply(segments_list, function(x) c(x, rep(NA, max_length - length(x))))
  # convert the list of padded segments into a data frame
  df_segments <- do.call(rbind, segments_list_padded)
  # rename the columns based on the number of parts
  colnames(df_segments) <- paste0("col", seq_len(ncol(df_segments)))
  # combine with the original dataframe
  weight_pre <- cbind(weight_pre, df_segments) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(across(col1:paste0('col',max_length), ~ifelse(. == "NA", NA, .)))

  # regional-sectorial weights. The World region weights 1 for each year
  weight_dt <- weight_pre
  for (num in rev(seq(2,max_length,1))) {
    cc = paste0('col',num)

    tmp <- weight_pre %>%
      dplyr::filter(!is.na(get(cc)))
    if (num < max_length) {
      tmp <- tmp %>%
        dplyr::filter(is.na(get(paste0('col',num+1))))
    }

    tmp <- tmp %>%
      dplyr::group_by(scenario, year, region, !!rlang::sym(paste0('col', num-1))) %>%
      dplyr::mutate(total = sum(value)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(!!paste0('weights_', cc) := as.numeric(value / total)) %>%
      dplyr::select(scenario, region, year, cc, !!rlang::sym(paste0('weights_col', num)))

    weight_dt <- weight_dt %>%
      dplyr::left_join(tmp, by = c('scenario', 'region', 'year', cc))

  }

  # weight to the main sector
  to_multiply = paste(paste0('weights_col', seq(2, max_length, 1)), collapse = '*')
  weight_dt_clean <- weight_dt %>%
    dplyr::mutate(across(starts_with('weights_'), ~ ifelse(is.na(.), 1, .))) %>%
    dplyr::mutate(sec_prevsec_weight = eval(parse(text = to_multiply))) %>%
    dplyr::select(scenario, region, year, var, sec_prevsec_weight) %>%
    tidyr::complete(tidyr::nesting(scenario, var), year = c(1975, 1990, available_reporting_years), region = unique(dt$region), fill = list(sec_prevsec_weight = 0))

  # weight to the immediate parent sector
  # weight_dt_clean <- weight_dt %>%
  #   dplyr::mutate(recieving_var = sub("\\|[^|]+$", "", var)) %>%
  #   dplyr::mutate(weight = dplyr::coalesce(!!!syms(paste0("weights_col", max_length:2)))) %>%
  #   dplyr::mutate(weight = dplyr::if_else(is.na(weight), 1, weight)) %>%
  #   dplyr::select(scenario, region, recieving_var, var, year, weight)

  return(weight_dt_clean)

}



#' handle_warning
#'
#' An internal function designed to manage mismatches between mapping files and queries.
#' It prompts the user for input, offering the option to manually check the issue or to continue
#' with the current process.
#'
#' @param mapping_name1 The name of the mapping file 1.
#' @param mapping_name2 The name of the mapping file 2.
#' @param query_name The name of the query.
#' @return A warning is issued, and the user's decision (manual check or continuation) is captured.
#' @export
handle_warning <- function(mapping_name1, mapping_name2 = NULL, query_name = NULL) {
  if (is.null(mapping_name2) && is.null(query_name)) {
    stop('`mapping_name2` or `query_name` must be specified')
  }

  if (is.null(mapping_name2) && !is.null(query_name)) {
    # warning message
    warning(sprintf('ATTENTION: The mapping file `%s` has a mismatch with the query `%s`.', mapping_name1, query_name))
  } else if (!is.null(mapping_name2) && is.null(query_name)) {
    # warning message
    warning(sprintf('ATTENTION: The mapping files `%s` and `%s` have a mismatch.', mapping_name1, mapping_name2))
  } else {
    # warning message
    warning(sprintf('ATTENTION: The mapping files `%s` and `%s` have a mismatch with the query `%s`.', mapping_name1, mapping_name2, query_name))
  }

  if (.myGlobals$interactive.global & interactive()) {
    # prompt for user input
    user_input <- readline(prompt = "Do you want to manually check (M) or continue (C)? Press M or C: ")

    # handling user response
    if (toupper(user_input) %in% c("m","M")) {
      stop("Manual check requested. Stopping execution.")
    } else if (!toupper(user_input) %in% c("c","C")) {
      stop("Invalid input. Stopping execution.")
    }
    # if user chooses "C", the script continues
  } # end of checking if the script was run interactively or from commandline
}


#' left_join_strict
#'
#' A restrictive version of \code{\link{left_join}} that ensures that all keys in the left dataset have corresponding matches in the right dataset.
#' If any rows in the left dataset do not have matching keys in the right dataset, the function will throw an error.
#'
#' @param left_df A data frame. The left dataset in the join.
#' @param right_df A data frame. The right dataset in the join.
#' @param by A character vector of variables to join by. If `NULL`, the function will use all common variables.
#' @param by_message A character vector of variables to join by to output the ERROR message, if necessay. If `NULL`, the function will use the variables defined in the `by` parameter.
#' @param mapping Optional. Mapping name to be displayed in case of ERROR.
#' @param ignore Optional. Policy names introduced by the user to ignore during gcamreport processing of physical quantities, since otherwise they will be flagged as names missing from mapping files and cause an error. Note: Currently having one of the specified name patterns in any column of the query results, such as sector, subsector, input, etc. will cause the error to be disregarded. Same behavior than adding the policy names to the corresponding mappings indicating `NoReported`.
#' @param ... Additional arguments passed to `dplyr::left_join()`.
#' @return A data frame resulting from the left join. If any rows in `left_df` do not have matching keys in `right_df`, an error is thrown.
#' @export
left_join_strict <- function(left_df, right_df, by = NULL, by_message = by, mapping = "",
                             ignore = if (exists("ignore.global", envir = .myGlobals)) .myGlobals$ignore.global else NULL, ...) {
  # Perform the left join
  result <- dplyr::left_join(left_df, right_df, by = by, ...)

  # Identify unmatched rows (rows with NA in any of the columns from right_df)
  unmatched <- result %>%
    dplyr::filter(dplyr::if_any(-one_of(names(left_df)), is.na))

  # Ignore any unmatched rows which have names (in any column) specified as fine to be
  # ignored and remove them from the `result` dataset, which will be returned to the user
  if (!is.null(ignore) & nrow(unmatched) > 0) {
    unmatched_ignore <- unmatched %>%
      dplyr::filter(dplyr::if_any(.cols = everything(), ~ grepl(paste(ignore, collapse = "|"), .)))
    unmatched <- suppressMessages(
      unmatched %>%
        dplyr::anti_join(unmatched_ignore)
    )
    result <-suppressMessages(
      result %>%
        dplyr::anti_join(unmatched_ignore)
    )
  }

  # Check if there are any unmatched rows
  if (nrow(unmatched) > 0) {
    left_join_strict_details <- unique(unmatched %>%
                                         dplyr::select(by_message))
    left_join_strict_details <<- left_join_strict_details
    stop(sprintf("Error: Some rows in the left dataset do not have matching keys in the right dataset. Type `left_join_strict_details` to see the full log. Some of the rows that the mapping %s miss are:\n%s",
                 mapping,
                 paste(capture.output(print(left_join_strict_details)), collapse = "\n")))
  }

  return(result)
}


#' left_join_error_no_match
#'
#' A restrictive version of \code{\link{left_join}}. Function from \code{\link{gcamdata}}.
#'
#' @param d Data frame (typically from pipeline)
#' @param ... Rest of call to \code{\link{left_join}}
#' @param ignore_columns Optional column name(s) to ignore, character vector
#' @return Joined data.
#' @details Restrictive version of `dplyr::left_join()` meant for replacing `match` calls.
# Ensures that number of rows of data doesn't change, and everything has matched data.
#' @export
left_join_error_no_match <- function (d, ..., ignore_columns = NULL) {
  d = tibble::as_tibble(d)
  assertthat::assert_that(tibble::is_tibble(d))
  dnames <- names(d)
  drows <- nrow(d)
  d <- dplyr::left_join(d, ...)
  if (nrow(d) != drows) {
    stop("left_join_no_match: number of rows in data changed")
  }
  names_to_check <- dplyr::setdiff(names(d), dnames) %>% dplyr::setdiff(ignore_columns)
  if (any(is.na(d[names_to_check]))) {
    stop("left_join_no_match: NA values in new data columns")
  }
  d
}

#' filter_desired_regions
#'
#' Filters and returns the desired regions available in the loaded project.
#'
#' @param des_reg Vector of user-specified desired regions.
#' @return Vector of desired regions that are available in the loaded project.
#' @export
filter_desired_regions <- function(des_reg) {
  r <- 1
  rmax <- length(rgcam::listQueries(prj))
  void_tmp = TRUE
  while (r <= rmax) {
    tmp <- check_inf(rgcam::getQuery(prj, rgcam::listQueries(prj)[r]))
    if ("region" %in% colnames(tmp)) {
      des_reg <- unique(tmp$region)
      return(des_reg)
    } else if (nrow(tmp) != 0) {
      void_tmp = FALSE
    }
    r <- r + 1
  }
  if (!void_tmp) warning("Desired regions could not be filtered through the loaded project data. The standardize report will contain the regions specified by the user.")
  return(des_reg)
}



#' transform_to_xml
#'
#' Converts a list of parsed queries into an XML document.
#'
#' @param parsed_queries_list List of parsed queries.
#' @return XML document generated from the provided queries list.
#' @keywords internal
#' @export
transform_to_xml <- function(parsed_queries_list) {
  queries <- lapply(parsed_queries_list, function(query) {
    query_title <- query$title
    query_xml <- paste("<aQuery>\n  <all-regions/>\n", query$query, "</aQuery>\n", sep = "")
    return(query_xml)
  })
  xml_string <- paste("<queries>", paste(queries, collapse = ""), "</queries>", sep = "")
  xml_doc <- xml2::read_xml(xml_string)
  return(xml_doc)
}

#' start_with_pattern
#'
#' Returns elements of a vector that start with a specified pattern.
#'
#' @param vector Vector to search within.
#' @param pattern Pattern to match at the beginning of vector elements.
#' @return Subvector of `vector` containing elements that start with `pattern`.
#' @importFrom magrittr %>%
#' @keywords internal
#' @export
start_with_pattern <- function(vector, pattern) {
  matching_elements <- vector[substr(vector, 1, nchar(pattern)) == pattern]
  return(matching_elements)
}


#' filter_loading_regions
#'
#' Filters a GCAM project dataframe by the desired regions.
#'
#' @param data Dataframe to filter.
#' @param desired_regions Vector of regions to include. Defaults to 'All'. To view available regions, run `available_regions()`. The dataset will only include the specified regions.
#' @param variable Variable information for the dataset.
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @return Filtered dataframe.
#' @importFrom magrittr %>%
#' @keywords internal
#' @export
filter_loading_regions <- function(data, desired_regions = "All", variable, GCAM_version = 'v8.2') {
  market <- region <- NULL

  if (!(identical(desired_regions, "All"))) {
    desired_regions <- c(desired_regions, "global")
    # the variable CO2 prices does not contain "region", but "markets". Now we
    # filter for all market items that do not contain the desired regions
    if (variable %in% c("CO2 prices")) {
      pattern <- paste(c(
        "CO2", "airCO2", "nonCO2", "CO2_FUG", "CO2 removal",
        "H2", "Exports"
      ), collapse = "|")
      # desired_regions special case: if some "EU" region is present, consider the
      # "EU" region to compute CO2 price
      if (any(grepl("^EU", desired_regions))) {
        desired_regions_tmp <- c(desired_regions, "EU")
      } else {
        desired_regions_tmp <- desired_regions
      }
      data <- data %>%
        dplyr::mutate(region = sapply(
          strsplit(as.character(market), pattern),
          function(x) x[1]
        )) %>%
        dplyr::filter(region %in% desired_regions_tmp) %>%
        dplyr::select(-region)
    } else if (variable %in% "supply of all markets") {
      # Create regex pattern for regions
      region_pattern <- paste0("(", paste(c(available_regions(F, GCAM_version),'EU','global'), collapse = "|"), ")")
      if (any(grepl("^EU", desired_regions))) {
        desired_regions_tmp <- c(desired_regions, "EU")
      } else {
        desired_regions_tmp <- desired_regions
      }
      data <- data %>%
        dplyr::mutate(region = stringr::str_extract(market, region_pattern)) %>%
        dplyr::filter(region %in% desired_regions_tmp) %>%
        dplyr::select(-region)

    } else if (!(variable %in% c(
      "CO2 concentrations", "global mean temperature",
      "total climate forcing", "primary energy consumption with CCS by region (direct equivalent)"
    )) & ('region' %in% colnames(data))) {
      # check the desired regions are available in the data
      avail_reg <- unique(data$region)
      if (!all(desired_regions %in% avail_reg)) {
        not_avail <- setdiff(desired_regions, avail_reg)
        if (length(not_avail) == 1) {
          warning(sprintf(
            "The desired region '%s' is not available in the loaded project. Specifically, it is missing from the query '%s'. Please check the available regions in the project or update the query.",
            not_avail, variable
          ))
        } else if (length(not_avail) > 1) {
          warning(sprintf(
            "The desired regions %s are not available in the loaded project. Specifically, they are missing from the query '%s'. Please check the available regions in the project or update the query.",
            paste(shQuote(not_avail), collapse = ", "), variable
          ))
        }
      }
      data <- data %>%
        dplyr::filter(region %in% desired_regions)
    } else if (!(variable %in% c(
      "CO2 concentrations", "global mean temperature",
      "total climate forcing", "primary energy consumption with CCS by region (direct equivalent)"
    )) & ('market' %in% colnames(data))) {
      # check the desired regions are available in the data
      avail_markets <- unique(data$market)
      default_regions <- get(paste('reg_cont',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['region']]
      default_regions <- default_regions[!is.na(default_regions)]
      default_regions <- c(default_regions, 'global')
      pattern <- paste0("(", paste(default_regions, collapse = "|"), ")")
      avail_reg <- unique(stringr::str_extract(avail_markets, pattern))
      avail_reg <- avail_reg[!is.na(avail_reg) & avail_reg != "NA"]
      if (!all(desired_regions %in% avail_reg)) {
        not_avail <- setdiff(desired_regions, avail_reg)
        if (length(not_avail) == 1) {
          warning(sprintf(
            "The desired region '%s' is not available in the loaded project. Specifically, it is missing from the query '%s'. Please check the available regions in the project or update the query.",
            not_avail, variable
          ))
        } else if (length(not_avail) > 1) {
          warning(sprintf(
            "The desired regions %s are not available in the loaded project. Specifically, they are missing from the query '%s'. Please check the available regions in the project or update the query.",
            paste(shQuote(not_avail), collapse = ", "), variable
          ))
        }
      }
      data <- data %>%
        dplyr::mutate(region = stringr::str_extract(market, pattern)) %>%
        dplyr::filter(region %in% desired_regions) %>%
        dplyr::select(-region)
    }
  }

  return(data)
}


#' filter_variables
#'
#' Filters a dataframe based on specified variables.
#'
#' @param data Dataframe to filter.
#' @param variable Variable to use for filtering.
#' @return Filtered dataframe.
#' @importFrom magrittr %>%
#' @keywords internal
#' @export
filter_variables <- function(data, variable = NULL, extra = NULL) {

  # if (variable %in% variables.global[variables.global$required == TRUE, ]$name) {
  if (!(length(desired_variables.global) == 1 && desired_variables.global == "All")) {
    if ("var" %in% colnames(data)) {
      data <- data %>%
        dplyr::filter(var %in% c(desired_variables.global,'NoReported',extra))
    }
  }
  # }

  return(data)
}


#' gather_map
#'
#' Formats multiple maps into a long-format table.
#'
#' @keywords internal
#' @export
gather_map <- function(df) {
  . <- identifier <- var <- NULL

  untouched_cols <- names(df) %>% .[!grepl("var", names(df))]
  df %>%
    tidyr::gather(identifier, var, -untouched_cols) %>%
    dplyr::select(-identifier) %>%
    dplyr::filter(!is.na(var), var != "") %>%
    return()
}


#' conv_ghg_co2e
#'
#' Converts GHG emissions to CO2e.
#'
#' @param data Dataset containing GHG emissions.
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @param GWP_version Global Warming Potential (GWP) version: 'AR5' (default), 'AR6', or 'AR4'.
#' @importFrom magrittr %>%
#' @keywords internal conversion
#' @export
conv_ghg_co2e <- function(data, GCAM_version = 'v8.2', GWP_version = 'AR5') {
  ghg <- variable <- value <- GWP <- NULL

  # GHG emission conversion
  res <- suppressWarnings(
    data %>%
      # aggregate by ghg (exclude sector)
      tidyr::separate(ghg, into = c("variable", "sector"), sep = "_", fill = "right") %>%
      dplyr::filter(variable %in% get(paste('GHG_gases',GCAM_version,sep='_'), envir = asNamespace("gcamreport")),
                    sector %in% unique(get(paste('ghg_GWP',GWP_version,sep='_'), envir = asNamespace("gcamreport"))[['sector']])) %>%
      left_join_error_no_match(get(paste('ghg_GWP',GWP_version,sep='_'), envir = asNamespace("gcamreport")), by = c("variable" = "GHG_gases", "sector")) %>%
      dplyr::mutate(value = value * GWP, Units = "CO2e") %>%
      dplyr::filter(!is.na(value)) %>% # remove NAs due to unexisting subsectors
      dplyr::select(-GWP)
  )

  return(res)
}


#' conv_EJ_GW
#'
#' Converts energy from EJ to GW.
#'
#' @param data Dataset containing energy data.
#' @param cf Conversion factor for EJ to GW.
#' @param EJ Amount of energy in EJ.
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @importFrom magrittr %>%
#' @keywords internal conversion
#' @export
conv_EJ_GW <- function(data, cf, EJ, GCAM_version = 'v8.2') {
  data %>%
    dplyr::mutate(gw = EJ / (cf *
                               get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['hr_per_yr']] *
                               get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['EJ_to_GWh']]))
}

#' approx_fun
#'
#' Performs interpolation or extrapolation on given values.
#'
#' @param year Year(s) for interpolation or extrapolation.
#' @param value Values to interpolate or extrapolate from.
#' @param rule Number of points to use for extrapolation.
#' @importFrom magrittr %>%
#' @keywords internal
#' @export
approx_fun <- function(year, value, rule = 1) {
  if (rule == 1 | rule == 2) {
    res <- tryCatch(
      {
        stats::approx(as.vector(year), value, rule = rule, xout = year)$y
      },
      error = function(e) {
        message("An error occured: ", conditionMessage(e))
        return(NA)
      }
    )
  } else {
    stop("Use fill_exp_decay_extrapolate!")
  }
  return(invisible(res))
}


#' check_match
#'
#' Check for excluded and included variables: find the unique values of
#' x$colmn_x that are not covered in y$colmn_y, unless opt is set to "i",
#' in which case it returns the ones that are included
#' @param x base dataset containing colmn_x.
#' @param y base dataset containing colmn_y.
#' @param colmn_x column name present in dataset x.
#' @param colmn_y column name present in dataset y. If set to NULL, colmn_x will be used for both datasets (x and y).
#' @param opt if "e", find values present in x$colmn_x NOT PRESENT in y$colmn_y, if "i", find values PRESENT in x$colmn_x that are also present in y$colmn_y.
#' @keywords internal
#' @export
check_match <- function(x, y, colmn_x, colmn_y = NULL, opt = "e") {

  colmn_y <- ifelse(is.null(colmn_y), colmn_x, colmn_y)

  x <- tibble::as_tibble(x)
  y <- tibble::as_tibble(y)

  if (!colmn_x %in% names(x)) {
    stop(sprintf(
      "Invalid colmn_x '%s'. It is not present in dataset `x`.",
      colmn_x
    ))
  }
  if (!colmn_y %in% names(y)) {
    stop(sprintf(
      "Invalid colmn_y '%s'. It is not present in dataset `y`.",
      colmn_y
    ))
  }

  loc_x <- which(names(x) == colmn_x)
  loc_y <- which(names(y) == colmn_y)

  x <- as.matrix(x)
  y <- as.matrix(y)

  val_x <- as.data.frame(x[,loc_x])
  val_y <- as.data.frame(y[,loc_y])

  if(opt == "e") {
    excl = unique(val_x[!(val_x[,1] %in% val_y[,1]),1])
    lst <- ifelse(length(excl) == 0, print("There are no variables in the first one that are not in the second one."),
                  print(list("The following are in the first one, but not in the second one:", excl)))
  }
  if(opt == "i") {
    incl = unique(val_x[(val_x[,1] %in% val_y[,1]),1])
    lst <- ifelse(length(incl) == 0, print("There are no variables in the first one that are included in the second one."),
                  print(list("The following are in both:", incl)))
  }
}


#########################################################################
#                         LOAD QUERIES FUNCTIONS                        #
#########################################################################

# Scioeconomics
# ==============================================================================================
#' get_population
#'
#' Retrieves the population query and converts units to millions.
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @return `population_clean` global variable.
#' @keywords internal population
#' @importFrom magrittr %>%
#' @export
get_population <- function(GCAM_version = 'v8.2') {
  value <- population_clean <- NULL

  check_queries('population_clean', GCAM_version)

  population_clean <-
    check_inf(rgcam::getQuery(prj, "population by region"),
              dataset_name = "population by region") %>%
    dplyr::mutate(
      value = value *
        get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_thousand_million']],
      var = "Population"
    ) %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  population_clean <<- population_clean
}


#' get_population_weights
#'
#' Retrieves the population weight by region. World = 1
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @return `pop_weights` global variable.
#' @keywords internal population
#' @importFrom magrittr %>%
#' @export
get_population_weights <- function(GCAM_version = 'v8.2') {
  value <- pop_weights <- NULL

  check_queries('pop_weights', GCAM_version)

  pop_weights <- population_clean %>%
    dplyr::group_by(scenario, year) %>%
    dplyr::mutate(total = sum(value)) %>%
    dplyr::mutate(share = value / total) %>%
    dplyr::ungroup() %>%
    dplyr::select(scenario, region, year, share)

  pop_weights <<- pop_weights
}



#' get_poverty
#'
#' Compute povery variables
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @return `poverty_clean` global variables.
#' @keywords internal econ
#' @importFrom magrittr %>%
#' @export
get_poverty <- function(GCAM_version = 'v8.2') {
  poverty_clean <- NULL

  check_queries('poverty_clean', GCAM_version)

  if (GCAM_version %in% c(get('deciles_GCAM_versions', envir = asNamespace("gcamreport")))) {
    income <- income_raw %>% # thous 1990$ per cap
      dplyr::select(-Units, -var, decile = `gcam-consumer`, income = value) %>%
      dplyr::group_by(scenario, region, year) %>%
      dplyr::mutate(median_by_reg = median(income)) %>%
      dplyr::ungroup() %>%
      # from 1990$ to 2017$
      dplyr::mutate(income = income *
                      get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_17USD_90USD']])

    population <- population_clean %>% # million
      dplyr::select(-var, pop = value)

    # ## Population|Poverty|Below 50% of National Median Income [Share]
    # poverty_50 <- income %>%
    #   dplyr::mutate(below_reg_50 = ifelse(income < median_by_reg, T, F)) %>%
    #   dplyr::group_by(scenario, region, year) %>%
    #   dplyr::mutate(poverty_rate = 10 * sum(below_reg_50, na.rm = TRUE),
    #                 var = 'Population|Poverty|Below 50% of National Median Income [Share]') %>%
    #   dplyr::ungroup()

    # Population|Poverty|Extreme Poverty
    threshold <- 2.15 #USD_2017 2.15 PPP per day (World Bank definition)
    poverty_extreme <- income %>%
      # check threshold
      dplyr::mutate(extreme_poverty = ifelse(income < threshold, T, F)) %>%
      dplyr::group_by(scenario, region, year) %>%
      dplyr::summarise(value = 10 * sum(extreme_poverty, na.rm = TRUE),
                    var = 'Population|Poverty|Extreme Poverty') %>%
      dplyr::ungroup() %>%
      # compute population
      left_join_error_no_match(population,
                               by = c('scenario','region','year')) %>%
      dplyr::mutate(value = value / 1e2 * pop) %>%
      dplyr::select(-pop)


    # Population|Poverty|LMIC Poverty Line
    threshold <- 3.65 #USD_2017 3.65 PPP per day (World Bank LMIC definition)
    poverty_LMIC <- income %>%
      # check threshold
      dplyr::mutate(extreme_poverty = ifelse(income < threshold, T, F)) %>%
      dplyr::group_by(scenario, region, year) %>%
      dplyr::summarise(value = 10 * sum(extreme_poverty, na.rm = TRUE),
                    var = 'Population|Poverty|LMIC Poverty Line') %>%
      dplyr::ungroup() %>%
      # compute population
      left_join_error_no_match(population,
                               by = c('scenario','region','year')) %>%
      dplyr::mutate(value = value / 1e2 * pop) %>%
      dplyr::select(-pop)


    # Population|Poverty|UMIC Poverty Line
    threshold <- 6.85 #USD_2017 6.85 PPP per day (World Bank UMIC definition)
    poverty_UMIC <- income %>%
      # check threshold
      dplyr::mutate(extreme_poverty = ifelse(income < threshold, T, F)) %>%
      dplyr::group_by(scenario, region, year) %>%
      dplyr::summarise(value = 10 * sum(extreme_poverty, na.rm = TRUE),
                    var = 'Population|Poverty|UMIC Poverty Line') %>%
      dplyr::ungroup() %>%
      # compute population
      left_join_error_no_match(population,
                               by = c('scenario','region','year')) %>%
      dplyr::mutate(value = value / 1e2 * pop) %>%
      dplyr::select(-pop)

    poverty_clean <- rbind(
      poverty_extreme,
      poverty_LMIC,
      poverty_UMIC
    )

  } else {
    poverty_clean <- NULL
    warning("The 'Poverty' variables are unavailable in your project. They are only supported from GCAM version 7.1 onwards. If you are using version 7.1 or newer, please ensure the `subregional income` query is valid and not returning empty results.")
  }

  poverty_clean <<- poverty_clean

}



#' get_inequality
#'
#' Compute inequality variables
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @return `inequality_clean` global variables.
#' @keywords internal econ
#' @importFrom magrittr %>%
#' @export
get_inequality <- function(GCAM_version = 'v8.2') {
  inequality_clean <- NULL

  check_queries('inequality_clean', GCAM_version)

  if (GCAM_version %in% c(get('deciles_GCAM_versions', envir = asNamespace("gcamreport")))) {
    income <- income_raw %>% # thous 1990$ per cap
      dplyr::select(-Units, -var, decile = `gcam-consumer`, income = value) %>%
      dplyr::group_by(scenario, region, year) %>%
      dplyr::mutate(national_average = mean(income)) %>%
      dplyr::ungroup() %>%
      # from 1990$ to 2017$
      dplyr::mutate(income = income *
                      get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_17USD_90USD']]) %>%
      # decile number
      dplyr::mutate(decile_num = as.numeric(stringr::str_extract(decile, "(?<=d)\\d+")))

    population <- population_clean %>% # million
      dplyr::select(-var, pop = value)

    # Inequality|Average Income|Bottom 40% [Ratio]
    ineq_40 <- income %>%
      # ratio: bottom 40% (deciles 1-4) / national average
      dplyr::filter(decile_num <= 4) %>%
      dplyr::group_by(scenario, region, year) %>%
      dplyr::mutate(bottom40 = mean(income)) %>%
      dplyr::reframe(value = 100 * bottom40 / national_average,
                     var = 'Inequality|Average Income|Bottom 40% [Ratio]')


    # Inequality index|Gini
    ineq_gini <- income %>%
      dplyr::group_by(scenario, region, year) %>%
      # order deciles 1 - 10
      dplyr::arrange(decile_num) %>%
      # proprtion of income by decile
      dplyr::mutate(income_share = income / sum(income)) %>%
      # accumulated income (Y_i)
      dplyr::mutate(cum_income = cumsum(income_share)) %>%
      # Gini computation by the trapezium formula
      dplyr::summarize(value = {
        # add the (0,0) point to do the full computation
        x <- seq(0, 1, by = 0.1)
        y <- c(0, cum_income)

        # Area below the Lorenz curve (Trapezium integral)
        # Area = sum ( (x_i - x_{i-1}) * (y_i + y_{i-1}) / 2 )
        lorenz_area <- sum(diff(x) * (y[-1] + y[-length(y)]) / 2)

        # Gini = equality area [0.5] - Lorenz area) / equality area [0.5]
        # which simplifying is: 1 - 2 * lorenz_area
        1 - 2 * lorenz_area
      },
      var = 'Inequality index|Gini',
      .groups = "drop")

    inequality_clean <- rbind(
      ineq_40,
      ineq_gini
    )

  } else {
    inequality_clean <- NULL
    warning("The 'Inequality' variables are unavailable in your project. They are only supported from GCAM version 7.1 onwards. If you are using version 7.1 or newer, please ensure the `subregional income` query is valid and not returning empty results.")
  }

  inequality_clean <<- inequality_clean

}




#' get_income
#'
#' Compute share of total income my decile
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @return `income_clean` and `income_raw` global variables.
#' @keywords internal econ
#' @importFrom magrittr %>%
#' @export
get_income <- function(GCAM_version = 'v8.2') {
  income_clean <- income_raw <- income_w <- income <- NULL

  check_queries('income_clean', GCAM_version)

  if (GCAM_version %in% c(get('deciles_GCAM_versions', envir = asNamespace("gcamreport")))) {
    income_raw <-
      check_inf(rgcam::getQuery(prj, "subregional income"),
                dataset_name = "subregional income") %>%
      dplyr::filter(grepl('resid',`gcam-consumer`)) %>%
      # update variable name
      dplyr::mutate(var = paste0('Income|D',
                                 stringr::str_extract(`gcam-consumer`, "(?<=_d)\\d+"),
                                 ' [Share]'))

    # compute income share by decile
    income <- income_raw %>%
      dplyr::group_by(scenario, region, year) %>%
      dplyr::mutate(total = sum(value)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(value = 100 * value / total) %>%
      dplyr::select(dplyr::all_of(gcamreport::long_columns))

    # compute income share by decile at the World level
    income_w <- income_raw %>%
      dplyr::group_by(scenario, year, var) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(scenario, year) %>%
      dplyr::mutate(total = sum(value)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(value = 100 * value / total,
                    region = 'World') %>%
      dplyr::select(dplyr::all_of(gcamreport::long_columns))

    # aggregate regional and global data
    income_clean <- rbind(
      income,
      income_w
    )

  } else {
    income_clean <- NULL
    income_raw <- NULL
    warning("The 'Income by Decile' variables are unavailable in your project. They are only supported from GCAM version 7.1 onwards. If you are using version 7.1 or newer, please ensure the `subregional income` query is valid and not returning empty results.")
  }

  income_raw <<- income_raw
  income_clean <<- income_clean

}


#' get_labor
#'
#' Compute active and inactive labor force
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @return `labor_clean` global variables.
#' @keywords internal econ
#' @importFrom magrittr %>%
#' @export
get_labor <- function(GCAM_version = 'v8.2') {
  labor_clean <- NULL

  check_queries('labor_clean', GCAM_version)

  if (GCAM_version %in% c('v7.0',get('deciles_GCAM_versions', envir = asNamespace("gcamreport"))) & 'National Account' %in% rgcam::listQueries(prj)) {
    labor_active <-
      check_inf(rgcam::getQuery(prj, "National Account"),
                dataset_name = "National Account") %>%
      dplyr::filter(year != dplyr::if_else(base_year == 2021, 2020, 2021)) %>% # avoid null values present in the data due to the byu
      dplyr::filter(account == 'labor-force') %>%
      dplyr::mutate(
        value = value *
          get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_thousand_million']],
        var = "Labor Force|Employed"
      ) %>%
      dplyr::select(dplyr::all_of(gcamreport::long_columns))

    labor_inactive <- merge(
      population_clean %>%
        dplyr::rename(pop = value),
      labor_active %>%
        dplyr::rename(labor_active = value),
      by = c('scenario','region','year')
    ) %>%
      dplyr::mutate(value = pop - labor_active) %>%
      dplyr::mutate(var = 'Labor Force|Inactive') %>%
      dplyr::select(dplyr::all_of(gcamreport::long_columns))

    labor_clean = rbind(
      labor_active,
      labor_inactive
    )
  } else {
    labor_clean <- NULL
    warning("The 'Labor' variables are unavailable in your project. They are only supported from GCAM version 7.0 onwards. If you are using version 7.0 or newer, please ensure the `National Accunt` query is valid and not returning empty results.")
  }

  labor_clean <<- labor_clean

}

#' get_gdp_ppp
#'
#' Retrieves GDP (PPP) data, computes regional GDP and annual GDPpc growth rate, and converts units to 10 USD.
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @return `GDP_PPP_clean`, `GDP_PPP_pc_growth_clean`, and `GDP_PPP_pc_oecd_share_clean` global variables.
#' @keywords internal econ
#' @importFrom magrittr %>%
#' @export
get_gdp_ppp <- function(GCAM_version = 'v8.2') {
  value <- pop_mill <- GDP_PPP_clean <- GDP_PPP_pc_growth_clean <-
    GDP_PPP_pc_oecd_share_clean <- NULL

  check_queries('GDP_PPP_clean', GCAM_version)
  check_queries('GDP_PPP_pc_growth_clean', GCAM_version)
  check_queries('GDP_PPP_pc_oecd_share_clean', GCAM_version)

  GDP_PPP_clean <-
    check_inf(rgcam::getQuery(prj, "GDP per capita PPP by region"),
              dataset_name = "GDP per capita PPP by region") %>%
    dplyr::filter(value != 0) %>% # avoid null values present in the data due to the byu
    left_join_error_no_match(population_clean %>% dplyr::rename(pop_mill = value), by = c("scenario", "region", "year")) %>%
    dplyr::mutate(
      value = value * pop_mill * get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_90USD_10USD']],
      var = "GDP|PPP"
    ) %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  GDP_PPP_pc_growth_clean <-
    check_inf(rgcam::getQuery(prj, "GDP per capita PPP by region"),
              dataset_name = "GDP per capita PPP by region") %>%
    dplyr::filter(value != 0) %>% # avoid null values present in the data due to the byu
    dplyr::arrange(year) %>%
    tibble::as_tibble() %>%
    dplyr::group_by(scenario, region) %>%
    dplyr::mutate(value = 100*((value / dplyr::lag(value))^(1 / (year - dplyr::lag(year))) - 1)) %>%
    dplyr::ungroup() %>%
    # Remove the first rate, which is NA
    dplyr::filter(!is.na(value)) %>%
    dplyr::mutate(
      var = "GDP|PPP [Growth Rate per capita]"
    ) %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  GDP_PPP_pc_oecd_share_clean <-
    check_inf(rgcam::getQuery(prj, "GDP per capita PPP by region"),
              dataset_name = "GDP per capita PPP by region") %>%
    dplyr::filter(value != 0) %>% # avoid null values present in the data due to the byu
    dplyr::arrange(year) %>%
    tibble::as_tibble() %>%
    dplyr::mutate(
      value = 100 * (value * get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_90USD_10USD']])
              / (get('GDP_PPP_OECD_pc_av', envir = asNamespace("gcamreport")) / get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_10USD_25USD']]),
      var = "GDP|PPP [per capita relative to OECD]"
    ) %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))


  GDP_PPP_pc_oecd_share_clean <<- GDP_PPP_pc_oecd_share_clean
  GDP_PPP_pc_growth_clean <<- GDP_PPP_pc_growth_clean
  GDP_PPP_clean <<- GDP_PPP_clean
}


#' get_gdp_mer
#'
#' Retrieves GDP (MER) data and converts units to 10 USD.
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @return `GDP_MER_clean` global variable.
#' @keywords internal econ
#' @importFrom magrittr %>%
#' @export
get_gdp_mer <- function(GCAM_version = 'v8.2') {
  value <- GDP_MER_clean <- NULL

  check_queries('GDP_MER_clean', GCAM_version)

  GDP_MER_clean <-
    check_inf(rgcam::getQuery(prj, "GDP MER by region"),
              dataset_name = "GDP MER by region") %>%
    dplyr::filter(value != 0) %>% # avoid null values present in the data due to the byu
    dplyr::mutate(
      value = value *
        get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_million_billion']] *
        get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_90USD_10USD']],
      var = "GDP|MER"
    ) %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  GDP_MER_clean <<- GDP_MER_clean
}


#' get_goods_trade
#'
#' Compute net exports of all goods measured in monetary quantities: materials-net-export +
#' capital-net-export + energy-net-export
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @return `goods_trade_clean` global variables.
#' @keywords internal econ
#' @importFrom magrittr %>%
#' @export
get_goods_trade <- function(GCAM_version = 'v8.2') {
  goods_trade_clean <- NULL

  check_queries('goods_trade_clean', GCAM_version)

  if (GCAM_version %in% c('v7.0',get('deciles_GCAM_versions', envir = asNamespace("gcamreport"))) & 'National Account' %in% rgcam::listQueries(prj)) {
    goods_trade_clean <-
      check_inf(rgcam::getQuery(prj, "National Account"),
                dataset_name = "National Account") %>%
      dplyr::filter(year != dplyr::if_else(base_year == 2021, 2020, 2021)) %>% # avoid null values present in the data due to the byu
      dplyr::filter(account %in% c('materials-net-export','energy-net-export','capital-net-export')) %>%
      dplyr::mutate(
        value = value *
          get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_million_billion']] *
          get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_90USD_10USD']],
        var = "Trade|Goods [Value]"
      ) %>%
      dplyr::group_by(scenario, region, year, var) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::ungroup() %>%
      dplyr::select(dplyr::all_of(gcamreport::long_columns))
  } else {
    goods_trade_clean <- NULL
    warning("The 'Gross Trade' variable is unavailable in your project. It is only supported from GCAM version 7.0 onwards. If you are using version 7.0 or newer, please ensure the `National Accunt` query is valid and not returning empty results.")
  }

  goods_trade_clean <<- goods_trade_clean
}

#' get_value_added
#'
#' Compute value added by the the aggregated agr + ind + services sectors.
#' Each sector receives 1/3 of the total value added
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @return `value_added_clean` global variables.
#' @keywords internal econ
#' @importFrom magrittr %>%
#' @export
get_value_added <- function(GCAM_version = 'v8.2') {
  value_added_clean <- NULL

  check_queries('value_added_clean', GCAM_version)

  if (GCAM_version %in% c('v7.0',get('deciles_GCAM_versions', envir = asNamespace("gcamreport"))) & 'National Account' %in% rgcam::listQueries(prj)) {
    value_added <-
      check_inf(rgcam::getQuery(prj, "National Account"),
                dataset_name = "National Account") %>%
      dplyr::filter(year != dplyr::if_else(base_year == 2021, 2020, 2021)) %>% # avoid null values present in the data due to the byu
      dplyr::filter(account %in% c('value-added')) %>%
      dplyr::mutate(
        value = value *
          get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_million_billion']] *
          get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_90USD_10USD']] / 3,
        var = 'Value Added|Industry'
      ) %>%
      dplyr::select(dplyr::all_of(gcamreport::long_columns))

    value_added_clean <- rbind(
      value_added, #ind
      value_added %>%
        dplyr::mutate(var = 'Value Added|Agriculture'), #agr
      value_added %>%
        dplyr::mutate(var = 'Value Added|Services') #services
    )
  } else {
    value_added_clean <- NULL
    warning("The 'Value Added' variables are unavailable in your project. They are only supported from GCAM version 7.0 onwards. If you are using version 7.0 or newer, please ensure the `National Accunt` query is valid and not returning empty results.")
  }

  value_added_clean <<- value_added_clean
}


#' get_en_expenditure
#'
#' Computes energy expenditure.as the share of the total expenditure by decile
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @return `energy_expenditure_per_clean` global variable.
#' @keywords internal economy
#' @importFrom magrittr %>%
#' @export
get_en_expenditure <- function(GCAM_version = 'v8.2') {
  energy_expenditure_per_clean <- energy_expenditure_per_w <-
    energy_expenditure_per <- energy_expenditure_per_avR <- NULL

  check_queries('energy_expenditure_per_clean', GCAM_version)

  # fix income
  income <- income_raw %>%
    dplyr::select(-var, -Units) %>%
    dplyr::rename(income = value,
                  `gcam-decile` = `gcam-consumer`) %>%
    # Units: from thous. 1990$ per capita to 1900$ per capita
    dplyr::mutate(income = 1e3 * income) %>%
    # fix South America_Northern
    dplyr::mutate(income = dplyr::if_else(region == 'South America_Northern', income / 20, income)) %>%
    dplyr::mutate(`gcam-decile` = as.numeric(gsub("[^0-9.]", "", `gcam-decile`)))


  # Energy expenditure
  energy_mult <- get(paste('en_multiplier',GCAM_version,sep='_'), envir = asNamespace("gcamreport")) %>%
    dplyr::select(-year)

  energy_expenditure <-
    # energy prices
    check_inf(rgcam::getQuery(prj, "building service costs"),
              dataset_name = "building service costs") %>%
    # Units: from 1975$ to 1990$
    dplyr::mutate(value = value *
                    get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_75USD_10USD']] /
                    get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_90USD_10USD']]) %>%
    # Units: from GJ to EJ (1GJ = 1e9EJ)
    dplyr::mutate(value = get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['GJ_to_EJ']] * value) %>%
    # other
    dplyr::rename(cost = value) %>%
    dplyr::filter(year %in% gcam_years) %>%
    # energy demand
    dplyr::left_join(
      # left_join_error_no_match(
      check_inf(rgcam::getQuery(prj, "building service output by service"),
                dataset_name = "building service output by service") %>%
        dplyr::rename(demand = value) %>%
        tidyr::complete(tidyr::nesting(Units, scenario, region, sector), year = gcam_years, fill = list(demand = 0)),
      by = c('scenario','region','sector','year')
    ) %>%
    # fix NAs in demand (Taiwan resid others coal)
    dplyr::mutate(demand = dplyr::if_else(is.na(demand), 0, demand)) %>%
    # EXPENDITURE
    dplyr::mutate(energy_expenditure = demand * cost) %>%
    dplyr::mutate(Units = '1990$') %>%
    dplyr::select(-Units.x, -Units.y, `gcam-consumer` = sector) %>%
    dplyr::filter(grepl('resid', `gcam-consumer`)) %>%
    dplyr::mutate(`gcam-decile` = as.numeric(gsub("[^0-9.]", "", `gcam-consumer`))) %>%
    dplyr::group_by(scenario, region, `gcam-decile`, year, Units) %>%
    dplyr::summarise(energy_expenditure = sum(energy_expenditure)) %>%
    dplyr::ungroup() %>%
    # compute per capita expenditure
    left_join_error_no_match(
      rgcam::getQuery(prj, "population by region") %>%
        # Units: from thous. to abs
        dplyr::mutate(pop = 1e2 * value) %>%
        dplyr::select(scenario, region, year, pop),
      by = c('scenario','region','year')) %>%
    dplyr::mutate(energy_expenditure = energy_expenditure / pop) %>%
    dplyr::mutate(Units = '1990$cap') %>%
    # add multipliers
    left_join_error_no_match(energy_mult,
                             by = c('region')) %>%
    dplyr::mutate(energy_expenditure = energy_mult * energy_expenditure) %>%
    dplyr::select(scenario, region, year, energy_expenditure, `gcam-decile`, Units) %>%
    # restrict to desired years
    dplyr::filter(year <= final_year.global)


  energy_expenditure_per <- energy_expenditure %>%
    left_join_error_no_match(income,
                             by = c('region','gcam-decile','year','scenario')) %>%
    dplyr::mutate(energy_expenditure_per = 1e2 * energy_expenditure / income) %>%
    dplyr::mutate(var = paste0('Expenditure|Households|Energy|D',`gcam-decile`, ' [Share]')) %>%
    dplyr::select(scenario, region, var, year, value = energy_expenditure_per)

  energy_expenditure_per_avR <- energy_expenditure_per %>%
    dplyr::group_by(scenario, region, year) %>%
    dplyr::summarise(var = 'Expenditure|Households|Energy [Share]',
                     value = mean(value)) %>%
    dplyr::ungroup()

  energy_expenditure_per_w <- energy_expenditure_per %>%
    left_join_error_no_match(tibble::as_tibble(pop_weights),
                             by = c('scenario','region','year')) %>%
    # weighted sum
    dplyr::mutate(en_expenditure_per_weighted = value * share * 100) %>%
    dplyr::group_by(scenario, year, var) %>%
    dplyr::summarise(region = 'World',
                     value = sum(en_expenditure_per_weighted)) %>%
    dplyr::ungroup()


  energy_expenditure_per_clean <- rbind(
    energy_expenditure_per,
    energy_expenditure_per_avR,
    energy_expenditure_per_w
  )

  energy_expenditure_per_clean <<- energy_expenditure_per_clean
}


#' get_food_expenditure
#'
#' Computes food expenditure.as the share of the total expenditure by decile
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @return `food_expenditure_per_clean` global variable.
#' @keywords internal economy
#' @importFrom magrittr %>%
#' @export
get_food_expenditure <- function(GCAM_version = 'v8.2') {
  food_expenditure_per_clean <- food_expenditure_per_w <-
    food_expenditure_per <- food_expenditure_per_avR <- NULL

  check_queries('food_expenditure_per_clean', GCAM_version)


  # fix income
  income <- income_raw %>%
    dplyr::select(-var, -Units) %>%
    dplyr::rename(income = value,
                  `gcam-decile` = `gcam-consumer`) %>%
    # Units: from thous. 1990$ per capita to 1900$ per capita
    dplyr::mutate(income = 1e3 * income) %>%
    # fix South America_Northern
    dplyr::mutate(income = dplyr::if_else(region == 'South America_Northern', income / 20, income)) %>%
    dplyr::mutate(`gcam-decile` = as.numeric(gsub("[^0-9.]", "", `gcam-decile`)))


  # Food expenditure
  food_exp <- get(paste('food_expenditures_average',GCAM_version,sep='_'), envir = asNamespace("gcamreport")) %>%
    dplyr::select(-year)

  # CASE 1: Income groups available
  if ("food demand prices by income group" %in% rgcam::listQueries(prj)) {
    food_expenditure <-
      # food prices
      check_inf(rgcam::getQuery(prj, "food demand prices by income group"),
                dataset_name = "food demand prices by income group") %>%
      # Units: from 2005$ to 2020$ (1990 to 2020 / 0.5575288)
      dplyr::mutate(value = value *
                      get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_05USD_10USD']] /
                      get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_90USD_10USD']]
                    / 0.5575288) %>%
      # Units: from Mcal to Pcal (1Mcal = 1e9Pcal)
      dplyr::mutate(value = value * 1e9) %>%
      # Units: from day to year (1year = 365.25days)
      dplyr::mutate(value = value * 365.25) %>%
      # other
      dplyr::rename(cost = value) %>%
      dplyr::filter(year %in% gcam_years) %>%
      # food demand
      left_join_error_no_match(
        check_inf(rgcam::getQuery(prj, "food demand by income group"),
                  dataset_name = "food demand by income group") %>%
          dplyr::rename(demand = value),
        by = c('scenario','region','gcam-consumer','nodeinput','input','year')
      ) %>%
      # EXPENDITURE
      dplyr::mutate(food_expenditure = demand * cost) %>%
      # sum staples and nonstaples to have the total food expenditure
      dplyr::mutate(`gcam-decile` = as.numeric(gsub("[^0-9.]", "", `gcam-consumer`))) %>%
      dplyr::group_by(scenario, region, `gcam-decile`, year) %>%
      dplyr::summarise(food_expenditure = sum(food_expenditure)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Units = '2020$') %>%
      # compute per capita expenditure
      left_join_error_no_match(
        rgcam::getQuery(prj, "population by region") %>%
          # Units: from thous. to abs
          dplyr::mutate(pop = 1e2 * value) %>%
          dplyr::select(scenario, region, year, pop),
        by = c('scenario','region','year')) %>%
      dplyr::mutate(food_expenditure = food_expenditure / pop) %>%
      dplyr::mutate(Units = '2020$cap') %>%
      # restrict to desired years
      dplyr::filter(year <= final_year.global)

  # CASE 2: Income groups not available
  } else {
    food_expenditure <-
      # food prices
      check_inf(rgcam::getQuery(prj, "food demand prices"),
                dataset_name = "food demand prices") %>%
      # Units: from 2005$ to 2020$ (1990 to 2020 / 0.5575288)
      dplyr::mutate(value = value *
                      get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_05USD_10USD']] /
                      get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_90USD_10USD']]
                    / 0.5575288) %>%
      # Units: from Mcal to Pcal (1Mcal = 1e9Pcal)
      dplyr::mutate(value = value * 1e9) %>%
      # Units: from day to year (1year = 365.25days)
      dplyr::mutate(value = value * 365.25) %>%
      # other
      dplyr::rename(cost = value) %>%
      dplyr::filter(year %in% gcam_years) %>%
      # food demand
      left_join_error_no_match(
        check_inf(rgcam::getQuery(prj, "food demand"),
                  dataset_name = "food demand") %>%
          dplyr::rename(demand = value),
        by = c('scenario','region','gcam-consumer','nodeinput','input','year')
      ) %>%
      # EXPENDITURE
      dplyr::mutate(food_expenditure = demand * cost,
                    `gcam-decile` = '') %>%
      # sum staples and nonstaples to have the total food expenditure
      dplyr::group_by(scenario, region, `gcam-decile`, year) %>%
      dplyr::summarise(food_expenditure = sum(food_expenditure)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Units = '2020$') %>%
      # compute per capita expenditure
      left_join_error_no_match(
        rgcam::getQuery(prj, "population by region") %>%
          # Units: from thous. to abs
          dplyr::mutate(pop = 1e2 * value) %>%
          dplyr::select(scenario, region, year, pop),
        by = c('scenario','region','year')) %>%
      dplyr::mutate(food_expenditure = food_expenditure / pop) %>%
      dplyr::mutate(Units = '2020$cap') %>%
      # restrict to desired years
      dplyr::filter(year <= final_year.global)

    income <- income %>%
      dplyr::group_by(region, year, scenario) %>%
      dplyr::summarise(`gcam-decile` = '',
                       income = mean(income)) %>%
      dplyr::ungroup()
  }

  # compute the food multipliers
  food_mult <- food_expenditure %>%
    dplyr::filter(year == base_year_p) %>%
    dplyr::group_by(scenario, region) %>%
    dplyr::summarise(gcam_av_food_exp = mean(food_expenditure)) %>%
    dplyr::ungroup() %>%
    left_join_error_no_match(food_exp,
                             by = c('region')) %>%
    dplyr::mutate(food_mult = av_food_exp / gcam_av_food_exp) %>%
    dplyr::select(scenario, region, food_mult)

  # add multipliers
  food_expenditure <- food_expenditure %>%
    left_join_error_no_match(food_mult,
                             by = c('scenario', 'region')) %>%
    dplyr::mutate(food_expenditure_mult = food_mult * food_expenditure) %>%
    dplyr::select(scenario, region, year, food_expenditure, food_expenditure_mult, food_mult, `gcam-decile`, Units)


  food_expenditure_per <- food_expenditure %>%
    left_join_error_no_match(income %>%
                               dplyr::mutate(income = income / 0.5575288),
                             by = c('region','gcam-decile','year','scenario')) %>%
    dplyr::mutate(food_expenditure_per = 100 * food_expenditure_mult / income) %>%
    dplyr::mutate(var = paste0('Expenditure|Households|Food|D',`gcam-decile`, ' [Share]')) %>%
    dplyr::select(scenario, region, var, year, value = food_expenditure_per)

  food_expenditure_per_avR <- food_expenditure_per %>%
    dplyr::group_by(scenario, region, year) %>%
    dplyr::summarise(var = 'Expenditure|Households|Food [Share]',
                     value = mean(value)) %>%
    dplyr::ungroup()

  food_expenditure_per_w <- food_expenditure_per %>%
    left_join_error_no_match(tibble::as_tibble(pop_weights),
                             by = c('scenario','region','year')) %>%
    # weighted sum
    dplyr::mutate(food_expenditure_per_weighted = value * share * 100) %>%
    dplyr::group_by(scenario, year, var) %>%
    dplyr::summarise(region = 'World',
                     value = sum(food_expenditure_per_weighted)) %>%
    dplyr::ungroup()


  food_expenditure_per_clean <- rbind(
    food_expenditure_per,
    food_expenditure_per_avR,
    food_expenditure_per_w
  ) %>%
    dplyr::filter(var != 'Expenditure|Households|Food|D [Share]')


  food_expenditure_per_clean <<- food_expenditure_per_clean
}



#' get_expenditure
#'
#' Computes food + energy expenditure.
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @return `expenditure_per_clean` global variable.
#' @keywords internal economy
#' @importFrom magrittr %>%
#' @export
get_expenditure <- function(GCAM_version = 'v8.2') {
  value <- expenditure_per_clean <- NULL

  check_queries('expenditure_per_clean', GCAM_version)

  expenditure_per_clean <- rbind(
    food_expenditure_per_clean,
    energy_expenditure_per_clean
  ) %>%
    dplyr::mutate(decile = stringr::str_extract(var, "\\|D[0-9]+")) %>%
    dplyr::mutate(decile = tidyr::replace_na(decile, "")) %>%
    dplyr::group_by(scenario, region, decile, year) %>%
    dplyr::reframe(value = sum(value),
                   var = paste0('Expenditure|Households', decile, ' [Share]')) %>%
    dplyr::distinct() %>%
    dplyr::select(-decile)

  expenditure_per_clean <<- expenditure_per_clean
}


#' get_capital_stock
#'
#' Retrieves Capital Stock data and converts units to 2010 USD.
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @return `capital_stock_clean` global variable.
#' @keywords internal economy
#' @importFrom magrittr %>%
#' @export
get_capital_stock <- function(GCAM_version = 'v8.2') {
  value <- var <- capital_stock_clean <- NULL

  check_queries('capital_stock_clean', GCAM_version)

  if (GCAM_version %in% c('v7.0',get('deciles_GCAM_versions', envir = asNamespace("gcamreport"))) & 'National Account' %in% rgcam::listQueries(prj)) {
    capital_stock_clean <-
      check_inf(rgcam::getQuery(prj, "National Account"),
                dataset_name = "National Account") %>%
      dplyr::filter(year != dplyr::if_else(base_year == 2021, 2020, 2021)) %>% # avoid null values present in the data due to the byu
      dplyr::filter(account == 'capital-stock') %>%
      dplyr::mutate(var = 'Capital Stock',
                    # million 1990$ to billion 2010$
                    value = value * get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_90USD_10USD']] *
                      get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_million_billion']]) %>%
      dplyr::select(dplyr::all_of(gcamreport::long_columns))
  } else {
    capital_stock_clean <- NULL
    warning("The 'Capital Stock' variable is unavailable in your project. It is only supported from GCAM version 7.0 onwards. If you are using version 7.0 or newer, please ensure the `National Accunt` query is valid and not returning empty results.")
  }

  capital_stock_clean <<- capital_stock_clean
}

#' get_capital_formation
#'
#' Computes Capital Formation data.
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @return `capital_formation_clean` global variable.
#' @keywords internal economy
#' @importFrom magrittr %>%
#' @export
get_capital_formation <- function(GCAM_version = 'v8.2') {
  value <- var <- capital_formation_clean <- NULL

  check_queries('capital_formation_clean', GCAM_version)

  if (GCAM_version %in% c('v7.0',get('deciles_GCAM_versions', envir = asNamespace("gcamreport"))) & 'National Account' %in% rgcam::listQueries(prj)) {
    capital_formation_clean <-
      capital_stock_clean %>%
      dplyr::arrange(scenario, region, var, year) %>%
      dplyr::group_by(scenario, region, var) %>%
      dplyr::mutate(net_addition = value - dplyr::lag(value)) %>%
      dplyr::select(-value) %>%
      dplyr::rename(value = net_addition) %>%
      dplyr::mutate(var = 'Capital Formation') %>%
      dplyr::select(dplyr::all_of(gcamreport::long_columns))

  } else {
    capital_stock_clean <- NULL
    warning("The 'Capital Formation' variable is unavailable in your project. It is only supported from GCAM version 7.0 onwards. If you are using version 7.0 or newer, please ensure the `National Accunt` query is valid and not returning empty results.")
  }

  capital_formation_clean <<- capital_formation_clean
}


#' get_food_availability
#'
#' Computes Food availability
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @return `food_availability_clean` global variable.
#' @keywords internal food
#' @importFrom magrittr %>%
#' @export
get_food_availability <- function(GCAM_version = 'v8.2') {
  value <- food_availability_clean <- NULL

  check_queries('food_availability_clean', GCAM_version)

  # GCAM does not track consumer waste, so food availability is food intake adjusted by the food waste parameter (exogenous)
  food_availability_pre <- check_inf(rgcam::getQuery(prj, 'food consumption by type (specific)'),
                                     dataset_name = "food consumption by type (specific)") %>%
    dplyr::filter(year <= final_year.global, year >= 1990) %>%
    # if "subsector...4" is a colnum name, change it to "subsector". Otherwise, keep going
    {
      if ("subsector...4" %in% names(.)) {
        dplyr::rename(., subsector = `subsector...4`)
      } else {
        .
      }
    } %>%
    left_join_strict(get(paste('food_intake_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")),
                     by = c("subsector","technology"), mapping = paste('food_intake_map',GCAM_version,sep='_'), multiple = "all") %>%
    dplyr::filter(var != 'NoReported', !is.na(var)) %>%
    filter_variables() %>%
    dplyr::distinct() %>%
    dplyr::rename(GCAM_commodity = technology) %>%
    dplyr::mutate(value = value * unit_conv) %>%
    dplyr::mutate(var = gsub('Food Intake','Food Availability',var)) %>%
    # try to detect SSP scenarios in the scenarios' names. By default SSP2
    dplyr::mutate(ssp = stringr::str_extract(scenario,'SSP1|SSP2|SSP3|SSP4|SSP5')) %>%
    dplyr::mutate(ssp = dplyr::if_else(is.na(ssp), 'SSP2', ssp)) %>%
    dplyr::filter(year > 2005) %>%
    # merge with L100.AgMIP_FoodWaste_Share_Pathway_SSP
    left_join_error_no_match(get(paste('L100.AgMIP_FoodWaste_Share_Pathway_SSP',GCAM_version,sep='_'), envir = asNamespace("gcamreport")),
                             by = c('region','ssp','year','GCAM_commodity')) %>%
    # calculate food availability
    dplyr::mutate(value = value / (1-WasteShare)) %>%
    dplyr::group_by(scenario, region, var, year) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    # from Pcal (region/yr) to kcal/cap/day (1Pcal = 1e12kcal)
    left_join_strict(population_clean %>%
                       dplyr::rename(pop = value) %>%
                       dplyr::select(-var),
                     by = c("scenario", "region", "year"),
                     multiple = "all") %>%
    dplyr::mutate(value = dplyr::if_else(!is.na(value), value * 1e6 / pop / 365.25, 0)) # pop in million


  # Compute total by var
  food_availability_clean <- food_availability_pre %>%
    dplyr::group_by(scenario, region, var, year) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  # World value: pop weighted average
  food_availability_clean_w <- food_availability_pre %>%
    # add regional weights (World = 1)
    left_join_strict(pop_weights, by = c('scenario','region','year')) %>%
    # compute World weighted average
    dplyr::mutate(value = value * share) %>%
    dplyr::group_by(scenario, var, year) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::mutate(region = 'World') %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  food_availability_clean <- rbind(
    food_availability_clean,
    food_availability_clean_w
  )

  food_availability_clean <<- food_availability_clean
}

#' get_food_demand
#'
#' Computes Food demand
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @return `food_demand_clean` global variable.
#' @keywords internal food
#' @importFrom magrittr %>%
#' @export
get_food_demand <- function(GCAM_version = 'v8.2') {
  value <- food_demand_clean <- NULL

  food_demand_clean <- food_availability_clean %>%
    dplyr::mutate(var = gsub('Food Availability','Food Demand',var))

  food_demand_clean <<- food_demand_clean
}

#' get_food_intake
#'
#' Computes Food intake
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @return `food_intake_clean` global variable.
#' @keywords internal food
#' @importFrom magrittr %>%
#' @export
get_food_intake <- function(GCAM_version = 'v8.2') {
  value <- food_intake_clean <- food_intake_clean_w <- NULL

  check_queries('food_intake_clean', GCAM_version)

  food_intake_pre <-
    check_inf(rgcam::getQuery(prj, 'food consumption by type (specific)'),
              dataset_name = "food consumption by type (specific)") %>%
    dplyr::filter(year <= final_year.global, year >= 1990) %>%
    # if "subsector...4" is a colnum name, change it to "subsector". Otherwise, keep going
    {
      if ("subsector...4" %in% names(.)) {
        dplyr::rename(., subsector = `subsector...4`)
      } else {
        .
      }
    } %>%
    left_join_strict(get(paste('food_intake_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")),
                     by = c("subsector","technology"), mapping = paste('food_intake_map',GCAM_version,sep='_'), multiple = "all") %>%
    dplyr::filter(var != 'NoReported', !is.na(var)) %>%
    filter_variables() %>%
    dplyr::mutate(value = value * unit_conv) %>%
    dplyr::group_by(scenario, region, var, GCAM_commodity = technology, year) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    # from Pcal (region/yr) to kcal/cap/day (1Pcal = 1e12kcal)
    left_join_strict(population_clean %>%
                       dplyr::rename(pop = value) %>%
                       dplyr::select(-var),
                     by = c("scenario", "region", "year"),
                     multiple = "all") %>%
    dplyr::filter(var != 'NoReported', !is.na(var)) %>%
    filter_variables() %>%
    dplyr::mutate(value = value * 1e6 / pop / 365.25) # pop in million

  # Compute total by var
  food_intake_clean <- food_intake_pre %>%
    dplyr::group_by(scenario, region, var, year) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  # World value: pop weighted average
  food_intake_clean_w <- food_intake_pre %>%
    # add regional weights (World = 1)
    left_join_strict(pop_weights, by = c('scenario','region','year')) %>%
    # compute World weighted average
    dplyr::mutate(value = value * share) %>%
    dplyr::group_by(scenario, var, year) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::mutate(region = 'World') %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  food_intake_clean <- rbind(
    food_intake_clean,
    food_intake_clean_w
  )

  food_intake_clean <<- food_intake_clean

}


#' get_forestry
#'
#' Computes Forestry production and demand
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @return `forestry_demand_clean` and `forestry_production_clean` global variables.
#' @keywords internal forestry
#' @importFrom magrittr %>%
#' @export
get_forestry <- function(GCAM_version = 'v8.2') {
  value <- forestry_demand_clean <- forestry_production_clean <- NULL

  check_queries('forestry_demand_clean', GCAM_version)
  check_queries('forestry_production_clean', GCAM_version)

  ## INDUSTRIAL ROUNDWOOD
  # demand = domestic + imports
  forestry_demand_indroundwood <-
    check_inf(rgcam::getQuery(prj, "inputs by tech"),
              dataset_name = "inputs by tech") %>%
    dplyr::filter(grepl('regional industrial_roundwood', sector)) %>%
    dplyr::group_by(scenario, region, year) %>%
    # billion m3 to million m3
    dplyr::summarise(value = sum(value) /
                       get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_million_billion']]) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(var = 'Forestry Demand|Roundwood|Industrial Roundwood') %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  # production = domestic + exports
  forestry_exports <-
    check_inf(rgcam::getQuery(prj, "inputs by tech"),
              dataset_name = "inputs by tech") %>%
    dplyr::filter(stringr::str_detect(technology, stringr::regex("traded industrial_roundwood", ignore_case = TRUE))) %>%
    dplyr::select(-sector,-technology,-input) %>%
    dplyr::mutate(
      region = stringr::str_split_fixed(subsector, " traded industrial_roundwood", 2)[, 1],
      subsector = 'traded industrial_roundwood'
    )

  forestry_domestic <-
    check_inf(rgcam::getQuery(prj, "inputs by tech"),
              dataset_name = "inputs by tech") %>%
    dplyr::filter(grepl('domestic industrial_roundwood', subsector)) %>%
    dplyr::select(-sector,-technology,-input)


  forestry_production_indroundwood <- rbind(
    forestry_domestic,
    forestry_exports
  ) %>%
    dplyr::group_by(scenario, region, year) %>%
    # billion m3 to million m3
    dplyr::summarise(value = sum(value) /
                       get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_million_billion']]) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(var = 'Forestry Production|Roundwood|Industrial Roundwood') %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))


  ## WOOD FUEL
  # Wood fuel is assumed to be forestry residues (in EJ)
  check_inf(rgcam::getQuery(prj, "residue biomass production"),
            dataset_name = "residue biomass production") %>%
    dplyr::filter(sector == "Forest") %>%
    dplyr::select(scenario, region, year, WoodFuel_EJ = value) ->
    ResidueBio_R_Y

  # the goal here is to get Wood fuel in M3 historical years
  get(paste('WoodFuel_IndRoundwood_ratio',GCAM_version,sep='_'), envir = asNamespace("gcamreport")) %>%
    dplyr::filter(year %in% unique(ResidueBio_R_Y$year)) %>%
    # we can calculate WoodFuel_M3 using Ratio_WoodFuel_IndRoundwood
    # but need Industrial_Roundwood results in M3
    dplyr::select(region, year, WoodFuel_m3 = `Wood fuel`) ->
    WoodFuel_m3

  # join M3 and EJ to compute ratio and apply to future periods
  ResidueBio_R_Y %>%
    dplyr::left_join(WoodFuel_m3) %>%
    dplyr::mutate(WoodFuel_Mm3 = WoodFuel_m3 / 10^6) %>%
    dplyr::mutate(Ratio_M3_EJ = WoodFuel_Mm3 / WoodFuel_EJ) %>%
    dplyr::group_by(region) %>%
    tidyr::fill(Ratio_M3_EJ, .direction = "down") %>%
    # tmp adjustment: adjust China for future time periods
    dplyr::mutate(Ratio_M3_EJ = dplyr::if_else(region == "China" & year >= 2025, Ratio_M3_EJ /10, Ratio_M3_EJ)) %>%
    dplyr::mutate(WoodFuel_Mm3 = WoodFuel_EJ * Ratio_M3_EJ) %>%
    dplyr::ungroup() %>%
    dplyr::select(scenario, region, year, value = WoodFuel_Mm3) %>%
    dplyr::mutate(var = 'Forestry Production|Roundwood|Wood Fuel') ->
    forestry_production_woodfuel


  # aggregate
  forestry_production <- rbind(
    forestry_production_woodfuel,
    forestry_production_indroundwood
  )

  forestry_production_clean <- rbind(
    forestry_production,
    forestry_production %>%
      dplyr::mutate(var = 'Forestry Production|Roundwood')
  ) %>%
    dplyr::group_by(scenario, region, year, var) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))



  forestry_demand_woodfuel <- forestry_production_woodfuel %>%
    dplyr::mutate(var = 'Forestry Demand|Roundwood|Wood Fuel')

  forestry_demand <- rbind(
    forestry_demand_woodfuel,
    forestry_demand_indroundwood
  )

  forestry_demand_clean <- rbind(
    forestry_demand,
    forestry_demand %>%
      dplyr::mutate(var = 'Forestry Demand|Roundwood')
  ) %>%
    dplyr::group_by(scenario, region, year, var) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))


  forestry_demand_clean <<- forestry_demand_clean
  forestry_production_clean <<- forestry_production_clean

}


#' get_ag_trade
#'
#' Computes Agricultural trade
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @return `ag_trade` global variable.
#' @keywords internal food
#' @importFrom magrittr %>%
#' @export
get_ag_trade <- function(GCAM_version = 'v8.2') {
  value <- ag_trade <- ag_trade_exports <- ag_trade_imports <- NULL

  check_queries('ag_trade', GCAM_version)

  ag_trade_exports <-
    check_inf(rgcam::getQuery(prj, "ag export to the world center (USA) (Intl. Armington competition)"),
              dataset_name = "ag export to the world center (USA) (Intl. Armington competition)") %>%
    dplyr::mutate(region = sub(" traded.*", "", subsector)) %>%
    left_join_strict(get(paste('trade_ag',GCAM_version,sep='_'), envir = asNamespace("gcamreport")),
                     by = c("sector"), mapping = paste('trade_ag',GCAM_version,sep='_'), multiple = "all") %>%
    dplyr::filter(var != 'NoReported', !is.na(var)) %>%
    # add water content data
    left_join_strict(get(paste('water_content'), envir = asNamespace("gcamreport")) %>%
                       dplyr::mutate(input = GCAM_commodity),
                     by = 'input') %>%
    # units to annual million t DM
    dplyr::mutate(value = value * (as.numeric(unit_conv) - as.numeric(mean_water_content))) %>%
    dplyr::group_by(scenario, region, var, year) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    # billion m3 to million m3 for Trade|Forestry
    dplyr::mutate(value = dplyr::if_else(grepl("Trade\\|Forestry", var),
                                         value / get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_million_billion']],
                                         value)) %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))


  ag_trade_imports <-
    check_inf(rgcam::getQuery(prj, "ag import vs. domestic supply (Regional Armington competition)"),
              dataset_name = "ag import vs. domestic supply (Regional Armington competition)") %>%
    dplyr::filter(grepl("imported",subsector)) %>%
    dplyr::select(-sector) %>%
    dplyr::rename(sector = input) %>%
    left_join_strict(get(paste('trade_ag',GCAM_version,sep='_'), envir = asNamespace("gcamreport")),
                     by = c("sector"), mapping = paste('trade_ag',GCAM_version,sep='_'), multiple = "all") %>%
    dplyr::filter(var != 'NoReported', !is.na(var)) %>%
    filter_variables() %>%
    # add water content data
    left_join_strict(get(paste('food_items_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")) %>%
                       dplyr::mutate(regional_item = stringr::str_replace(regional_item, 'regional', 'traded')) %>%
                       dplyr::rename(sector = regional_item),
                     by = 'sector') %>%
    left_join_strict(get(paste('water_content'), envir = asNamespace("gcamreport")) %>%
                       dplyr::mutate(item = GCAM_commodity),
                     by = 'item') %>%
    # units to annual million t DM
    dplyr::mutate(value = value * (as.numeric(unit_conv) - as.numeric(mean_water_content))) %>%
    dplyr::group_by(scenario, region, var, year) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    # billion m3 to million m3 for Trade|Forestry
    dplyr::mutate(value = dplyr::if_else(grepl("Trade\\|Forestry", var),
                                         value / get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_million_billion']],
                                         value)) %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  ag_trade <- merge(ag_trade_exports %>%
                      dplyr::rename(exp = value),
                    ag_trade_imports %>%
                      dplyr::rename(imp = value),
                    by = c('scenario','region','year','var'),
                    all = T) %>%
    dplyr::filter(year > 1975) %>%
    dplyr::mutate(exp = dplyr::if_else(is.na(exp), 0, exp),
                  imp = dplyr::if_else(is.na(imp), 0, imp)) %>%
    dplyr::group_by(scenario, region, var, year) %>%
    dplyr::mutate(value = exp - imp) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  ag_trade <<- ag_trade
}

#' get_fert_consumption
#'
#' Computes Fertilizer consumption
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @return `fert_consumption_clean` global variable.
#' @keywords internal fertilizer
#' @importFrom magrittr %>%
#' @export
get_fert_consumption <- function(GCAM_version = 'v8.2') {
  value <- fert_consumption_clean <- NULL

  check_queries('fert_consumption_clean', GCAM_version)

  fert_consumption_clean <-
    check_inf(rgcam::getQuery(prj, "fertilizer consumption by region"),
              dataset_name = "fertilizer consumption by region") %>%
    dplyr::left_join(get(paste('fertilizer_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")),
                     by = "input", multiple = "all") %>%
    # 1Mt = 1Tg
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  fert_consumption_clean <<- fert_consumption_clean

}


# Climate and emissions
# ==============================================================================================
#' get_forcing
#'
#' Retrieves the global forcing query.
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @return `forcing_clean` global variable.
#' @keywords internal forcing
#' @importFrom magrittr %>%
#' @export
get_forcing <- function(GCAM_version = 'v8.2') {
  year <- forcing_clean <- NULL

  check_queries('forcing_clean', GCAM_version)

  forcing_clean <-
    check_inf(rgcam::getQuery(prj, "total climate forcing"),
              dataset_name = "total climate forcing") %>%
    dplyr::filter(year %in% gcam_years) %>%
    dplyr::mutate(var = "Forcing", region = "World") %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  forcing_clean <<- forcing_clean
}


#' get_temperature
#'
#' Retrieves the global mean temperature query.
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @return `global_temp_clean` global variable.
#' @keywords internal temperature
#' @importFrom magrittr %>%
#' @export
get_temperature <- function(GCAM_version = 'v8.2') {
  year <- global_temp_clean <- NULL

  check_queries('global_temp_clean', GCAM_version)

  global_temp_clean <-
    check_inf(rgcam::getQuery(prj, "global mean temperature"),
              dataset_name = "global mean temperature") %>%
    dplyr::filter(year %in% gcam_years) %>%
    dplyr::mutate(var = "Temperature|Global Mean", region = "World") %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  global_temp_clean <<- global_temp_clean
}


#' get_co2_concentration
#'
#' Retrieves the global CO2 concentration query.
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @return `co2_concentration_clean` global variable.
#' @keywords internal co2
#' @importFrom magrittr %>%
#' @export
get_co2_concentration <- function(GCAM_version = 'v8.2') {
  year <- co2_concentration_clean <- NULL

  check_queries('co2_concentration_clean', GCAM_version)

  co2_concentration_clean <-
    check_inf(rgcam::getQuery(prj, "CO2 concentrations"),
              dataset_name = "CO2 concentrations") %>%
    dplyr::filter(year %in% gcam_years) %>%
    dplyr::mutate(var = "Concentration|CO2", region = "World") %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  co2_concentration_clean <<- co2_concentration_clean
}

#' get_co2_ets
#'
#' Get World's CO2 ETS emissions query.
#' @keywords internal co2
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @return `co2_ets_byreg` and `co2_ets_bysec` global variables
#' @importFrom magrittr %>%
#' @export
get_co2_ets <- function(GCAM_version = 'v8.2') {
  ghg <- value <- year <- unit_conv <- scenario <- region <- var <-
    co2_ets_bysec <- co2_ets_byreg <- NULL

  check_queries("co2_ets_bysec", GCAM_version)
  check_queries("co2_ets_byreg", GCAM_version)

  var_fun_map <- get(paste('var_fun_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))
  queryItemReg <- var_fun_map[var_fun_map$name == "co2_ets_byreg", "queries"][[1]][1]
  queryItemSec <- var_fun_map[var_fun_map$name == "co2_ets_bysec", "queries"][[1]][1]

  co2_ets_byreg <-
    tibble::as_tibble(check_inf(rgcam::getQuery(prj, queryItemReg), dataset_name = queryItemReg)) %>%
    dplyr::filter(ghg == "CO2_ETS") %>%
    # change units to CO2 equivalent and set the variable
    dplyr::mutate(
      value = value * get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[["CO2_equivalent"]],
      var = "Emissions|CO2_ETS|Energy and Industrial Processes"
    ) %>%
    dplyr::select(all_of(gcamreport::long_columns))

  co2_ets_bysec <-
    tibble::as_tibble(check_inf(rgcam::getQuery(prj, queryItemSec), dataset_name = queryItemSec)) %>%
    dplyr::filter(ghg == "CO2_ETS") %>%
    dplyr::group_by(scenario, region, sector, ghg, year) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    # change units to CO2 equivalent and group by sector
    dplyr::left_join(get(paste('co2_ets_sector_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")), by = "sector", multiple = "all") %>%
    dplyr::mutate(value = value * unit_conv) %>%
    dplyr::group_by(scenario, region, year, var) %>% #
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(all_of(gcamreport::long_columns))

  co2_ets_byreg <<- co2_ets_byreg
  co2_ets_bysec <<- co2_ets_bysec
}



# Get CO2 emissions by tech, to break out ships vs rail vs aviation
# and to get Emissions|CO2|Energy| Coal vs Gas vs Oil.
# Must create CO2 emissions by tech (no bio) output first to be consistent. There is no query for this
# Instead, find the difference in emissions to match the values from the by sector (no bio) query

# Apply bio negative emissions by joining by sector and by sector (no bio) and finding difference in emissions

#' get_nonbio_tmp
#'
#' Retrieves the non-bio CO2 emissions query by sector.
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @return `nonbio_diff` global variable.
#' @keywords internal co2
#' @importFrom magrittr %>%
#' @export
get_nonbio_tmp <- function(GCAM_version = 'v8.2') {
  value.y <- value.x <- queryItem1 <- queryItem2 <-
    by_sector <- by_sector_no_bio <- nonbio_diff <- NULL

  check_queries("nonbio_diff", GCAM_version)

  var_fun_map <- get(paste('var_fun_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))
  queryItem1 <- var_fun_map[var_fun_map$name == "nonbio_diff", "queries"][[1]][1]
  queryItem2 <- var_fun_map[var_fun_map$name == "nonbio_diff", "queries"][[1]][2]

  # More closely align sectors in the original query with those in the no bio query
  by_sector <- check_inf(rgcam::getQuery(prj, queryItem1), dataset_name = queryItem1) %>%
    dplyr::mutate(ghg = 'CO2',
                  sector = dplyr::case_when(grepl("elec_", sector) & !grepl("CCS", sector) ~ "electricity",
                                            .default = sector)) %>%
    dplyr::group_by(across(c(-value))) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup()

  by_sector_no_bio <- check_inf(rgcam::getQuery(prj, queryItem2), dataset_name = queryItem2)

  # NOTE: verify that the sectors which don't appear in the (no bio) query are expected to be missing,
  # i.e. negative biomass emissions sectors which are yet to be distributed by sector
  nonbio_diff <-
    by_sector %>%
    # dplyr::left_join because we can not control queries matching
    dplyr::full_join(by_sector_no_bio,
                     by = c("region", "scenario", "year", "sector", "Units")) %>%
    tidyr::replace_na(list(value.x = 0, value.y = 0)) %>%
    # Convert to (no bio) by calculating the difference by sector
    dplyr::mutate(
      diff = value.y - value.x
    ) %>%
    dplyr::select(-value.x, -value.y)


  nonbio_diff <<- nonbio_diff
}


#' get_co2_emiss
#'
#' Retrieves the non-bio CO2 emissions query by sector, subsector, and technology.
#' Emissions are scaled to the no bio sector within the get_co2_emiss function.
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @return `co2_emiss` global variable.
#' @keywords internal co2 tmp
#' @importFrom magrittr %>%
#' @export
get_co2_emiss <- function(GCAM_version = 'v8.2') {
  var <- value <- unit_conv <- scenario <- region <- year <-
    queryItem1 <- co2_emiss <- ghg <- technology <- subsector <- NULL

  check_queries("co2_emiss", GCAM_version)

  var_fun_map <- get(paste('var_fun_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))
  queryItem1 <- var_fun_map[var_fun_map$name == "co2_emiss", "queries"][[1]][1]
  queryItem2 <- var_fun_map[var_fun_map$name == "co2_emiss", "queries"][[1]][2]
  queryItem3 <- var_fun_map[var_fun_map$name == "co2_emiss", "queries"][[1]][3] # to do the check


  # Add the no bio difference by sector
  tmp <-
    check_inf(rgcam::getQuery(prj, queryItem1), dataset_name = queryItem1) %>%
    dplyr::filter(year %in% years_in_prj) %>%
    dplyr::mutate(ghg = 'CO2') %>%
    dplyr::mutate(sector_orig = sector) %>%
    dplyr::mutate(sector = dplyr::case_when(grepl("elec_", sector) & !grepl("CCS", sector) ~ "electricity",
                                            .default = sector)) %>%
    dplyr::full_join(nonbio_diff %>%
                       dplyr::select(-ghg),
                     by = c("region", "scenario", "year", "sector", "Units")) %>%
    tidyr::replace_na(list(value = 0, diff = 0)) %>%
    dplyr::group_by(scenario, sector) %>%
    # For sectors where the no bio query returns a small value, whereas the normal query has no value,
    # make the subsector and technology the first types present for other data, so that mapping matches
    dplyr::mutate(subsector = ifelse(is.na(subsector), unique(subsector)[1], subsector)) %>%
    dplyr::mutate(technology = ifelse(is.na(technology), unique(technology)[1], technology)) %>%
    dplyr::group_by(scenario, region, sector, year) %>%
    # ensure that negative bio emissions not applied to purely fossil techs like coal and NG
    dplyr::mutate(diff = ifelse(grepl("coal", technology) & sum(!grepl("coal", technology)) > 0, 0, diff)) %>%
    dplyr::mutate(diff = ifelse(grepl("NG", technology) & sum(!grepl("NG", technology)) > 0, 0, diff)) %>%
    # Distribute the emissions difference based on the tech's share of emissions within the sector
    dplyr::mutate(diff_by_share = dplyr::case_when(sum(diff != 0) > 0 ~ diff * value / (sum(value[diff != 0])),
                                                   .default = diff)) %>%
    # Correct ones which became NaN due to sum of value being zero
    dplyr::mutate(diff_by_share = ifelse(is.na(diff_by_share), diff / sum(diff != 0), diff_by_share)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(value = value + diff_by_share) %>%
    dplyr::mutate(sector = ifelse(!is.na(sector_orig), sector_orig, sector)) %>%
    dplyr::select(-diff, -diff_by_share, -sector_orig)

  # gather deciles if necessary
  if(GCAM_version %in% get('deciles_GCAM_versions', envir = asNamespace("gcamreport"))) {
    tmp <- tmp %>%
      tidyr::separate(sector, into = c("sector", "decile"), sep = "_d", extra = "merge", fill = "right") %>%
      dplyr::group_by(Units, scenario, region, sector, subsector, technology, year, ghg) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::ungroup()
  }

  # CO2 emissions by technology
  co2_emiss_tech <-
    tmp %>%
    left_join_strict(get(paste('co2_tech_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")),
                     by = c("sector", "subsector", "technology"),
                     mapping = paste('co2_tech_map',GCAM_version,sep='_'), multiple = "all") %>%
    dplyr::filter(var != 'NoReported', !is.na(var)) %>%
    filter_variables(extra = c("Emissions|CO2|Energy and Industrial Processes",
                               "Emissions|CO2|Energy|Demand|Industry",
                               "Emissions|CO2|Energy|Demand|Transportation",
                               "Emissions|CO2|Energy|Demand|Residential and Commercial",
                               "Emissions|CO2|Energy|Supply",
                               "Emissions|CO2_ETS|Energy and Industrial Processes",
                               "Emissions|CO2_ETS|Energy|Demand|Industry",
                               "Emissions|CO2_ETS|Energy|Demand|Transportation",
                               "Emissions|CO2_ETS|Energy|Demand|Residential and Commercial",
                               "Emissions|CO2_ETS|Energy|Supply",
                               available_variables(F, GCAM_version)[grepl("^Gross Emissions", available_variables(F, GCAM_version))]
    )) %>%
    dplyr::mutate(value = value * unit_conv) %>%
    dplyr::group_by(scenario, region, year, var) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  # CO2 emissions by resource production
  co2_emiss_resource <-
    check_inf(rgcam::getQuery(prj, queryItem2), dataset_name = queryItem2) %>%
    left_join_strict(get(paste('co2_resource_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")),
                     by = c("resource", "subresource", "ghg"),
                     mapping = paste('co2_resource_map',GCAM_version,sep='_'), multiple = "all") %>%
    dplyr::filter(var != 'NoReported', !is.na(var)) %>%
    filter_variables(extra = c("Emissions|CO2|Energy and Industrial Processes",
                               "Emissions|CO2|Energy|Demand|Industry",
                               "Emissions|CO2|Energy|Demand|Transportation",
                               "Emissions|CO2|Energy|Demand|Residential and Commercial",
                               "Emissions|CO2|Energy|Supply",
                               "Emissions|CO2_ETS|Energy and Industrial Processes",
                               "Emissions|CO2_ETS|Energy|Demand|Industry",
                               "Emissions|CO2_ETS|Energy|Demand|Transportation",
                               "Emissions|CO2_ETS|Energy|Demand|Residential and Commercial",
                               "Emissions|CO2_ETS|Energy|Supply",
                               available_variables(F, GCAM_version)[grepl("^Gross Emissions", available_variables(F, GCAM_version))]
    )) %>%
    dplyr::mutate(value = value * unit_conv) %>%
    dplyr::group_by(scenario, region, year, var) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  # Total CO2 emissions
  co2_emiss <- rbind(
    co2_emiss_tech,
    co2_emiss_resource
  ) %>%
    dplyr::group_by(scenario, region, year, var) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  # Check Total Emissions|CO2 matches the output of CO2 emissions by region query
  avail_variables <- available_variables(print = F, GCAM_version = GCAM_version)
  if (grepl("^All$", desired_variables.global)[1] | all(avail_variables[grep('^Emissions',avail_variables)] %in% desired_variables.global)) {
    check <-
      check_inf(rgcam::getQuery(prj, queryItem3), dataset_name = queryItem3) %>%
      dplyr::mutate(value = value *
                      get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['CO2_equivalent']]) %>%
      dplyr::group_by(dplyr::across(-c(Units, value))) %>%
      dplyr::summarise(value = sum(value, na.rm = T)) %>%
      dplyr::ungroup() %>%
      dplyr::filter(year %in% available_reporting_years) %>%
      left_join_error_no_match(co2_emiss %>%
                                 dplyr::filter(var == 'Emissions|CO2'),
                               by = c('scenario','region','year')) %>%
      dplyr::mutate(diff = value.x - value.y,
                    diff_pct = diff/value.x * 100)

    if (!max(abs(check$diff)) < 1e-2) {
      check <<- check %>%
        dplyr::rename(value.var = value.x,
                      value.query = value.y)
      warning("The annual Emissions|CO2 sum by region does not match the output of the `CO2 emissions by region` query.\nType `check` to see the details.")
    }
  }

  co2_emiss <<- co2_emiss
}


#' get_gross_co2_emiss
#'
#' Retrieves the Gross CO2 emissions: Gross emissions of carbon dioxide (CO2),
#' not accounting for negative emissions from bioenergy with CCS (BECCS) or
#' agriculture, forestry and other land use (AFOLU)
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @return `gross_co2_emiss_clean` global variable.
#' @keywords internal co2 tmp
#' @importFrom magrittr %>%
#' @export
get_gross_co2_emiss <- function(GCAM_version = 'v8.2') {
  var <- value <- unit_conv <- scenario <- region <- year <-
    gross_co2_emiss_clean <- ghg <- technology <- subsector <- NULL

  check_queries("gross_co2_emiss_clean", GCAM_version)

  gross_co2_emiss_clean <- rbind(
    co2_emissions_clean,
    co2_removal_raw) %>%
    dplyr::group_by(scenario, region, var, year) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(var = gsub('Emissions', 'Gross Emissions', var)) %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  gross_co2_emiss_clean <<- gross_co2_emiss_clean
}


#' get_lu_co2
#'
#' Retrieves land use CO2 emissions data.
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @return `LUC_emiss` global variable.
#' @keywords internal lu co2
#' @importFrom magrittr %>%
#' @export
get_lu_co2 <- function(GCAM_version = 'v8.2') {
  year <- scenario <- region <- value <- var <- LUC_emiss <- NULL

  check_queries("LUC_emiss", GCAM_version)

  LUC_emiss <-
    # Land use CO2
    check_inf(rgcam::getQuery(prj, "LUC emissions by region"),
              dataset_name = "LUC emissions by region") %>%
    dplyr::filter(year %in% gcam_years) %>%
    dplyr::group_by(scenario, region, year) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      value = value * get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_C_CO2']],
      var = "Emissions|CO2|AFOLU"
    ) %>%
    filter_variables(extra = c('Emissions|CO2', available_variables(F, GCAM_version)[grepl("^Gross Emissions", available_variables(F, GCAM_version))])) %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  LUC_emiss <- rbind(
    LUC_emiss,
    LUC_emiss %>%
      dplyr::mutate(var = 'Emissions|CO2'),
    LUC_emiss %>%
      dplyr::mutate(var = 'Emissions|CO2|AFOLU [NGHGI]')
  ) %>%
    filter_variables(extra = available_variables(F, GCAM_version)[grepl("^Gross Emissions", available_variables(F, GCAM_version))])

  LUC_emiss <<- LUC_emiss
}


#' get_co2_emissions
#'
#' Combines CO2 emission queries into a single dataset.
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @return `co2_emissions_clean` global variable.
#' @keywords internal co2 process
#' @importFrom magrittr %>%
#' @export
get_co2_emissions <- function(GCAM_version = 'v8.2') {
  scenario <- region <- year <- var <- value <- co2_emissions_clean <- NULL

  check_queries("co2_emissions_clean", GCAM_version)

  co2_emissions_clean <-
    dplyr::bind_rows(co2_emiss, LUC_emiss) %>%
    dplyr::group_by(scenario, region, year, var) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  co2_emissions_clean <<- co2_emissions_clean
}

#' get_nonco2_emissions
#'
#' Retrieves non-CO2 emissions data.
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @return `nonco2_clean` global variable.
#' @keywords internal nonco2
#' @importFrom magrittr %>%
#' @export
get_nonco2_emissions <- function(GCAM_version = 'v8.2') {
  value <- unit_conv <- scenario <- region <- year <- var <-
    queryItem1 <- queryItem2 <- nonco2_clean <- NULL

  check_queries("nonco2_clean", GCAM_version)

  var_fun_map <- get(paste('var_fun_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))
  queryItem1 <- var_fun_map[var_fun_map$name == "nonco2_clean", "queries"][[1]][1]
  queryItem2 <- var_fun_map[var_fun_map$name == "nonco2_clean", "queries"][[1]][2]
  nonco2_emis_sector_map <- get(paste('nonco2_emis_sector_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))

  nonco2_tmp <-
    check_inf(rgcam::getQuery(prj, queryItem1), dataset_name = queryItem1)
  nonco2_agg <- nonco2_tmp %>%
    dplyr::filter(!(grepl('UnmanagedLand', sector) & grepl('ForestFire|GrasslandFires', subsector))) %>%
    dplyr::filter(!(grepl('urban processes', sector) & grepl('landfills|wastewater|waste_incineration', subsector))) %>%
    dplyr::mutate(subsector = dplyr::if_else(subsector %in% unique(nonco2_emis_sector_map$subsector),subsector,NA)) %>%
    dplyr::group_by(Units, scenario, region, sector, subsector, ghg, year) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup()

  if(GCAM_version %in% get('deciles_GCAM_versions', envir = asNamespace("gcamreport"))) {
    nonco2_agg <- nonco2_agg %>%
      tidyr::separate(sector, into = c("sector", "decile"), sep = "_d", extra = "merge", fill = "right") %>%
      dplyr::group_by(Units, scenario, region, sector, subsector, ghg, year) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::ungroup()
  }


  nonco2_tmp2 <- dplyr::bind_rows(
    nonco2_agg %>%
      rbind(nonco2_tmp %>% # Land|Fires|Forest Burning
              dplyr::filter(grepl('UnmanagedLand', sector) &
                              grepl('ForestFire|GrasslandFires', subsector)) %>%
              dplyr::mutate(subsector = dplyr::if_else(grepl('ForestFire',subsector),'ForestFire',
                                                       dplyr::if_else(grepl('GrasslandFires',subsector),'GrasslandFires',NA))) %>%
              dplyr::group_by(Units, scenario, region, sector, subsector, ghg, year) %>%
              dplyr::summarise(value = sum(value)) %>%
              dplyr::ungroup()) %>%
      rbind(nonco2_tmp %>% # Waste
              dplyr::filter(grepl('urban processes', sector) &
                              grepl('landfills|wastewater|waste_incineration', subsector)) %>%
              dplyr::mutate(subsector = dplyr::if_else(grepl('landfills',subsector),'landfills',
                                                       dplyr::if_else(grepl('wastewater',subsector),'wastewater',
                                                                      dplyr::if_else(grepl('waste_incineration',subsector),'waste_incineration',NA)))) %>%
              dplyr::group_by(Units, scenario, region, sector, subsector, ghg, year) %>%
              dplyr::summarise(value = sum(value)) %>%
              dplyr::ungroup()) %>%
      dplyr::filter(!grepl('CO2',ghg)) %>%
      left_join_strict(nonco2_emis_sector_map,
                       by = c("ghg", "sector", "subsector"), mapping = paste('nonco2_emis_sector_map',GCAM_version,sep='_'), multiple = "all", relationship = "many-to-many"),
    check_inf(rgcam::getQuery(prj, queryItem2),
              dataset_name = queryItem2) %>%
      dplyr::filter(!grepl('CO2',ghg)) %>%
      left_join_strict(get(paste('nonco2_emis_resource_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")),
                       by = c("ghg", "resource"), multiple = "all", relationship = "many-to-many")
  ) %>%
    dplyr::filter(var != 'NoReported', !is.na(var)) %>%
    filter_variables() %>%
    # 1Tg = 1Mt; 1Gg = 1kt; 1Tg = 1000kt
    dplyr::mutate(value = value * unit_conv) %>%
    dplyr::group_by(scenario, region, year, var) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))


  nonco2_clean <-
    nonco2_tmp2 %>%
    rbind(f_gases_hfc,
          f_gases_pfc) %>%
    rbind(f_gases_total)


  nonco2_clean <<- nonco2_clean
}

#' get_fgas
#'
#' Computes F-Gases emissions.
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @param GWP_version Global Warming Potential (GWP) version: 'AR5' (default), 'AR6', or 'AR4'.
#' @return `f_gases_total`, `f_gases_hfc`, `f_gases_pfc` global variables.
#' @keywords internal f-gases process
#' @importFrom magrittr %>%
#' @export
get_fgas <- function(GCAM_version = 'v8.2', GWP_version = 'AR5') {
  ghg <- variable <- scenario <- region <- year <- value <-
    f_gases_total <- f_gases_hfc <- f_gases_pfc <- NULL

  check_queries("f_gases_total", GCAM_version)

  f_gas_subtotal <-
    check_inf(rgcam::getQuery(prj, "nonCO2 emissions by region"),
              dataset_name = "nonCO2 emissions by region") %>%
    dplyr::filter(!grepl("CO2_ETS", ghg)) %>%
    conv_ghg_co2e(GWP_version = GWP_version, GCAM_version = GCAM_version) %>%
    dplyr::filter(variable %in% get(paste('F_GASES',GCAM_version,sep='_'), envir = asNamespace("gcamreport")))

  # F-Gases total
  f_gases_total <-
    f_gas_subtotal %>%
    dplyr::group_by(scenario, region, year) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(var = "Emissions|F-Gases") %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  # HFCs & PFCs (aggregates of several gases homogenizing the units)
  gwp_hfc <- get(paste('ghg_GWP',GWP_version,sep='_'), envir = asNamespace("gcamreport")) %>%
    dplyr::filter(GHG_gases == 'HFC134a') %>%
    dplyr::pull(GWP)

  f_gases_hfc <- f_gas_subtotal %>%
    dplyr::filter(grepl('HFC', variable)) %>%
    dplyr::mutate(value = value / gwp_hfc) %>%
    dplyr::group_by(scenario, region, year) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(var = "Emissions|HFC") %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))


  gwp_pfc <- get(paste('ghg_GWP',GWP_version,sep='_'), envir = asNamespace("gcamreport")) %>%
    dplyr::filter(GHG_gases == 'CF4') %>%
    dplyr::pull(GWP)

  f_gases_pfc <- f_gas_subtotal %>%
    dplyr::filter(grepl('CF4', variable) | grepl('C2F6', variable)) %>%
    dplyr::mutate(value = value / gwp_pfc) %>%
    dplyr::group_by(scenario, region, year) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(var = "Emissions|PFC") %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))


  f_gases_total <<- f_gases_total
  f_gases_hfc <<- f_gases_hfc
  f_gases_pfc <<- f_gases_pfc
}


#' get_kyoto_gases
#'
#' Get sectorial GHG emissions.
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @param GWP_version Global Warming Potential (GWP) version: 'AR5' (default), 'AR6', or 'AR4'.
#' @return `kyoto_gases_clean` global variable.
#' @keywords internal ghg
#' @importFrom magrittr %>%
#' @export
get_kyoto_gases <- function(GCAM_version = 'v8.2', GWP_version = 'AR5') {
  ghg <- resource <- subresource <- sector <- variable <- scenario <- tmp <-
    region <- var <- year <- value <- queryItem1 <- queryItem2 <- queryItem3 <-
    Units <- subsector <- kyoto_gases_clean <- unit_conv <- NULL

  check_queries("kyoto_gases_clean", GCAM_version)

  var_fun_map <- get(paste('var_fun_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))
  queryItem1 <- var_fun_map[var_fun_map$name == "kyoto_gases_clean", "queries"][[1]][1]
  queryItem2 <- var_fun_map[var_fun_map$name == "kyoto_gases_clean", "queries"][[1]][2]
  queryItem3 <- var_fun_map[var_fun_map$name == "kyoto_gases_clean", "queries"][[1]][3]

  kyoto_sector_map <- get(paste('kyoto_sector_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))

  kyoto_gases_1 <- check_inf(rgcam::getQuery(prj, queryItem1), dataset_name = queryItem1)

  kyoto_gases_fires <- kyoto_gases_1 %>% # Land|Fires|Forest Burning
    dplyr::filter(grepl('UnmanagedLand', sector) & grepl('ForestFire|GrasslandFires', subsector)) %>%
    dplyr::mutate(subsector = dplyr::if_else(grepl('ForestFire',subsector),'ForestFire',
                                             dplyr::if_else(grepl('GrasslandFires',subsector),'GrasslandFires',NA))) %>%
    dplyr::group_by(Units, scenario, region, sector, sec_subsector = subsector, ghg, year) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup()

  kyoto_gases_waste <- kyoto_gases_1 %>% # Waste
    dplyr::filter(grepl('urban processes', sector) & grepl('landfills|wastewater|waste_incineration', subsector)) %>%
    dplyr::mutate(subsector = dplyr::if_else(grepl('landfills',subsector),'landfills',
                                             dplyr::if_else(grepl('wastewater',subsector),'wastewater',
                                                            dplyr::if_else(grepl('waste_incineration',subsector),'waste_incineration',NA)))) %>%
    dplyr::group_by(Units, scenario, region, sector, sec_subsector = subsector, ghg, year) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup()

  kyoto_gases_agg <- kyoto_gases_1 %>%
    dplyr::filter(!(grepl('UnmanagedLand', sector) & grepl('ForestFire|GrasslandFires', subsector))) %>%
    dplyr::filter(!(grepl('urban processes', sector) & grepl('landfills|wastewater|waste_incineration', subsector))) %>%
    dplyr::mutate(sec_subsector  = dplyr::if_else(subsector %in% unique(kyoto_sector_map$subsector),subsector,NA)) %>%
    dplyr::group_by(Units, scenario, region, sector, sec_subsector , ghg, year) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup()

  kyoto_gases_2 <- dplyr::bind_rows(kyoto_gases_fires,
                                    kyoto_gases_waste,
                                    kyoto_gases_agg) %>%
    dplyr::filter(!grepl("CO2", ghg)) %>%
    dplyr::bind_rows(check_inf(rgcam::getQuery(prj, queryItem2),
                               dataset_name = queryItem2) %>%
                       dplyr::rename(sector = resource) %>%
                       dplyr::select(-subresource) %>%
                       dplyr::mutate(sec_subsector = NA)) %>%
    dplyr::bind_rows(check_inf(rgcam::getQuery(prj, queryItem3),
                               dataset_name = queryItem3) %>%
                       dplyr::mutate(ghg = "CO2") %>%
                       dplyr::mutate(sec_subsector  = dplyr::if_else(subsector %in% unique(kyoto_sector_map$subsector),subsector,NA)) %>%
                       dplyr::group_by(Units, scenario, region, sector, sec_subsector , ghg, year) %>%
                       dplyr::summarise(value = sum(value)) %>%
                       dplyr::ungroup()) %>%
    dplyr::mutate(subsector = sector) %>%
    conv_ghg_co2e(GWP_version = GWP_version, GCAM_version = GCAM_version) %>%
    dplyr::filter(variable %in% get(paste('GHG_gases',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))) %>%
    dplyr::rename(ghg = variable,
                  ghg_sector = sector,
                  sector = subsector,
                  subsector = sec_subsector) %>%
    dplyr::mutate(ghg_sector = dplyr::if_else(is.na(ghg_sector), 'none', ghg_sector))

  # gather deciles if necessary
  if(GCAM_version %in% get('deciles_GCAM_versions', envir = asNamespace("gcamreport"))) {
    kyoto_gases_2 <- kyoto_gases_2 %>%
      tidyr::separate(sector, into = c("sector", "decile"), sep = "_d", extra = "merge", fill = "right") %>%
      dplyr::group_by(Units, scenario, region, ghg, ghg_sector, sector, subsector, year) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::ungroup()
  }

  kyoto_gases_clean <- kyoto_gases_2 %>%
    left_join_strict(kyoto_sector_map %>%
                       dplyr::select(-unit_conv) %>%
                       dplyr::mutate(ghg_sector = dplyr::if_else(is.na(ghg_sector), 'none', ghg_sector)),
                     by = c("ghg", "ghg_sector", "sector", "subsector"),
                     mapping = paste('kyoto_sector_map',GCAM_version,sep='_'), multiple = "all", relationship = "many-to-many") %>%
    dplyr::filter(var != 'NoReported', !is.na(var)) %>%
    filter_variables() %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns)) %>%
    dplyr::bind_rows(
      LUC_emiss %>%
        dplyr::filter(var == 'Emissions|CO2|AFOLU') %>%
        dplyr::mutate(var = "Emissions|Kyoto Gases") %>%
        filter_variables(),
      LUC_emiss %>%
        dplyr::filter(var == 'Emissions|CO2|AFOLU') %>%
        dplyr::mutate(var = "Emissions|Kyoto Gases|AFOLU") %>%
        filter_variables()
    ) %>%
    dplyr::group_by(scenario, region, var, year) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup()

  kyoto_gases_clean <<- kyoto_gases_clean
}


#' get_refliq_bioshare
#'
#' Get biomass share of feedstocks refined liquids production
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal co2
#' @return `refliq_bioshare` global variable.
#' @importFrom magrittr %>%
#' @export
get_refliq_bioshare <- function(GCAM_version = 'v8.2') {
  scenario <- region <- year <- var <- value <- unit_conv <-
    refliq_bioshare <- refliq_bioshare_w <- NULL

  check_queries("refliq_bioshare", GCAM_version)

  refliq_bioshare <- check_inf(rgcam::getQuery(prj, "refined liquids production by tech"),
                               dataset_name = "refined liquids production by tech") %>%
    dplyr::group_by(scenario, region, subsector, year) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(scenario, region, year) %>%
    dplyr::mutate(total = sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(share = value/total)


  refliq_bioshare_w <- refliq_bioshare %>%
    dplyr::group_by(scenario, subsector, year) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(scenario, year) %>%
    dplyr::mutate(total = sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(share = value/total,
                  region = 'World')

  refliq_bioshare <- rbind(
    refliq_bioshare,
    refliq_bioshare_w
  ) %>%
    dplyr::filter(subsector == 'biomass liquids') %>%
    dplyr::select(scenario,region,year,share)

  refliq_bioshare <<- refliq_bioshare
}


#' get_co2_sequestration
#'
#' Get carbon sequestration.
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal co2
#' @return `co2_sequestration_clean` and `co2_removal_raw` global variables.
#' @importFrom magrittr %>%
#' @export
get_co2_sequestration <- function(GCAM_version = 'v8.2') {
  scenario <- region <- year <- var <- value <- unit_conv <-
    co2_removal_raw <- co2_sequestration <- NULL

  check_queries("co2_sequestration_clean", GCAM_version)
  check_queries("co2_removal_raw", GCAM_version)

  co2_sequestration <- suppressWarnings(
    check_inf(rgcam::getQuery(prj, "CO2 sequestration by tech"),
              dataset_name = "CO2 sequestration by tech") %>%
    dplyr::filter(year %in% years_in_prj) %>%
      left_join_strict(get(paste('carbon_seq_tech_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")),
                       by = c("sector", "technology"), mapping = paste('carbon_seq_tech_map',GCAM_version,sep='_'), multiple = "all") %>%
      dplyr::filter(var != 'NoReported', !is.na(var)) %>%
      tidyr::complete(tidyr::nesting(scenario, region, year),
                      var = unique(var),
                      fill = list(value = 0)
      ) %>%
      dplyr::filter(!is.na(var), year > 2005, year <= final_year.global) %>%
      # add refliq_bioshare (share of biomass of refined liquids production. Only used in Carbon Removal)
      left_join_strict(refliq_bioshare %>%
                         tidyr::complete(tidyr::nesting(scenario, year),
                                         region = unique(na.omit(get(paste('reg_cont',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['region']])),
                                         fill = list(share = 0)
                         ),
                       by = c('scenario','region','year')) %>%
      dplyr::mutate(share = dplyr::if_else(grepl('refliq_bioshare',unit_conv), share, 1),
                    unit_conv = stringr::str_remove(unit_conv, "Xrefliq_bioshare")) %>%
      dplyr::mutate(value = value * as.numeric(unit_conv) * share) %>%
      # aggregate by variable
      dplyr::group_by(scenario, region, year, var) %>%
      dplyr::summarise(value = sum(value, na.rm = T)) %>%
      dplyr::ungroup() %>%
      dplyr::select(dplyr::all_of(gcamreport::long_columns))
  ) %>%
    dplyr::bind_rows(
      # Inverse of CO2_LUC when negative, zero when CO2_LUC is positive
      # All Carbon Removal|Land Use emissions are considered as Re/Afforestation
      LUC_emiss %>%
        dplyr::filter(var == 'Emissions|CO2|AFOLU') %>%
        dplyr::mutate(var = 'Carbon Removal') %>%
        dplyr::mutate(value = dplyr::if_else(value < 0, -value, 0)),
      LUC_emiss %>%
        dplyr::filter(var == 'Emissions|CO2|AFOLU') %>%
        dplyr::mutate(var = 'Carbon Removal|Land Use') %>%
        dplyr::mutate(value = dplyr::if_else(value < 0, -value, 0)),
      LUC_emiss %>%
        dplyr::filter(var == 'Emissions|CO2|AFOLU') %>%
        dplyr::mutate(var = 'Carbon Removal|Land Use|Re/Afforestation') %>%
        dplyr::mutate(value = dplyr::if_else(value < 0, -value, 0))
    ) %>%
    dplyr::group_by(scenario, region, year, var) %>% #
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup()

  # add Gross Removals|CO2 = Carbon Removal
  # add Gross Removals|CO2|AFOLU = Carbon Removal|Land Use"
  co2_sequestration_clean <- dplyr::bind_rows(
    co2_sequestration,
    co2_sequestration %>%
      dplyr::filter(var == 'Carbon Removal') %>%
      dplyr::mutate(var = 'Gross Removals|CO2'),
    co2_sequestration %>%
      dplyr::filter(var == 'Carbon Removal|Land Use') %>%
      dplyr::mutate(var = 'Gross Removals|CO2|AFOLU')
  )


  # CO2 Removal items with further desegregation to compute later the Gross emissions
  co2_removal_raw <- suppressWarnings(
    check_inf(rgcam::getQuery(prj, "CO2 sequestration by tech"),
              dataset_name = "CO2 sequestration by tech") %>%
      dplyr::filter(year %in% years_in_prj) %>%
      # consider only carbon removal items
      left_join_strict(get(paste('carbon_seq_tech_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")),
                       by = c("sector", "technology"), mapping = paste('carbon_seq_tech_map',GCAM_version,sep='_'), multiple = "all") %>%
      dplyr::filter(var != 'NoReported', !is.na(var)) %>%
      dplyr::filter(var == 'Carbon Removal', year >= 2005, year <= final_year.global) %>%
      # add refliq_bioshare (share of biomass of refined liquids production. Only used in Carbon Removal)
      left_join_strict(refliq_bioshare %>%
                         tidyr::complete(tidyr::nesting(scenario, year),
                                         region = unique(na.omit(get(paste('reg_cont',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['region']])),
                                         fill = list(share = 0)
                         ),
                       by = c('scenario','region','year')) %>%
      dplyr::mutate(share = dplyr::if_else(grepl('refliq_bioshare',unit_conv), share, 1),
                    unit_conv = stringr::str_remove(unit_conv, "Xrefliq_bioshare")) %>%
      dplyr::mutate(value = value * as.numeric(unit_conv) * share) %>%
      dplyr::select(-var, -unit_conv, -share) %>%
      # desegregate further the items (DAC add only to Emissions|CO2)
      left_join_strict(get(paste('co2_tech_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")),
                       by = c("sector", "subsector", "technology"), mapping = paste('co2_tech_map',GCAM_version,sep='_'), multiple = "all") %>%
      dplyr::filter(var != 'NoReported', !is.na(var)) %>%
      filter_variables(extra = 'Emissions|CO2|Other Capture and Removal') %>%
      tidyr::complete(tidyr::nesting(scenario, region, year),
                      var = unique(var),
                      fill = list(value = 0)
      ) %>%
      dplyr::filter(!is.na(var)) %>%
      dplyr::group_by(scenario, region, year, var) %>%
      dplyr::summarise(value = sum(value, na.rm = T)) %>%
      dplyr::ungroup() %>%
      dplyr::select(dplyr::all_of(gcamreport::long_columns))
  ) %>%
    dplyr::bind_rows(
      # Inverse of CO2_LUC when negative, zero when CO2_LUC is positive
      LUC_emiss %>%
        dplyr::mutate(value = dplyr::if_else(value < 0, -value, 0))
    ) %>%
    dplyr::group_by(scenario, region, year, var) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup()


  # add 0 to Carbon Removal|Ocean since GCAM does not report it
  co2_sequestration_clean <- rbind(
    co2_sequestration_clean,
    co2_sequestration_clean %>%
      dplyr::mutate(var = 'Carbon Removal|Ocean',
                    value = 0
      ) %>%
      dplyr::distinct()
  )

  co2_removal_raw <<- co2_removal_raw
  co2_sequestration_clean <<- co2_sequestration_clean
}


# Water
# ==============================================================================================
#' get_water_withdrawals
#'
#' Retrieves the water withdrawals.
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @return `water_withdrawals_clean` global variable.
#' @keywords internal water
#' @importFrom magrittr %>%
#' @export
get_water_withdrawals <- function(GCAM_version = 'v8.2') {
  year <- water_withdrawals_clean <- NULL

  check_queries("water_withdrawals_clean", GCAM_version)

  # read queries according to the indicated GCAM version
  var_fun_map <- get(paste('var_fun_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))
  queryItems <- var_fun_map[var_fun_map$name == "water_withdrawals_clean", "queries"][[1]]

  if (length(queryItems) > 1) {
    # if (grep('Europe',GCAM_version)) {
    water_withdrawals <- check_inf(rgcam::getQuery(prj, "water withdrawals by subsector - nested subsectors2"),
                                   dataset_name = "water withdrawals by subsector") %>%
      dplyr::rename(technology = 'subsector...6',
                    subsector = 'subsector...5') %>%
      rbind(check_inf(rgcam::getQuery(prj, "water withdrawals by subsector - nested subsectors1"),
                      dataset_name = "water withdrawals by subsector"))
  } else {
    water_withdrawals <-
      check_inf(rgcam::getQuery(prj, "water withdrawals by subsector"),
                dataset_name = "water withdrawals by subsector")
  }

  water_withdrawals_clean <- water_withdrawals %>%
    left_join_strict(get(paste('water_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")),
                     by = c("sector", "subsector"), mapping = paste('water_map',GCAM_version,sep='_')) %>%
    dplyr::filter(var != 'NoReported', !is.na(var)) %>%
    filter_variables() %>%
    dplyr::mutate(value = value * unit_conv) %>%
    dplyr::group_by(scenario, region, var, year) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    # conveyance loss factor for IRRIGATION water
    left_join_error_no_match(get(paste('conveyance.eff',GCAM_version,sep='_'), envir = asNamespace("gcamreport")),
                             by = 'region') %>%
    dplyr::mutate(value = dplyr::if_else(grepl('Irrigation',var), value / conveyance.eff, value)) %>%
    # set var to consumption
    dplyr::mutate(var = gsub("XX", "Withdrawal", var)) %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  water_withdrawals_clean <<- water_withdrawals_clean
}

#' get_water_consumption
#'
#' Retrieves the water get_water_consumption
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @return `water_consumption_clean` global variable.
#' @keywords internal water
#' @importFrom magrittr %>%
#' @export
get_water_consumption <- function(GCAM_version = 'v8.2') {
  year <- water_consumption_clean <- NULL

  check_queries("water_consumption_clean", GCAM_version)

  # read queries according to the indicated GCAM version
  var_fun_map <- get(paste('var_fun_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))
  queryItems <- var_fun_map[var_fun_map$name == "water_consumption_clean", "queries"][[1]]

  if (length(queryItems) > 1) {
  # if (grep('Europe',GCAM_version)) {
    water_consumption <- check_inf(rgcam::getQuery(prj, "water consumption by subsector - nested subsectors2"),
                                   dataset_name = "water consumption by subsector") %>%
      dplyr::rename(technology = 'subsector...6',
                    subsector = 'subsector...5') %>%
      rbind(check_inf(rgcam::getQuery(prj, "water consumption by subsector - nested subsectors1"),
                      dataset_name = "water consumption by subsector"))
  } else {
    water_consumption <-
      check_inf(rgcam::getQuery(prj, "water consumption by subsector"),
                dataset_name = "water consumption by subsector")
  }

  water_consumption_clean <- water_consumption %>%
    left_join_strict(get(paste('water_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")),
                     by = c("sector", "subsector"), mapping = paste('water_map',GCAM_version,sep='_')) %>%
    dplyr::filter(var != 'NoReported', !is.na(var)) %>%
    filter_variables() %>%
    dplyr::mutate(value = value * unit_conv) %>%
    dplyr::group_by(scenario, region, var, year) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    # conveyance loss factor for IRRIGATION water
    left_join_error_no_match(get(paste('conveyance.eff',GCAM_version,sep='_'), envir = asNamespace("gcamreport")),
                             by = 'region') %>%
    dplyr::mutate(value = dplyr::if_else(grepl('Irrigation',var), value / conveyance.eff, value)) %>%
    # set var to consumption
    dplyr::mutate(var = gsub("XX", "Consumption", var)) %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  water_consumption_clean <<- water_consumption_clean
}



# Agriculture and land use
# ==============================================================================================

#' get_yield
#'
#' Get agricultural demand.
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal ag
#' @return `yield_clean` global variable.
#' @importFrom magrittr %>%
#' @export
get_yield <- function(GCAM_version = 'v8.2') {
  yield_regional <- yield_world <- yield_map <- yield_clean <- NULL

  yield_map <- get(paste('yield_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))

  yield_regional <- suppressMessages(land_yield %>%
                                       dplyr::rename(land = value, land_var = var) %>%
                                       dplyr::filter(land_var %in% yield_map$land_var) %>%
                                       left_join_strict(yield_map, by = 'land_var') %>%
                                       dplyr::filter(year <= final_year.global) %>%
                                       left_join_strict(ag_production_clean %>%
                                                          dplyr::rename(prod = value, prod_var = var) %>%
                                                          dplyr::filter(prod_var %in% yield_map$prod_var) %>%
                                                          tidyr::complete(tidyr::nesting(scenario, region),
                                                                          year = unique(land_yield$year),
                                                                          prod_var = unique(yield_map$prod_var),
                                                                          prod = 0) %>%
                                                          dplyr::group_by(scenario, prod_var, region, year) %>%
                                                          dplyr::summarise(prod = sum(prod)) %>%
                                                          dplyr::ungroup() %>%
                                                          dplyr::filter(year <= final_year.global),
                                                        by = c('prod_var','scenario','region','year')) %>%
                                       dplyr::mutate(value = prod / land) %>%
                                       dplyr::mutate(value = dplyr::if_else(is.na(value) | is.nan(value), 0, value)) %>%
                                       dplyr::rename(var = yield_var) %>%
                                       dplyr::select(dplyr::all_of(gcamreport::long_columns)))

  yield_world <- suppressMessages(land_yield %>%
                                    dplyr::rename(land = value, land_var = var) %>%
                                    dplyr::filter(land_var %in% yield_map$land_var) %>%
                                    dplyr::group_by(scenario, land_var, year) %>%
                                    dplyr::summarise(land = sum(land)) %>%
                                    dplyr::ungroup() %>%
                                    dplyr::mutate(region = 'World') %>%
                                    left_join_strict(yield_map, by = 'land_var') %>%
                                    dplyr::filter(year <= final_year.global) %>%
                                    left_join_strict(ag_production_clean %>%
                                                       dplyr::rename(prod = value, prod_var = var) %>%
                                                       dplyr::filter(prod_var %in% yield_map$prod_var) %>%
                                                       dplyr::group_by(scenario, prod_var, year) %>%
                                                       dplyr::summarise(prod = sum(prod)) %>%
                                                       dplyr::ungroup() %>%
                                                       dplyr::mutate(region = 'World') %>%
                                                       tidyr::complete(tidyr::nesting(scenario, region, year),
                                                                       prod_var = unique(yield_map$prod_var),
                                                                       prod = 0) %>%
                                                       dplyr::group_by(scenario, prod_var, region, year) %>%
                                                       dplyr::summarise(prod = sum(prod)) %>%
                                                       dplyr::ungroup() %>%
                                                       dplyr::filter(year <= final_year.global),
                                                     by = c('prod_var','scenario','region','year')) %>%
                                    dplyr::mutate(value = prod / land) %>%
                                    dplyr::mutate(value = dplyr::if_else(is.na(value) | is.nan(value), 0, value)) %>%
                                    dplyr::rename(var = yield_var) %>%
                                    dplyr::select(dplyr::all_of(gcamreport::long_columns)))


  yield_clean <- rbind(
    yield_regional,
    yield_world
  )

  yield_clean <<- yield_clean
}

#' get_biomass_shares
#'
#' Get biomass production shares: total = residue + msw + purpose_grown;
#' The share of msw + residue over the total will be substracted from the biomass ag demand
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal ag
#' @return `biomass_shares` global variable.
#' @importFrom magrittr %>%
#' @export
get_biomass_shares <- function(GCAM_version = 'v8.2') {

  check_queries("biomass_shares", GCAM_version)

  biomass_shares <- dplyr::full_join(
    check_inf(rgcam::getQuery(prj, "purpose-grown biomass production"),
              dataset_name = "purpose-grown biomass production") %>%
      dplyr::rename(purpose_grown = value),
    check_inf(rgcam::getQuery(prj, "MSW production"),
              dataset_name = "MSW production") %>%
      dplyr::rename(msw = value,
                    sector = resource),
    by = c('Units','scenario','region','sector','year'), kee) %>%
    dplyr::full_join(
      check_inf(rgcam::getQuery(prj, "residue biomass production"),
                dataset_name = "residue biomass production") %>%
        dplyr::group_by(dplyr::across(-c(sector, value))) %>%
        dplyr::summarise(value = sum(value), .groups = 'drop') %>%
        dplyr::rename(residue = value,
                      sector = output),
      by = c('Units','scenario','region','sector','year')) %>%
    # add 0 when no-reported values (purpose_grown biomass might be 0 for some years-regions)
    dplyr::mutate(dplyr::across(where(is.numeric), ~ dplyr::if_else(is.na(.), 0, .))) %>%
    # compute residue + msw share (over total)
    dplyr::mutate(share = (residue + msw) / (purpose_grown  + residue + msw)) %>%
    dplyr::mutate(share = dplyr::if_else(is.na(share), 0, share)) %>%
    # clean dataset
    dplyr::select(scenario, region, sector, year, share)

  biomass_shares <<- biomass_shares

}

#' get_ag_demand
#'
#' Get agricultural demand.
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal ag
#' @return `ag_demand_clean` global variable.
#' @importFrom magrittr %>%
#' @export
get_ag_demand <- function(GCAM_version = 'v8.2') {
  sector <- input <- var <- value <- unit_conv <- scenario <- region <- year <-
    ag_demand_clean <- NULL

  check_queries("ag_demand_clean", GCAM_version)

  ag_demand_clean <-
    dplyr::bind_rows(
      check_inf(rgcam::getQuery(prj, "demand balances by crop commodity"),
                dataset_name = "demand balances by crop commodity"),
      check_inf(rgcam::getQuery(prj, "demand balances by meat and dairy commodity"),
                dataset_name = "demand balances by meat and dairy commodity"),
      check_inf(rgcam::getQuery(prj, "regional biomass consumption"),
                dataset_name = "regional biomass consumption") %>%
        # share of non-residue and non-msw biomass
        left_join_error_no_match(biomass_shares %>%
                                   dplyr::select(-sector) %>%
                                   # share of non-residue and non-msw biomass (inverse of the current share)
                                   dplyr::mutate(share = 1 - share),
                                 by = c('scenario','region','year')) %>%
        dplyr::mutate(value = value * share) %>%
        # Units: from EJ to Mt (biomass)
        # 1 EJ = 1e9 GJ; 1 Mt = 1e6 T; 1 Mt = EJ * 1e9 / (aglu.BIO_ENERGY_CONTENT_GJT * 1e6)
        dplyr::mutate(value = value * 1e3 / get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['aglu.BIO_ENERGY_CONTENT_GJT']])
    ) %>%
    left_join_strict(get(paste('ag_demand_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")),
                     by = c("input","sector"), mapping = paste('ag_demand_map',GCAM_version,sep='_'), multiple = "all", relationship = "many-to-many") %>%
    dplyr::filter(var != 'NoReported', !is.na(var)) %>%
    filter_variables() %>%
    # add water content data
    left_join_strict(get(paste('food_items_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")) %>%
                       dplyr::rename(input = regional_item,
                                     GCAM_commodity = item),
                     by = c("input")) %>%
    left_join_strict(get(paste('water_content'), envir = asNamespace("gcamreport")),
                     by = 'GCAM_commodity') %>%
    # units to annual million t DM
    dplyr::mutate(value = value * (unit_conv - mean_water_content)) %>%
    filter_variables() %>%
    dplyr::group_by(scenario, region, year, var) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  ag_demand_clean <<- ag_demand_clean
}


#' get_ag_weights
#'
#' Get agricultural items weighted by demand. By region and global.
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal ag
#' @return `ag_weights` and `ag_wld_weights` global variables.
#' @importFrom magrittr %>%
#' @export
get_ag_weights <- function(GCAM_version = 'v8.2') {
  sector <- input <- var <- value <- unit_conv <- scenario <- region <-
    year <- ag_weights <- ag_wld_weights <- NULL

  check_queries("ag_weights", GCAM_version)
  check_queries("ag_wld_weights", GCAM_version)

  ag_demand_tmp <-
    dplyr::bind_rows(
      check_inf(rgcam::getQuery(prj, "demand balances by crop commodity"),
                dataset_name = "demand balances by crop commodity"),
      check_inf(rgcam::getQuery(prj, "demand balances by meat and dairy commodity"),
                dataset_name = "demand balances by meat and dairy commodity"),
      check_inf(rgcam::getQuery(prj, "regional biomass consumption"),
                dataset_name = "regional biomass consumption") %>%
        # Units: from EJ to Mt (biomass)
        # 1 EJ = 1e9 GJ; 1 Mt = 1e6 T; 1 Mt = EJ * 1e9 / (aglu.BIO_ENERGY_CONTENT_GJT * 1e6)
        dplyr::mutate(value = value * 1e3 / get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['aglu.BIO_ENERGY_CONTENT_GJT']])
    ) %>%
    left_join_strict(get(paste('ag_demand_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")),
                     by = c("input","sector"), mapping = paste('ag_demand_map',GCAM_version,sep='_'), multiple = "all", relationship = "many-to-many") %>%
    dplyr::filter(var != 'NoReported', !is.na(var)) %>%
    filter_variables() %>%
    dplyr::mutate(value = value * unit_conv) %>%
    # select variables whose price will be computed
    dplyr::right_join(get(paste('ag_demand_price_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")) %>%
                        dplyr::filter(ag_price_variable != 'NoReported', ag_price_variable != ""),
                      by = c('var' = 'ag_demand_variable'), relationship = "many-to-many") %>%
    dplyr::distinct() %>%
    dplyr::filter(!is.na(value))



  # weights by sector within each region
  ag_weights <-
    ag_demand_tmp %>%
    dplyr::group_by(Units, scenario, region, year, ag_price_variable) %>%
    dplyr::mutate(total_demand_var = sum(value)) %>%
    dplyr::ungroup() %>%
    # compute weight by sector and input
    dplyr::mutate(weight = value / total_demand_var) %>%
    # clean dataset
    dplyr::select(dplyr::all_of(gcamreport::long_columns), sector = input,
                  ag_demand_variable = var, var = ag_price_variable, weight, -value)

  sectors_combination <- ag_weights %>%
    dplyr::select(sector, var, ag_demand_variable) %>%
    dplyr::distinct() %>%
    tidyr::expand_grid(
      scenario = unique(ag_weights$scenario),
      region = unique(ag_weights$region),
      year = unique(ag_weights$year)
    )

  ag_weights <- dplyr::left_join(
    sectors_combination,
    ag_weights,
    by = c('sector', 'var', 'ag_demand_variable', 'scenario', 'region', 'year')
  ) %>%
    dplyr::mutate(weight = dplyr::if_else(is.na(weight), 0, weight))


  # global weights by region-sector combination. World = 1
  ag_wld_weights <-
    ag_demand_tmp %>%
    dplyr::group_by(Units, scenario, year, ag_price_variable) %>%
    dplyr::mutate(total_demand_var = sum(value)) %>%
    dplyr::ungroup() %>%
    # compute weight by sector and input
    dplyr::mutate(weight = value / total_demand_var) %>%
    # clean dataset
    dplyr::select(dplyr::all_of(gcamreport::long_columns), sector = input,
                  ag_demand_variable = var, var = ag_price_variable, weight, -value)

  sectors_combination <- ag_wld_weights %>%
    dplyr::select(sector, var, ag_demand_variable) %>%
    dplyr::distinct() %>%
    tidyr::expand_grid(
      scenario = unique(ag_wld_weights$scenario),
      region = unique(ag_wld_weights$region),
      year = unique(ag_wld_weights$year)
    )

  ag_wld_weights <- dplyr::left_join(
    sectors_combination,
    ag_wld_weights,
    by = c('sector', 'var', 'ag_demand_variable', 'scenario', 'region', 'year')
  ) %>%
    dplyr::mutate(weight = dplyr::if_else(is.na(weight), 0, weight))


  ag_weights <<- ag_weights
  ag_wld_weights <<- ag_wld_weights
}


#' get_ag_production
#'
#' Get agricultural production.
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal ag
#' @return `ag_production_clean` global variable.
#' @importFrom magrittr %>%
#' @export
get_ag_production <- function(GCAM_version = 'v8.2') {
  Units <- scenario <- region <- year <- var <- value <-
    ag_production_clean <- NULL

  check_queries("ag_production_clean", GCAM_version)

  ag_production_clean <-
    check_inf(rgcam::getQuery(prj, "ag production by crop type"),
              dataset_name = "ag production by crop type") %>%
    rbind(check_inf(rgcam::getQuery(prj, "meat and dairy production by type"),
                    dataset_name = "meat and dairy production by type")) %>%
    # 1 EJ = 1e9 GJ; 1 Million t DM = EJ * 1e9 / (aglu.BIO_ENERGY_CONTENT_GJT * 1e6)
    dplyr::mutate(value = dplyr::if_else(sector == 'biomass', value * 1e3 / get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['aglu.BIO_ENERGY_CONTENT_GJT']], value),
                  Units = dplyr::if_else(sector == 'biomass', 'Mt', Units)) %>%
    dplyr::filter(Units == "Mt") %>%  # Forests produce in units of billion m3
    left_join_strict(get(paste('ag_production_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")),
                     by = c("sector"), mapping = paste('ag_production_map',GCAM_version,sep='_'), multiple = "all", relationship = "many-to-many") %>%
    dplyr::filter(var != 'NoReported', !is.na(var)) %>%
    # add water content data
    left_join_strict(get(paste('water_content'), envir = asNamespace("gcamreport")) %>%
                       dplyr::mutate(sector = GCAM_commodity),
                     by = 'sector') %>%
    # units to annual million t DM
    dplyr::mutate(value = value * (unit_conv - mean_water_content)) %>%
    # filter_variables() %>%
    dplyr::group_by(scenario, region, year, var) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  ag_production_clean <<- ag_production_clean
}


#' get_land
#'
#' Get land use area.
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal ag
#' @return `land_clean` and `land_yield` global variables.
#' @importFrom magrittr %>%
#' @export
get_land <- function(GCAM_version = 'v8.2') {
  value <- unit_conv <- scenario <- region <- year <- var <- land_clean <- land_yield <- landleaf <- NULL

  check_queries("land_clean", GCAM_version)
  cereal_scalar <- get(paste('cereal_scaler',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))

  land_tmp <-
    check_inf(rgcam::getQuery(prj, "land allocation by crop and water source"),
              dataset_name = "land allocation by crop and water source") %>%
    left_join_strict(get(paste('land_use_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")),
                     by = c("crop","water"), mapping = paste('land_use_map',GCAM_version,sep='_'), multiple = "all", relationship = "many-to-many") %>%
    dplyr::filter(var != 'NoReported', !is.na(var)) %>%
    # filter_variables() %>%
    # apply scaler to consider the harvest mistmatch
    dplyr::mutate(GCAM_commodity = crop) %>%
    dplyr::mutate(GCAM_commodity = dplyr::if_else(GCAM_commodity == 'CornC4', 'Corn', GCAM_commodity)) %>%
    dplyr::left_join(cereal_scalar,
                     by = c("region","GCAM_commodity","year")) %>%
    dplyr::distinct()

  # adjustment - goal: keep the total physical land value adjusting the cereal crops (Corn, OtherGrain, Rice, Wheat)
  # so, the physical land of non-cereal (but cropland) crops will be reduced by a ratio (all crops the same percentage)
  land_adj <- land_tmp %>%
    # distinguish between cereal and non-cereal (but cropland) crops; OtherArable is kept constant
    dplyr::mutate(crop_type = dplyr::if_else(GCAM_commodity %in% unique(cereal_scalar$GCAM_commodity),
                                             'cereal',
                                             dplyr::if_else(!GCAM_commodity %in% unique(cereal_scalar$GCAM_commodity) &
                                                              grepl('Land Cover\\|Cropland', var) &
                                                              GCAM_commodity != 'OtherArableLand',
                                                            'cropland', 'other'))) %>%
    dplyr::group_by(dplyr::across(-c('crop_type','var'))) %>%
    dplyr::mutate(crop_type = if ("cropland" %in% crop_type) "cropland" else crop_type) %>%
    dplyr::ungroup() %>%
    # total regional physical land before the cereal adjustment
    dplyr::group_by(Units, scenario, region, year) %>%
    dplyr::mutate(total_annual_R_cereal = sum(value[crop_type == 'cereal']),
                  total_annual_R_cropland = sum(value[crop_type == 'cropland'])) %>%
    dplyr::ungroup() %>%
    # apply the cereal adjustment and compute the ratio to adjust the non-cereal physical lands
    dplyr::mutate(value_adj = dplyr::if_else(!is.na(PhysicalLand_scaler), value * PhysicalLand_scaler, value)) %>%
    dplyr::group_by(Units, scenario, region, year) %>%
    # dplyr::mutate(total_annual_R_adj1 = sum(value_adj)) %>%
    dplyr::mutate(total_annual_R_adj1_cereal = sum(value_adj[crop_type == 'cereal'])) %>%
    dplyr::mutate(ratio_adj1 = (total_annual_R_adj1_cereal - total_annual_R_cereal) / total_annual_R_cropland) %>%
    dplyr::ungroup() %>%
    # apply the ratio to non-cereal (but cropland) physical lands
    dplyr::mutate(value_adj = dplyr::if_else(crop_type == 'cropland',
                                             value_adj * (1 - ratio_adj1), value_adj)) %>%
    # # checker
    # dplyr::group_by(Units, scenario, region, year) %>%
    # dplyr::mutate(checker = sum(value_adj)) %>%
    # dplyr::ungroup() %>%
    # dplyr::mutate(diff_checker = total_annual_R_cereal + total_annual_R_noncereal - checker)
    # thous km2 to million ha
    dplyr::mutate(value = value_adj * unit_conv) %>%
    dplyr::select(-ratio_adj1, -total_annual_R_adj1_cereal, -total_annual_R_cropland, -total_annual_R_cereal, -value_adj, -crop_type)

  land_tmp2 <- land_adj %>%
    dplyr::group_by(scenario, region, year, var) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  # forest area change (annual diff between reported years)
  land_achange <- land_adj %>%
    dplyr::group_by(scenario, region, year, var) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns)) %>%
    dplyr::filter(var == 'Land Cover|Forest') %>%
    dplyr::mutate(year = as.numeric(year),
                  var = 'Forest Area Change') %>%
    dplyr::distinct() %>%
    dplyr::group_by(scenario, region, var) %>%
    dplyr::arrange(year) %>%
    dplyr::mutate(year_diff = year - dplyr::lag(year),
                  value_diff = (value - dplyr::lag(value)) / year_diff) %>%
    dplyr::ungroup() %>%
    dplyr::select(-value) %>%
    dplyr::rename(value = value_diff) %>%
    dplyr::select(all_of(gcamreport::long_columns))


  # aggregate
  land_clean <- rbind(
    land_tmp2,
    land_achange
  )

  # consider land cover to estimate yield (undo the PhysicalLand_scaler and divide
  # by Yield_scaler, so that the final Yield value will be multiplied by Yield_scaler)
  land_yield <- land_adj %>%
    dplyr::mutate(value = dplyr::if_else(!is.na(PhysicalLand_scaler), (value / PhysicalLand_scaler) / Yield_scaler, value))

  land_yield2 <- land_yield %>%
    dplyr::group_by(scenario, region, year, var) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  land_yield <- rbind(
    land_yield2,
    land_achange
  )


  land_clean <<- land_clean
  land_yield <<- land_yield
}


# Primary Energy
# ==============================================================================================
#' get_primary_energy
#'
#' Retrieve primary energy consumption data by technology.
#'@param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal energy
#' @return `primary_energy_clean` global variable.
#' @importFrom magrittr %>%
#' @export
get_primary_energy <- function(GCAM_version = 'v8.2') {
  fuel <- Units <- year <- var <- value <- unit_conv <- scenario <- region <- NULL

  check_queries("primary_energy_clean", GCAM_version)

  primary_energy_clean <-
    check_inf(rgcam::getQuery(prj, "primary energy consumption with CCS by region (direct equivalent)"),
              dataset_name = "primary energy consumption with CCS by region (direct equivalent)") %>%
    dplyr::filter(
      !grepl("water", fuel),
      Units == "EJ"
    ) %>%
    left_join_strict(get(paste('primary_energy_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")),
                     by = c("fuel"), mapping = paste('primary_energy_map',GCAM_version,sep='_'), multiple = "all") %>%
    dplyr::filter(var != 'NoReported', !is.na(var)) %>%
    filter_variables() %>%
    dplyr::mutate(value = value * unit_conv) %>%
    dplyr::group_by(scenario, region, year, var) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    tidyr::complete(tidyr::nesting(scenario, region, year),
                    var = unique(var),
                    fill = list(value = 0)
    ) %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  primary_energy_clean <<- primary_energy_clean
}


#' get_pe_trade_prod
#'
#' Retrieve energy trade data.
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal energy
#' @return `pe_trade_prod` global variable.
#' @importFrom magrittr %>%
#' @export
get_pe_trade_prod <- function(GCAM_version = 'v8.2') {
  Units <- resource <- scenario <- region <- year <- value <-
    pe_trade_prod <- NULL

  check_queries("pe_trade_prod", GCAM_version)

  pe_trade_prod <-
    check_inf(rgcam::getQuery(prj, "resource production"),
              dataset_name = "resource production") %>%
    dplyr::filter(Units == "EJ") %>%
    dplyr::filter(resource %in% c("coal", "natural gas", "crude oil", "unconventional oil")) %>%
    dplyr::mutate(
      resource = sub("crude oil", "oil", resource),
      resource = sub("unconventional oil", "oil", resource)
    ) %>%
    # biomass pe production
    rbind(
      check_inf(rgcam::getQuery(prj, "purpose-grown biomass production"),
                dataset_name = "purpose-grown biomass production") %>%
        dplyr::rename(resource = sector),
      check_inf(rgcam::getQuery(prj, "residue biomass production"),
                dataset_name = "residue biomass production") %>%
        dplyr::select(-sector) %>%
        dplyr::rename(resource = output),
      check_inf(rgcam::getQuery(prj, "MSW production"),
                dataset_name = "MSW production")
    ) %>%
    dplyr::group_by(scenario, resource, region, year) %>%
    dplyr::summarise(production = sum(value)) %>%
    dplyr::ungroup()

  pe_trade_prod <<- pe_trade_prod
}


#' get_pe_trade_supply
#'
#' Retrieve energy trade supply data for calculating other variables.
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal energy tmp
#' @return `pe_trade_supply` global variable.
#' @importFrom magrittr %>%
#' @export
get_pe_trade_supply <- function(GCAM_version = 'v8.2') {
  market <- resource <- scenario <- region <- year <- value <-
    pe_trade_supply <- NULL

  check_queries("pe_trade_supply", GCAM_version)

  pe_trade_supply <- suppressWarnings(
    check_inf(rgcam::getQuery(prj, "supply of all markets"),
              dataset_name = "supply of all markets") %>%
      dplyr::filter(grepl("regional coal", market) | grepl("regional natural gas", market) |
                      grepl("regional oil", market) | grepl("regional biomass", market)) %>%
      tidyr::separate(market, into = c("region", "resource"), sep = "regional ", fill = "right") %>%
      dplyr::filter(resource != "oilpalm", resource != "oilcrop", resource != "biomassOil") %>%
      dplyr::group_by(scenario, resource, region, year) %>%
      dplyr::summarise(demand = sum(value)) %>%
      dplyr::ungroup()
  )

  pe_trade_supply <<- pe_trade_supply
}


#' get_pe_trade
#'
#' Retrieve primary energy trade.
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal energy tmp
#' @return `pe_trade` global variable
#' @importFrom magrittr %>%
#' @export
get_pe_trade <- function(GCAM_version = 'v8.2') {
  production <- demand <- resource <- pe_trade <- NULL

  check_queries("pe_trade", GCAM_version)

  pe_trade <-
    pe_trade_prod %>%
    left_join_strict(pe_trade_supply, by = c("scenario", "resource", "region", "year")) %>%
    dplyr::mutate(
      value = production - demand,
      resource = sub("biomass", "Biomass", resource),
      resource = sub("coal", "Coal", resource),
      resource = sub("natural gas", "Gas", resource),
      resource = sub("oil", "Oil", resource),
      var = paste0("Trade|Primary Energy|", resource, " [Volume]")
    ) %>%
    filter_variables() %>%
    dplyr::filter(year > 1975) %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  pe_trade <<- pe_trade
}

# Secondary Energy
# ==============================================================================================
#' get_elec_gen_tech
#'
#' Retrieve electricity generation.
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal electricity
#' @return `secondary_energy_clean` and `secondary_energy_raw` global variables
#' @importFrom magrittr %>%
#' @export
get_elec_gen_tech <- function(GCAM_version = 'v8.2') {
  var <- value <- unit_conv <- scenario <- region <- year <-
    secondary_energy_clean <- secondary_energy_raw <- NULL

  check_queries("secondary_energy_clean", GCAM_version)
  check_queries("secondary_energy_raw", GCAM_version)

  # deal with grid regions:
  # split the subsector "region_name load generation" and substitute the
  # grid region for the market name
  elec_gen_tmp <- check_inf(rgcam::getQuery(prj, "elec gen by gen tech"),
                            dataset_name = "elec gen by gen tech")
  if (any(grepl("generation", unique(elec_gen_tmp$subsector)))) {
    elec_gen_grids <- elec_gen_tmp %>%
      dplyr::filter(grepl('generation', subsector)) %>%
      dplyr::mutate(load_type = stringr::str_extract(subsector, "base load generation|intermediate generation|peak generation|subpeak generation"),
                    region = stringr::str_squish(stringr::str_remove(subsector, load_type))) %>%
      dplyr::select(Units,scenario,region,subsector=load_type,technology=load_type,year,value)

    elec_gen_tmp <- rbind(
      elec_gen_tmp %>%
        dplyr::filter(!grepl('generation', subsector)),
      elec_gen_grids
    )
  }

  secondary_energy_raw1 <- rbind(
    elec_gen_tmp %>%
      dplyr::mutate(output = 'electricity'),
    dplyr::bind_rows(
      check_inf(rgcam::getQuery(prj, "gas production by tech"),
                dataset_name = "gas production by tech"),
      check_inf(rgcam::getQuery(prj, "hydrogen production by tech"),
                dataset_name = "hydrogen production by tech"),
      check_inf(rgcam::getQuery(prj, "district heat production by subsector (fuel)"),
                dataset_name = "district heat production by subsector (fuel)") %>%
        dplyr::mutate(technology = subsector) %>%
        dplyr::select(-output),
      check_inf(rgcam::getQuery(prj, "refined liquids production by tech"),
                dataset_name = "refined liquids production by tech") %>%
        dplyr::select(-output)
    ) %>%
      dplyr::rename(output = sector)
  ) %>%
    left_join_strict(get(paste('secondary_energy_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")),
                     by = c("output", "subsector", "technology"), mapping = paste('secondary_energy_map',GCAM_version,sep='_'), multiple = "all") %>%
    dplyr::filter(var != 'NoReported', !is.na(var)) %>%
    dplyr::mutate(value = value * unit_conv)

  if (!any(grepl('Trade|Investment|Capacity|All',desired_variables.global))) {
    secondary_energy_raw <- secondary_energy_raw1 %>%
      filter_variables()
  } else {
    secondary_energy_raw <- secondary_energy_raw1
  }
  secondary_energy_clean <- secondary_energy_raw %>%
    dplyr::group_by(scenario, region, year, var) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    tidyr::complete(tidyr::nesting(scenario, region, year),
                    var = unique(var),
                    fill = list(value = 0)
    ) %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns)) %>%
    dplyr::bind_rows(secondary_solids)

  secondary_energy_raw <<- secondary_energy_raw
  secondary_energy_clean <<- secondary_energy_clean
}


#' get_secondary_solids
#'
#' Retrieve secondary solids.
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal energy
#' @return `secondary_solids` global variable
#' @importFrom magrittr %>%
#' @export
get_secondary_solids <- function(GCAM_version = 'v8.2') {
  input <- scenario <- region <- year <- value <- secondary_solids <- NULL

  check_queries("secondary_solids", GCAM_version)

  secondary_solids <-
    check_inf(rgcam::getQuery(prj, "inputs by sector"),
              dataset_name = "inputs by sector") %>%
    dplyr::filter(input %in% c("delivered biomass", "delivered coal")) %>%
    dplyr::group_by(scenario, region, year, input) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(var = ifelse(input == "delivered biomass", "Secondary Energy|Solids|Biomass",
                               "Secondary Energy|Solids|Coal"
    )) %>%
    dplyr::bind_rows(check_inf(rgcam::getQuery(prj, "inputs by sector"),
                               dataset_name = "inputs by sector") %>%
                       dplyr::filter(input %in% c("delivered biomass", "delivered coal")) %>%
                       dplyr::group_by(scenario, region, year) %>%
                       dplyr::summarise(value = sum(value, na.rm = T)) %>%
                       dplyr::ungroup() %>%
                       dplyr::mutate(var = "Secondary Energy|Solids")) %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  secondary_solids <<- secondary_solids
}


# Final Energy
# ==============================================================================================
# demand by sector by technology



#' get_consumption_hh
#'
#' Compute share of total energy household consumption my decile
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @return `consumption_hh_clean` global variables.
#' @keywords internal deciles
#' @importFrom magrittr %>%
#' @export
get_consumption_hh <- function(GCAM_version = 'v8.2') {
  consumption_hh_clean <- consumption_hh_w <- consumption_hh <-
    consumption_hh_tmp <- NULL

  check_queries('consumption_hh_clean', GCAM_version)

  if (GCAM_version %in% c(get('deciles_GCAM_versions', envir = asNamespace("gcamreport")))) {
    consumption_hh_tmp <-
      check_inf(rgcam::getQuery(prj, "building total final energy by service"),
                dataset_name = "building total final energy by service") %>%
      dplyr::filter(grepl('resid',sector)) %>%
      # update variable name
      dplyr::mutate(var = paste0('Consumption|Housing|Energy|D',
                                 stringr::str_extract(sector, "(?<=_d)\\d+"),
                                 ' [Share]')) %>%
      dplyr::group_by(scenario, region, year, var) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::ungroup()

    # compute consumption share by decile
    consumption_hh <- consumption_hh_tmp %>%
      dplyr::group_by(scenario, region, year) %>%
      dplyr::mutate(total = sum(value)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(value = 100 * value / total) %>%
      dplyr::select(dplyr::all_of(gcamreport::long_columns))

    # compute consumption share by decile at the World level
    consumption_hh_w <- consumption_hh_tmp %>%
      dplyr::group_by(scenario, year, var) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::ungroup() %>%
      dplyr::group_by(scenario, year) %>%
      dplyr::mutate(total = sum(value)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(value = 100 * value / total,
                    region = 'World') %>%
      dplyr::select(dplyr::all_of(gcamreport::long_columns))

    # aggregate regional and global data
    consumption_hh_clean <- rbind(
      consumption_hh,
      consumption_hh_w
    )
  } else {
    consumption_hh_clean <- NULL
    warning("The 'Household Consumption by Decile' variables are unavailable in your project. They are only supported from GCAM version 7.1 onwards. If you are using version 7.1 or newer, please ensure the `building total final energy by service` query is valid and not returning empty results.")
  }

  consumption_hh_clean <<- consumption_hh_clean

}



#' get_fe_sector_tmp
#'
#' Retrieve final energy demand by sector.
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal energy tmp
#' @return `fe_sector` and `fe_sector_raw` global variables
#' @importFrom magrittr %>%
#' @export
get_fe_sector_tmp <- function(GCAM_version = 'v8.2') {
  var <- value <- unit_conv <- scenario <- region <- year <- tmp <-
    sector <- Units <- input <- fe_sector <- fe_sector_raw <- NULL

  check_queries("fe_sector", GCAM_version)
  check_queries("fe_sector_raw", GCAM_version)

  # gather deciles if necessary
  tmp <- check_inf(rgcam::getQuery(prj, "final energy consumption by sector and fuel"),
                   dataset_name = "final energy consumption by sector and fuel") %>%
    dplyr::filter(!stringr::str_starts(sector, 'trn'))
  if(GCAM_version %in% get('deciles_GCAM_versions', envir = asNamespace("gcamreport"))) {
    tmp <- tmp %>%
      tidyr::separate(sector, into = c("sector", "decile"), sep = "_d", extra = "merge", fill = "right") %>%
      dplyr::group_by(Units, scenario, region, sector, input, year) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::ungroup()
  }

  fe_sector_raw <-
    tmp %>%
    left_join_strict(get(paste('final_energy_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")),
                     by = c("sector", "input"), mapping = paste('final_energy_map',GCAM_version,sep='_'), multiple = "all") %>%
    dplyr::filter(var != 'NoReported', !is.na(var)) %>%
    filter_variables() %>%
    dplyr::mutate(value = value * unit_conv)

  fe_sector <- fe_sector_raw %>%
    dplyr::group_by(scenario, region, year, var) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    tidyr::complete(tidyr::nesting(scenario, region, year),
                    var = unique(var),
                    fill = list(value = 0)
    ) %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  fe_sector_raw <<- fe_sector_raw
  fe_sector <<- fe_sector
}


#' get_fe_transportation_tmp
#'
#' Retrieve mode-specific transport final energy, including rail, ship, and domestic air.
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal energy  tmp
#' @return `fe_transportation` and `fe_transportation_raw` global variables
#' @importFrom magrittr %>%
#' @export
get_fe_transportation_tmp <- function(GCAM_version = 'v8.2') {
  var <- value <- unit_conv <- scenario <- region <- year <-
    fe_transportation <- fe_transportation_raw <- NULL

  check_queries("fe_transportation", GCAM_version)
  check_queries("fe_transportation_raw", GCAM_version)

  fe_transportation_raw <-
    check_inf(rgcam::getQuery(prj, "transport final energy by mode and fuel"),
              dataset_name = "transport final energy by mode and fuel") %>%
    left_join_strict(get(paste('transport_final_en_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")),
                     by = c("sector", "input", "mode"), mapping = paste('transport_final_en_map',GCAM_version,sep='_'), multiple = "all") %>%
    dplyr::filter(var != 'NoReported', !is.na(var)) %>%
    filter_variables() %>%
    dplyr::mutate(value = value * unit_conv)

  fe_transportation <- fe_transportation_raw %>%
    dplyr::group_by(scenario, region, year, var) %>%
    dplyr::summarise(value = sum(value,
                                 na.rm = T
    )) %>%
    dplyr::ungroup() %>%
    tidyr::complete(tidyr::nesting(scenario, region, year),
                    var = unique(var),
                    fill = list(value = 0)
    ) %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  fe_transportation_raw <<- fe_transportation_raw
  fe_transportation <<- fe_transportation
}


#' Compute and aggregate final energy by sector.
#'
#' This function consolidates final energy data by handling overlaps between sector-level and subsector-level queries.
#' For instance, both international and domestic air transport are categorized under aviation, but are sourced from different queries:
#' international from the sector-level and domestic from the subsector-level. This aggregation step prevents duplicate entries
#' with inconsistent data for the same reporting categories.
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal energy process
#' @return `fe_sector_clean` global variable.
#' @importFrom magrittr %>%
#' @export
get_fe_sector <- function(GCAM_version = 'v8.2') {
  scenario <- region <- var <- year <- value <- fe_sector_clean <- NULL

  check_queries("fe_sector_clean", GCAM_version)

  fe_sector_clean <-
    dplyr::bind_rows(fe_sector, fe_transportation) %>%
    dplyr::group_by(scenario, region, var, year) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup()

  fe_sector_clean <<- fe_sector_clean
}

#' get_total_trade
#'
#' Retrieve total trade.
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal energy tmp
#' @return `trade_clean` global variable
#' @importFrom magrittr %>%
#' @export
get_total_trade <- function(GCAM_version = 'v8.2') {
  trade_clean <- NULL

  check_queries("trade_clean", GCAM_version)

  trade_clean <- rbind(
    ag_trade,
    pe_trade
  )

  trade_clean <<- trade_clean
}

# Energy Service ----------------------------------------------------------

#' get_energy_service_transportation
#'
#' Retrieve transport service.
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal energy
#' @return `energy_service_transportation_clean` global variable
#' @importFrom magrittr %>%
#' @export
get_energy_service_transportation <- function(GCAM_version = 'v8.2') {
  var <- value <- unit_conv <- scenario <- region <- year <-
    energy_service_transportation_clean <- NULL

  check_queries("energy_service_transportation_clean", GCAM_version)

  energy_service_transportation <-
    check_inf(rgcam::getQuery(prj, "transport service output by tech and vintage"),
              dataset_name = "transport service output by tech and vintage") %>%
    tidyr::separate(technology, into = c("technology", NA), sep = ",year") %>%
    dplyr::group_by(dplyr::across(-value)) %>%
    dplyr::summarise(value = sum(value), .groups = 'drop') %>%
    left_join_strict(get(paste('transport_en_service',GCAM_version,sep='_'), envir = asNamespace("gcamreport")),
                     by = c("sector", "subsector", "technology"),
                     mapping = paste('transport_en_service',GCAM_version,sep='_'), multiple = "all") %>%
    dplyr::filter(var != 'NoReported', !is.na(var)) %>%
    filter_variables() %>%
    # from million km/yr to billion km/yr
    dplyr::mutate(value = value * unit_conv * get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_million_billion']]) %>%
    dplyr::group_by(scenario, region, year, var) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  # compute Active & Public share (regional)
  energy_service_transportation_share <-
    energy_service_transportation %>%
    dplyr::filter((grepl("Passenger", var) & grepl("[Share]",var, fixed = TRUE)) |
                    var == "Energy Service|Transportation|Passenger") %>%
    dplyr::group_by(scenario, region, year) %>%
    dplyr::mutate(
      total_p = value[var == "Energy Service|Transportation|Passenger"],
      ratio_active_p = dplyr::if_else(var == "Energy Service|Transportation|Passenger|Active Transport [Share]", 100 * value / total_p, NA),
      ratio_public_p = dplyr::if_else(var == "Energy Service|Transportation|Passenger|Public Transport [Share]", 100 * value / total_p, NA)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(value = dplyr::if_else(var == "Energy Service|Transportation|Passenger|Active Transport [Share]", ratio_active_p,
                                         dplyr::if_else(var == "Energy Service|Transportation|Passenger|Public Transport [Share]", ratio_public_p,
                                                        value))) %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  # compute Active & Public share (World)
  energy_service_transportation_share_w <- energy_service_transportation %>%
    dplyr::filter((grepl("Passenger", var) & grepl("[Share]",var, fixed = TRUE)) |
                    var == "Energy Service|Transportation|Passenger") %>%
    dplyr::group_by(scenario, year, var) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(scenario, year) %>%
    dplyr::mutate(
      total_p = value[var == "Energy Service|Transportation|Passenger"],
      ratio_active_p = dplyr::if_else(var == "Energy Service|Transportation|Passenger|Active Transport [Share]", 100 * value / total_p, NA),
      ratio_public_p = dplyr::if_else(var == "Energy Service|Transportation|Passenger|Public Transport [Share]", 100 * value / total_p, NA)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(value = dplyr::if_else(var == "Energy Service|Transportation|Passenger|Active Transport [Share]", ratio_active_p,
                                         dplyr::if_else(var == "Energy Service|Transportation|Passenger|Public Transport [Share]", ratio_public_p,
                                                        value)),
                  region = 'World') %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))


  energy_service_transportation_clean <- dplyr::bind_rows(
    energy_service_transportation %>%
      dplyr::filter(!grepl('Share', var)),
    energy_service_transportation_share %>%
      dplyr::filter(grepl('Share', var)),
    energy_service_transportation_share_w %>%
      dplyr::filter(grepl('Share', var))
  )

  energy_service_transportation_clean <<- energy_service_transportation_clean
}


#' get_energy_service_buildings
#'
#' Get ES buildings.
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal energy
#' @return `floor_space_clean` global variable
#' @importFrom magrittr %>%
#' @export
get_floor_space <- function(GCAM_version = 'v8.2') {
  var <- value <- unit_conv <- scenario <- region <- year <- building <-
    nodeinput <- Units <- floor_space_clean <- NULL

  check_queries("floor_space_clean", GCAM_version)

  # gather deciles if necessary
  tmp <- check_inf(rgcam::getQuery(prj, "building floorspace"),
                   dataset_name = "building floorspace")
  if(GCAM_version %in% get('deciles_GCAM_versions', envir = asNamespace("gcamreport"))) {
    tmp <- tmp %>%
      tidyr::separate(building, into = c("building", "decile"), sep = "_d", extra = "merge", fill = "right") %>%
      dplyr::group_by(Units, scenario, region, building, nodeinput, `building-node-input`, year) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::ungroup()
  }

  floor_space_clean <-
    tmp %>%
    left_join_strict(get(paste('buildings_en_service',GCAM_version,sep='_'), envir = asNamespace("gcamreport")),
                     by = c("building"), mapping = paste('buildings_en_service',GCAM_version,sep='_'), multiple = "all") %>%
    dplyr::filter(var != 'NoReported', !is.na(var)) %>%
    filter_variables() %>%
    dplyr::mutate(value = value * unit_conv) %>%
    dplyr::group_by(scenario, region, year, var) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  floor_space_clean <<- floor_space_clean
}



#' get_hdd_cdd
#'
#' Get HDD and CDD.
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal energy
#' @return `hdd_cdd_clean` global variable
#' @importFrom magrittr %>%
#' @export
get_hdd_cdd <- function(GCAM_version = 'v8.2') {
  hdd_cdd_clean <- NULL

  check_queries("hdd_cdd_clean", GCAM_version)

  # base data
  tmp <- get(paste('hdd_cdd',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))
  hdd_cdd_clean <- tmp %>%
    dplyr::mutate(scenario = scenarios.global[1])

  for (sc in scenarios.global[-1]) {
    hdd_cdd_clean <- rbind(
      hdd_cdd_clean,
      tmp %>%
        dplyr::mutate(scenario = sc)
    )
  }

  hdd_cdd_clean <<- hdd_cdd_clean
}



# industry production
# could add chemicals but they're in terms of EJ, need to also add cement

#' get_industry_production
#'
#' Retrieve industry production.
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal energy
#' @return `industry_production_clean` global variable
#' @importFrom magrittr %>%
#' @export
get_industry_production <- function(GCAM_version = 'v8.2') {
  var <- scenario <- region <- year <- value <- industry_production_clean <- NULL

  check_queries("industry_production_clean", GCAM_version)

  industry_production_clean <-
    check_inf(rgcam::getQuery(prj, "industry primary output by sector"),
              dataset_name = "industry primary output by sector") %>%
    dplyr::mutate(sector = dplyr::if_else(grepl('chemical',sector), 'chemical', sector)) %>%
    left_join_strict(get(paste('production_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")),
                     by = c("sector"), mapping = paste('production_map',GCAM_version,sep='_')) %>%
    dplyr::filter(var != 'NoReported', !is.na(var)) %>%
    filter_variables() %>%
    dplyr::group_by(scenario, region, var, year) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  industry_production_clean <<- industry_production_clean
}

#' get_iron_steel_imports
#'
#' Retrieve iron steel imports.
#' @keywords internal industry tmp
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @return `iron_steel_imports` global variable
#' @importFrom magrittr %>%
#' @export
get_iron_steel_imports <- function(GCAM_version = 'v8.2') {
  var <- scenario <- region <- year <- value <- subsector <- iron_steel_imports <- NULL

  check_queries("iron_steel_imports", GCAM_version)

  iron_steel_imports <-
    check_inf(rgcam::getQuery(prj, "regional iron and steel sources"),
              dataset_name = "regional iron and steel sources") %>%
    dplyr::filter(subsector == "domestic iron and steel") %>%
    left_join_error_no_match(get(paste('iron_steel_trade_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")), by = c("sector")) %>%
    # filter variables that are in terms of Mt
    dplyr::group_by(scenario, region, var, year) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  iron_steel_imports <<- iron_steel_imports
}

#' get_iron_steel_exports
#'
#' Retrieve iron steel production.
#' @keywords internal industry tmp
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @return `iron_steel_exports` global variable
#' @importFrom magrittr %>%
#' @export
get_iron_steel_exports <- function(GCAM_version = 'v8.2') {
  var <- scenario <- region <- year <- value <- iron_steel_exports <- NULL

  check_queries("iron_steel_exports", GCAM_version)

  iron_steel_exports <-
    check_inf(rgcam::getQuery(prj, "traded iron and steel"),
              dataset_name = "traded iron and steel") %>%
    left_join_error_no_match(get(paste('iron_steel_trade_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")), by = c("sector")) %>%
    # extract region
    dplyr::mutate(region = stringr::str_replace_all(subsector, " traded iron and steel", "")) %>%
    filter_desired_regions() %>%
    # filter variables that are in terms of Mt
    dplyr::group_by(scenario, region, var, year) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  iron_steel_exports <<- iron_steel_exports
}

#' get_iron_steel_clean
#'
#' Retrieve iron steel imports & exports
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal industry
#' @return `iron_steel_clean` global variable
#' @importFrom magrittr %>%
#' @export
get_iron_steel_clean <- function(GCAM_version = 'v8.2') {
  iron_steel_clean <- NULL

  check_queries("iron_steel_clean", GCAM_version)

  iron_steel_clean <- dplyr::bind_rows(
    iron_steel_imports,
    iron_steel_exports
  )

  iron_steel_clean <<- iron_steel_clean
}


# Prices
# ==============================================================================================

#' get_ag_price_wld_tmp
#'
#' Retrieve agricultural price index.
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal ag tmp
#' @return `ag_price_wld` global variable
#' @importFrom magrittr %>%
#' @export
get_ag_price_wld_tmp <- function(GCAM_version = 'v8.2') {
  var <- scenario <- sector <- year <- value <- ag_price_map <-
    ag_demand_reg_sec_weights <- ag_price_variable <- ag_price_wld <-
    ag_demand_variable <- Units <- region <- unit_conv <- NULL

  check_queries("ag_price_wld", GCAM_version)

  ag_price_wld <-
    check_inf(rgcam::getQuery(prj, "prices by sector"),
              dataset_name = "prices by sector") %>%
    dplyr::filter(Units == "1975$/kg" | sector == 'biomass') %>%
    dplyr::filter(!grepl('CO2', sector)) %>%
    left_join_strict(get(paste('ag_price_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")),
                     by = c("sector"), mapping = paste('ag_price_map',GCAM_version,sep='_'),
                     relationship = "many-to-many") %>%
    dplyr::filter(var != 'NoReported', !is.na(var)) %>%
    filter_variables() %>%
    interpolateGCAMdata(year_to_appear = 2020) %>%
    # compute index for non biomass items. Biomass units: from 1975$/GJ to 2010$GJ
    dplyr::group_by(scenario, region, sector) %>%
    dplyr::mutate(value = dplyr::if_else(sector != 'biomass',
                                         value * unit_conv / value[year == 2020],
                                         value * get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_75USD_10USD']])) %>%
    dplyr::mutate(value = dplyr::if_else(is.na(value), 0, value)) %>%
    dplyr::ungroup() %>%
    # add weights
    dplyr::filter(sector != 'FeedCrops',
                  year %in% gcam_years[gcam_years <= min(final_year.global, max(unique(ag_wld_weights$year)))],
                  var %in% unique(ag_wld_weights$var)) %>%
    left_join_strict(ag_wld_weights %>%
                       left_join_strict(get(paste('food_items_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")) %>%
                                          dplyr::rename(sector = regional_item),
                                        by = c('sector'), mapping = paste('food_items_map',GCAM_version,sep='_')) %>%
                       dplyr::filter(var != 'NoReported', !is.na(var)) %>%
                       filter_variables() %>%
                       dplyr::select(-sector) %>%
                       dplyr::rename(sector = item),
                     by = c('scenario','sector','var','region','year'),
                     by_message = c('sector','var','region','year')) %>%
    dplyr::filter(var != 'NoReported', !is.na(var)) %>%
    filter_variables() %>%
    # compute var weighted average price
    dplyr::mutate(value = value * weight) %>%
    dplyr::group_by(scenario, var, year) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    # rearrange dataset
    dplyr::mutate(region = 'World') %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  ag_price_wld <<- ag_price_wld
}


#' get_ag_price
#'
#' Calculate average mean for agricultural global index.
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal ag
#' @return `ag_price_clean` global variable
#' @importFrom magrittr %>%
#' @export
get_ag_price <- function(GCAM_version = 'v8.2') {
  var <- scenario <- region <- sector <- value <- unit_conv <- year <- Units <- NULL

  check_queries("ag_price_clean", GCAM_version)

  ag_price_clean <-
    check_inf(rgcam::getQuery(prj, "prices by sector"),
              dataset_name = "prices by sector") %>%
    dplyr::filter(Units == "1975$/kg" | sector == 'biomass') %>%
    dplyr::filter(!grepl('CO2', sector)) %>%
    left_join_strict(get(paste('ag_price_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")),
                     by = c("sector"), mapping = paste('ag_price_map',GCAM_version,sep='_')) %>%
    dplyr::filter(var != 'NoReported', !is.na(var)) %>%
    filter_variables() %>%
    interpolateGCAMdata(year_to_appear = 2020) %>%
    # compute index for non biomass items. Biomass units: from 1975$/GJ to 2010$GJ
    dplyr::group_by(scenario, region, sector) %>%
    dplyr::mutate(value = dplyr::if_else(sector != 'biomass',
                                         value * unit_conv / value[year == 2020],
                                         value * get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_75USD_10USD']])) %>%
    dplyr::mutate(value = dplyr::if_else(is.na(value), 0, value)) %>%
    dplyr::ungroup() %>%
    # add weights
    dplyr::filter(sector != 'FeedCrops',
                  year %in% gcam_years[gcam_years <= min(final_year.global, max(unique(ag_wld_weights$year)))],
                  var %in% unique(ag_weights$var)) %>%
    left_join_strict(ag_weights %>%
                       left_join_strict(get(paste('food_items_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")) %>%
                                          dplyr::rename(sector = regional_item),
                                        by = c('sector'), mapping = paste('food_items_map',GCAM_version,sep='_')) %>%
                       dplyr::filter(var != 'NoReported', !is.na(var)) %>%
                       filter_variables() %>%
                       dplyr::select(-sector) %>%
                       dplyr::rename(sector = item),
                     by_message = c('sector','var','year','region')) %>%
    dplyr::filter(var != 'NoReported', !is.na(var)) %>%
    filter_variables() %>%
    # compute var weighted average price
    dplyr::mutate(value = value * weight) %>%
    dplyr::group_by(scenario, region, var, year) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    # rearrange dataset
    dplyr::select(dplyr::all_of(gcamreport::long_columns)) %>%
    # add World values
    rbind(ag_price_wld)

  ag_price_clean <<- ag_price_clean
}


# calculate co2 price for all scenarios except for d_rap and d_delfrag

#' get_price_var_tmp
#'
#' Retrieve price variables to compute carbon price.
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal internal tmp process
#' @return `price_var` global variable
#' @importFrom magrittr %>%
#' @export
get_price_var_tmp <- function(GCAM_version = 'v8.2') {
  price_var <- NULL

  check_queries("price_var", GCAM_version)

  price_var <-
    unique(get(paste('co2_market_frag_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))$var)

  price_var <<- price_var
}



#' get_regions_tmp
#'
#' Retrieve regions for carbon price computation. This function fetches the regions necessary for calculating carbon prices.
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal tmp process
#' @return `regions.global` global variable.
#' @importFrom magrittr %>%
#' @export
get_regions_tmp <- function(GCAM_version = 'v8.2') {
  regions.global <- NULL

  check_queries("regions.global", GCAM_version)

  CO2_market_filteredReg <- filter_data_regions(get(paste('co2_market',GCAM_version,sep='_'), envir = asNamespace("gcamreport")), GCAM_version = GCAM_version) %>%
    dplyr::filter(region != 'NoReported')
  regions.global <-
    unique(CO2_market_filteredReg$region)

  regions.global <<- regions.global
}



#' get_co2_price_global_tmp
#'
#' Retrieve global co2 price.
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal co2 tmp
#' @return `co2_price_global` global variable
#' @importFrom magrittr %>%
#' @export
get_co2_price_global_tmp <- function(GCAM_version = 'v8.2') {
  market <- value <- co2_price_global_pre <- regions <- co2_price_global <- NULL

  check_queries("co2_price_global", GCAM_version)

  co2_price_global_pre <-
    check_inf(rgcam::getQuery(prj, "CO2 prices"),
              dataset_name = "CO2 prices") %>%
    dplyr::filter(market %in% c("WorldCO2","globalCO2","GlobalCO2","worldCO2"))

  if (nrow(co2_price_global_pre) > 1) {
    co2_price_global <-
      tibble::as_tibble(co2_price_global_pre) %>%
      dplyr::mutate(value = value /
                      get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_C_CO2']] *
                      get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_90USD_10USD']]
      ) %>%
      dplyr::mutate(market = gsub("global|Global|world|World", "", market)) %>%
      left_join_strict(get(paste('co2_market_frag_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")),
                       by = "market", mapping = paste('co2_market_frag_map',GCAM_version,sep='_'), multiple = "all") %>%
      dplyr::filter(var != 'NoReported', !is.na(var)) %>%
      tidyr::expand_grid(tibble::tibble(region = unique(co2_emiss$region))) %>%
      dplyr::select(dplyr::all_of(gcamreport::long_columns))
  } else {
    co2_price_global <- NULL
  }

  co2_price_global <<- co2_price_global
}


#' Get CO2 Price Share
#'
#' Retrieves the CO2 price share of each region or sector compared to the total CO2 price.
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal co2 tmp
#' @return Global variable `co2_price_share_bysec` containing CO2 price shares by sector.
#' @importFrom magrittr %>%
#' @export
get_co2_price_share_bysec <- function(GCAM_version = 'v8.2') {
  var <- year <- region <- value <- . <- sector <- co2_price_share_bysec <-
    CO2 <- scenario <- share_CO2_ETS <- NULL

  check_queries("co2_price_share_bysec", GCAM_version)

  co2_price_share_bysec_tmp <- co2_emiss %>%
    dplyr::filter(year == get(paste('last_historical_year',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))) %>%
    rbind(co2_ets_bysec %>%
            dplyr::filter(year == get(paste('last_historical_year',GCAM_version,sep='_'), envir = asNamespace("gcamreport")))) %>%
    # select only reported sectors and do a right join, so that all sectors are present,
    # even if the value is NA
    dplyr::right_join(expand.grid(
      var = c(
        "Emissions|CO2|Energy and Industrial Processes",
        "Emissions|CO2|Energy|Demand|Industry",
        "Emissions|CO2|Energy|Demand|Transportation",
        "Emissions|CO2|Energy|Demand|Residential and Commercial",
        "Emissions|CO2|Energy|Supply",
        "Emissions|CO2_ETS|Energy and Industrial Processes",
        "Emissions|CO2_ETS|Energy|Demand|Industry",
        "Emissions|CO2_ETS|Energy|Demand|Transportation",
        "Emissions|CO2_ETS|Energy|Demand|Residential and Commercial",
        "Emissions|CO2_ETS|Energy|Supply"
      ),
      region = unique(co2_emiss$region),
      scenario = unique(co2_emiss$scenario),
      year = get(paste('last_historical_year',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))
    ),
    by = c('scenario', 'region', 'var', 'year')) %>%
    dplyr::mutate(value = dplyr::if_else(is.na(value), 0, value)) %>%
    # compute the share
    dplyr::mutate(sector = sub(".*\\|([^|]+)$", "\\1", var),
                  ghg = sub("Emissions\\|([^|]+)\\|.*$", "\\1", var)) %>%
    dplyr::select(-var) %>%
    dplyr::distinct(.)

  # compute the CO2 ETS vs CO2 sectorial emission shares
  co2_price_share_bysec_share_CO2_ETS <- co2_price_share_bysec_tmp %>%
    tidyr::pivot_wider(names_from = "ghg", values_from = "value") %>%
    dplyr::mutate(CO2_ETS = dplyr::if_else(is.na(CO2_ETS), 0, CO2_ETS)) %>%
    dplyr::mutate(share_CO2_ETS = dplyr::if_else(CO2 == 0, 0, CO2_ETS / CO2)) %>%
    # if the share is > 1, set it to 1 (seems that "biomass" is not accounted in the CO2 emissions query)
    dplyr::mutate(share_CO2_ETS = dplyr::if_else(share_CO2_ETS > 1, 1, share_CO2_ETS)) %>%
    dplyr::select(scenario, region, year, sector, share_CO2_ETS)

  # compute the sectorial vs World CO2 emission shares
  co2_price_share_bysec_share_CO2_world <- co2_price_share_bysec_tmp %>%
    dplyr::group_by(scenario, sector, year) %>%
    dplyr::mutate(global_value = sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(ghg == 'CO2') %>%  # only CO2 emissions are accounted to do the regional shares, since CO2_ETS is already accounted
    # compute shares
    dplyr::mutate(share_CO2_world = value / global_value) %>%
    dplyr::select(scenario, region, year, sector, share_CO2_world)

  co2_price_share_bysec <- merge(
    co2_price_share_bysec_share_CO2_ETS,
    co2_price_share_bysec_share_CO2_world,
    by = c('scenario','region','year','sector')
  )

  co2_price_share_bysec <<- co2_price_share_bysec
}



#' get_co2_price_fragmented_tmp
#'
#' Retrieve CO2 fragmented price.
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal co2 tmp
#' @return `co2_price_fragmented` global variable
#' @importFrom magrittr %>%
#' @export
get_co2_price_fragmented_tmp <- function(GCAM_version = 'v8.2') {
  market <- Units <- regions <- year <- value <- market_adj <- scenario <-
    region <- CO2 <- sector <- var <- CO2_market_filteredReg <-
    co2_price_fragmented <- NULL

  check_queries("co2_price_fragmented", GCAM_version)

  co2_price_fragmented_pre <-
    check_inf(rgcam::getQuery(prj, "CO2 prices"),
              dataset_name = "CO2 prices") %>%
    dplyr::filter(!grepl("LUC", market)) %>%
    dplyr::filter(!grepl("global|Global|world|World", market)) %>%
    dplyr::filter(Units == "1990$/tC") %>%
    tibble::as_tibble() %>%
    dplyr::filter(!grepl(paste(.myGlobals$ignore.global, collapse = "|"), market))


  if (nrow(co2_price_fragmented_pre) > 1) {
    CO2_market_filteredReg <- filter_data_regions(get(paste('co2_market',GCAM_version,sep='_'), envir = asNamespace("gcamreport")), GCAM_version = GCAM_version) %>%
      dplyr::filter(region != 'NoReported') %>%
      dplyr::filter(!grepl(paste(.myGlobals$ignore.global, collapse = "|"), market))

    co2_price_fragmented <-
      co2_price_fragmented_pre %>%
      dplyr::left_join(CO2_market_filteredReg, by = c("market"), multiple = "all") %>%
      dplyr::filter(stats::complete.cases(.)) %>%
      dplyr::mutate(value = value /
                      get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_C_CO2']] *
                      get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_90USD_10USD']]
      ) %>%
      dplyr::mutate(
        market_adj = "CO2",
        market_adj = dplyr::if_else(grepl("ETS", market), "CO2_ETS", market_adj),
        market_adj = dplyr::if_else(grepl("CO2BLD|CO2_building", market), "CO2BLD", market_adj),
        market_adj = dplyr::if_else(grepl("CO2IND|CO2_industry|CO2_cement", market), "CO2_ETS", market_adj),
        market_adj = dplyr::if_else(grepl("CO2TRAN|CO2_transport", market), "CO2TRAN", market_adj)
      ) %>%
      # Remove attribution of "Rest of World" (ROW) carbon prices to regions with region-specific carbon prices
      dplyr::group_by(dplyr::across(c(-value, -market))) %>%
      dplyr::mutate(group_size = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::filter(!(grepl("ROW", market) & group_size > 1)) %>%
      # consider the value sum of by market
      dplyr::group_by(Units, scenario, year, market_adj, region) %>%
      dplyr::mutate(value = sum(value)) %>%
      dplyr::ungroup() %>%
      # apply the share between CO2 and CO2_ETS
      dplyr::select(-market) %>%
      dplyr::distinct() %>%
      tidyr::pivot_wider(names_from = "market_adj", values_from = "value") %>%
      dplyr::mutate(dplyr::across(5:length(colnames(.)), ~ ifelse(is.na(.), 0, .))) %>%
      left_join_strict(
        co2_price_share_bysec %>%
          dplyr::select(-'year', -'share_CO2_world') %>%
          dplyr::distinct(),
        by = c("scenario", "region")
      )

    if (!"CO2_ETS" %in% names(co2_price_fragmented)) {
      co2_price_fragmented <- co2_price_fragmented %>%
        dplyr::mutate(CO2_ETS = 0)
    }

    co2_price_fragmented <- co2_price_fragmented %>%
      dplyr::mutate(value = CO2 + CO2_ETS * share_CO2_ETS) %>%
      dplyr::select(Units, scenario, year, region, value, CO2, CO2_ETS, share_CO2_ETS, sector) %>%
      left_join_strict(get(paste('co2_market_frag_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")),
                       by = "sector", multiple = "all") %>%
      dplyr::filter(stats::complete.cases(.)) %>%
      tidyr::complete(tidyr::nesting(scenario, var, year, market, Units), region = regions.global, fill = list(value = 0)) %>%
      dplyr::select(all_of(gcamreport::long_columns))

  } else {
    co2_price_fragmented <- NULL
  }
  co2_price_fragmented <<- co2_price_fragmented
}


#' get_co2_price
#'
#' Retrieve co2 price.
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal co2
#' @return `co2_price_clean` global variable
#' @importFrom magrittr %>%
#' @export
get_co2_price <- function(GCAM_version = 'v8.2') {
  co2_price_clean_pre <- region <- var <- year <- co2_price_clean <-
    co2_price_regional <- co2_price_world <- market <- sector <- value <-
    share_CO2 <- scenario <- weighted_value <- NULL

  check_queries("co2_price_clean", GCAM_version)

  co2_price_clean_pre <-
    dplyr::bind_rows(co2_price_global, co2_price_fragmented) %>%
    filter_data_regions(GCAM_version = GCAM_version)

  if (nrow(co2_price_clean_pre) < 1) {
    co2_price_clean <-
      tibble::tibble(var = unique(get(paste('co2_market_frag_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))$var)) %>%
      tidyr::expand_grid(tibble::tibble(scenario = unique(co2_emiss$scenario))) %>%
      tidyr::expand_grid(tibble::tibble(year = unique(co2_emiss$year))) %>%
      tidyr::expand_grid(tibble::tibble(region = c(unique(co2_emiss$region), "Global"))) %>%
      dplyr::mutate(value = 0) %>%
      dplyr::select(dplyr::all_of(gcamreport::long_columns))
  } else {
    co2_price_regional <- co2_price_clean_pre %>%
      tidyr::complete(tidyr::nesting(region, var, year), scenario = unique(co2_emiss$scenario), fill = list(value = 0))

    # compute Global value using the emission weights
    co2_price_world <- co2_price_regional %>%
      left_join_strict(co2_price_share_bysec %>%
                         left_join_strict(get(paste('co2_market_frag_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")),
                                          by = "sector", mapping = paste('co2_market_frag_map',GCAM_version,sep='_'), multiple = "all") %>%
                         dplyr::filter(var != 'NoReported', !is.na(var)) %>%
                         dplyr::select(-sector,-market,-year),
                       by = c('region','scenario','var')) %>%
      dplyr::filter(var != 'NoReported', !is.na(var)) %>%
      dplyr::mutate(weighted_value = value * share_CO2_world) %>%
      dplyr::group_by(scenario, var, year) %>%
      dplyr::summarise(value = sum(weighted_value)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(region = "Global")

    co2_price_clean <- co2_price_regional %>%
      rbind(co2_price_world) %>%
      dplyr::select(dplyr::all_of(gcamreport::long_columns))
  }

  co2_price_clean <<- co2_price_clean
}


#' get_gov_revenue
#'
#' Retreive overall carbon revenue.
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal revenue
#' @return `gov_revenue_clean` global variable
#' @importFrom magrittr %>%
#' @export
get_gov_revenue <- function(GCAM_version = 'v8.2') {
  scenario <- region <- year <- value <- gov_revenue_clean <- NULL

  check_queries("gov_revenue_clean", GCAM_version)

  gov_revenue_clean <-
    gov_revenue_sector <-
    co2_emiss %>%
    dplyr::mutate(
      sector = ifelse(var == "Emissions|CO2|Energy|Demand|Industry", "Carbon|Demand|Industry", NA),
      sector = ifelse(var == "Emissions|CO2|Energy|Demand|Residential and Commercial", "Carbon|Demand|Buildings", sector),
      sector = ifelse(var == "Emissions|CO2|Energy|Demand|Transportation", "Carbon|Demand|Transport", sector),
      sector = ifelse(var == "Emissions|CO2|Energy|Supply|Electricity", "Carbon|Supply", sector),
      emiss = value
    ) %>%
    dplyr::select(-var, -value) %>%
    dplyr::filter(!is.na(sector)) %>%
    left_join_strict(
      co2_price_clean %>%
        dplyr::mutate(
          sector = ifelse(var == "Price|Carbon|Energy|Demand|Industry", "Carbon|Demand|Industry", NA),
          sector = ifelse(var == "Price|Carbon|Energy|Demand|Residential and Commercial", "Carbon|Demand|Buildings", sector),
          sector = ifelse(var == "Price|Carbon|Energy|Demand|Transportation", "Carbon|Demand|Transport", sector),
          sector = ifelse(var == "Price|Carbon|Energy|Supply", "Carbon|Supply", sector)
        ) %>%
        dplyr::select(-var),
      by = c("scenario", "region", "sector", "year")
    ) %>%
    dplyr::mutate(
      value = value * emiss,
      var = "Revenue|Government"
    ) %>%
    dplyr::mutate(value = value / 1000) %>%
    dplyr::group_by(scenario, region, year, var) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  gov_revenue_clean <<- gov_revenue_clean
}



#' get_en_weights
#'
#' Get energy items weighted by demand. By region and global.
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal prices
#' @return `en_weights` and `en_wld_weights` global variables
#' @importFrom magrittr %>%
#' @export
get_en_weights <- function(GCAM_version = 'v8.2') {
  sector <- input <- var <- value <- unit_conv <- scenario <- region <-
    year <- en_weights <- en_wld_weights <- NULL

  check_queries("en_weights", GCAM_version)
  check_queries("en_wld_weights", GCAM_version)

  en_cons_tmp <- dplyr::bind_rows(
    fe_sector_raw,
    fe_transportation_raw %>%
      dplyr::select(-sector) %>%
      dplyr::rename(sector = input),
    primary_energy_clean %>%
      dplyr::mutate(sector = var),
    secondary_energy_raw %>%
      dplyr::rename(sector = subsector)
  ) %>%
    # sum regardless the input (already accounted in the var item)
    dplyr::group_by(scenario, region, sector, year, var) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    # select variables whose price will be computed
    dplyr::inner_join(get(paste('en_demand_price_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")) %>%
                        dplyr::filter(en_price_var != 'NoReported', en_price_var != ""),
                      by = c('var' = 'en_consumption_var'), relationship = "many-to-many") %>%
    filter_variables() %>%
    dplyr::distinct()


  # weights by sector within each region
  en_weights <-
    en_cons_tmp %>%
    dplyr::group_by(scenario, region, year, en_price_var) %>%
    dplyr::mutate(total_cons_var = sum(value)) %>%
    dplyr::ungroup() %>%
    # compute weight by sector and input
    dplyr::mutate(weight = value / total_cons_var) %>%
    # clean dataset
    dplyr::select(dplyr::all_of(gcamreport::long_columns), sector,
                  en_consumption_var = var, var = en_price_var, weight, -value)

  sectors_combination <- en_weights %>%
    dplyr::select(sector, var, en_consumption_var) %>%
    dplyr::distinct() %>%
    tidyr::expand_grid(
      scenario = unique(en_weights$scenario),
      region = unique(en_weights$region),
      year = unique(en_weights$year)
    )

  en_weights <- dplyr::left_join(
    sectors_combination,
    en_weights,
    by = c('sector', 'var', 'en_consumption_var', 'scenario', 'region', 'year')
  ) %>%
    dplyr::mutate(weight = dplyr::if_else(is.na(weight), 0, weight))


  # global weights by region-sector combination. World = 1
  en_wld_weights <-
    en_cons_tmp %>%
    dplyr::group_by(scenario, year, en_price_var) %>%
    dplyr::mutate(total_cons_var = sum(value)) %>%
    dplyr::ungroup() %>%
    # compute weight by sector and input
    dplyr::mutate(weight = value / total_cons_var) %>%
    # clean dataset
    dplyr::select(dplyr::all_of(gcamreport::long_columns), sector,
                  en_consumption_var = var, var = en_price_var, weight, -value)

  sectors_combination <- en_wld_weights %>%
    dplyr::select(sector, var, en_consumption_var) %>%
    dplyr::distinct() %>%
    tidyr::expand_grid(
      scenario = unique(en_wld_weights$scenario),
      region = unique(en_wld_weights$region),
      year = unique(en_wld_weights$year)
    )

  en_wld_weights <- dplyr::left_join(
    sectors_combination,
    en_wld_weights,
    by = c('sector', 'var', 'en_consumption_var', 'scenario', 'region', 'year')
  ) %>%
    dplyr::mutate(weight = dplyr::if_else(is.na(weight), 0, weight))


  en_weights <<- en_weights
  en_wld_weights <<- en_wld_weights

}

#' get_energy_price_tmp
#'
#' Binds regional oil, gas, coal prices with other energy prices.
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal prices process
#' @return `energy_price` global variable
#' @importFrom magrittr %>%
#' @export
get_energy_price_tmp <- function(GCAM_version = 'v8.2') {
  var <- market <- scenario <- region <- year <- energy_price <-
    value <- PrimaryFuelCO2Coef <- price_C <- unit_conv <- NULL

  check_queries("energy_price", GCAM_version)

  CO2_market_filteredReg <- filter_data_regions(get(paste('co2_market',GCAM_version,sep='_'), envir = asNamespace("gcamreport")), GCAM_version = GCAM_version)
  for (ign in unique(.myGlobals$ignore.global)) {
    CO2_market_filteredReg <- rbind(
      CO2_market_filteredReg,
      CO2_market_filteredReg %>%
        dplyr::mutate(market = region) %>%
        dplyr::distinct() %>%
        dplyr::mutate(market = paste0(market, ign))
    )
  }

  tmp1 <- check_inf(rgcam::getQuery(prj, "CO2 prices"),
                    dataset_name = "CO2 prices") %>%
    dplyr::filter(!grepl("LUC", market))
  if (NA %in% unique(tmp1$market)) {
    warning('ATTENTION: At least one scenario does not contain CO2 price')
  }
  missing_markets <- setdiff(unique(tmp1$market), c(unique(CO2_market_filteredReg$market),NA))
  if (length(missing_markets) != 0 & !.myGlobals$interactive.global) {
    warning(sprintf('ATTENTION: CO2 markets including:\n %.800s \nare not present in the `co2_market_new` mapping file.',
                    paste(missing_markets, collapse = ", ")))
  } else if (length(missing_markets) != 0 & .myGlobals$interactive.global & interactive()) {

    warning(sprintf('ATTENTION: CO2 markets including:\n %.800s \nare not present in the `co2_market_new` mapping file.',
                    paste(missing_markets, collapse = ", ")))
    # user response
    user_input <- readline(prompt =
                             sprintf('ATTENTION: CO2 markets including:\n %.100s \nare not present in the `co2_market_new` mapping file.\nDo you want to continue without adding them (Y/N)? Press Y or N: ',
                                     paste(missing_markets, collapse = ", ")))

    # handling user response
    if (toupper(user_input) %in% c("n","N")) {
      stop("Manual check requested. Stopping execution.")
    } else if (!toupper(user_input) %in% c("y","Y")) {
      stop("Invalid input. Stopping execution.")
    }
  }


  energy_price_map <- get(paste('energy_price_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))

  prices_subsector_pre1 <-
    check_inf(rgcam::getQuery(prj, "prices of all markets"),
              dataset_name = "prices of all markets") %>%
    dplyr::mutate(region = stringr::str_extract(market, paste(as.character(get(paste('reg_cont',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['region']]), collapse = '|')),
                  market = stringr::str_replace(market, paste(as.character(get(paste('reg_cont',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['region']]), collapse = '|'), "")) %>%
    dplyr::select(-Units) %>%
    dplyr::mutate(region = dplyr::if_else(region == 'NA', NA, region))

  prices_subsector_pre2 <- prices_subsector_pre1 %>%
    dplyr::filter(is.na(region)) %>%
    tidyr::complete(tidyr::nesting(scenario, year, market, value), region = unique(na.omit(prices_subsector_pre1$region))) %>%
    dplyr::filter(!is.na(region))

  prices_subsector_pre <- rbind(
    prices_subsector_pre1 %>%
      dplyr::filter(!is.na(region)),
    prices_subsector_pre2) %>%
    left_join_strict(energy_price_map,by = c("market"),
                     relationship = "many-to-many") %>%
    dplyr::filter(var != 'NoReported') %>%
    filter_variables() %>%
    dplyr::mutate(value = value * unit_conv) %>%
    dplyr::select(-unit_conv)

  energy_price_fragmented_biomass <- prices_subsector_pre %>%
    dplyr::filter(grepl("biomass", market)) %>%
    # read in carbon content in kg C per GJ -> convert to tC per GJ
    left_join_strict(
      get(paste('carbon_content',GCAM_version,sep='_'), envir = asNamespace("gcamreport")) %>%
        dplyr::filter(grepl("biomass", PrimaryFuelCO2Coef.name)) %>%
        dplyr::rename("market" = "PrimaryFuelCO2Coef.name"),
      by = c("region", "market")
    ) %>%
    dplyr::filter(var != 'NoReported', !is.na(var)) %>%
    filter_variables() %>%
    dplyr::mutate(PrimaryFuelCO2Coef = PrimaryFuelCO2Coef / 1000) %>%
    tidyr::replace_na(list(PrimaryFuelCO2Coef = 0)) %>%
    dplyr::left_join(check_inf(rgcam::getQuery(prj, "CO2 prices"),
                               dataset_name = "CO2 prices") %>% # left_join already checked
                       dplyr::filter(!grepl("LUC", market)) %>%
                       dplyr::left_join(CO2_market_filteredReg, by = c("market"), relationship = "many-to-many") %>%
                       dplyr::filter(region != 'NoReported') %>%
                       dplyr::select(scenario, region, year, price_C = value), by = c("scenario", "region", "year"),
                     relationship = "many-to-many") %>%
    tidyr::replace_na(list(price_C = 0)) %>%
    # remove carbon price (subsidy) 1990$/tC from biomass 1975$/GJ
    dplyr::mutate(
      price_C = PrimaryFuelCO2Coef * price_C *
        get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_90USD_10USD']],
      value = value *
        get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_75USD_10USD']] + price_C
    ) %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns), market)

  energy_price <-
    rbind(prices_subsector_pre %>%
            dplyr::filter(!grepl("biomass", market)),
          energy_price_fragmented_biomass) %>%
    filter_variables() %>%
    # consider the nº of items for each variable to do the average
    dplyr::group_by(scenario, year, region, var) %>%
    dplyr::mutate(n_count = dplyr::n()) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(scenario, year, region, var) %>%
    dplyr::reframe(value = sum(value) / n_count) %>%
    dplyr::ungroup() %>%
    dplyr::distinct() %>%
    # rearrange dataset
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  energy_price <<- energy_price
}


#' get_total_revenue
#'
#' Compute total revenue: total production * global price.
#' @keywords internal revenue
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @return `total_revenue` global variable
#' @importFrom magrittr %>%
#' @export
get_total_revenue <- function(GCAM_version = 'v8.2') {
  resource <- scenario <- year <- value <- sector <- resource_price <-
    total_production <- NULL

  check_queries("total_revenue", GCAM_version)

  total_revenue <-
    check_inf(rgcam::getQuery(prj, "resource production"),
              dataset_name = "resource production") %>%
    dplyr::filter(resource %in% c("coal", "crude oil", "natural gas")) %>%
    dplyr::group_by(scenario, resource, year) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(total_production = value) %>%
    dplyr::left_join(
      check_inf(rgcam::getQuery(prj, "prices by sector"),
                dataset_name = "prices by sector") %>%
        dplyr::filter(sector %in% c("regional coal", "regional oil", "regional natural gas")) %>%
        dplyr::mutate(
          resource = ifelse(sector == "regional coal", "coal", NA),
          resource = ifelse(sector == "regional oil", "crude oil", resource),
          resource = ifelse(sector == "regional natural gas", "natural gas", resource)
        ) %>%
        dplyr::rename(resource_price = value) %>%
        dplyr::group_by(scenario, resource, year) %>%
        dplyr::summarise(resource_price = mean(resource_price, na.rm = T)) %>%
        dplyr::ungroup(),
      by = c("scenario", "year", "resource")
    ) %>%
    dplyr::mutate(
      total_production = total_production *
        get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['GJ_to_EJ']],
      total_revenue = total_production * resource_price *
        get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_75USD_10USD']]
    )

  total_revenue <<- total_revenue
}

#' get_regional_emission
#'
#' Compute regional nonCO2 emission: regional production * nonCO2 coef.
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal nonco2
#' @return `regional_emission` global variable
#' @importFrom magrittr %>%
#' @export
get_regional_emission <- function(GCAM_version = 'v8.2') {
  year <- Non.CO2 <- emiss.coef <- CH4 <- N2O <- CH4.coef <- N2O.coef <-
    region <- resource <- value <- regional_production <- NULL

  check_queries("regional_emission", GCAM_version)

  regional_emission <- suppressWarnings(
    get(paste('nonco2_content',GCAM_version,sep='_'), envir = asNamespace("gcamreport")) %>%
      dplyr::filter(year == 2005) %>%
      tidyr::spread(Non.CO2, emiss.coef) %>%
      dplyr::rename(
        CH4.coef = CH4,
        N2O.coef = N2O
      ) %>%
      dplyr::mutate(
        CH4.coef = CH4.coef / 1000000,
        N2O.coef = N2O.coef / 1000000
      ) %>%
      dplyr::select(region, resource, CH4.coef, N2O.coef) %>%
      dplyr::left_join(check_inf(rgcam::getQuery(prj, "resource production"),
                                 dataset_name = "resource production") %>%
                         dplyr::filter(resource %in% c("coal", "crude oil", "natural gas")) %>%
                         dplyr::rename(regional_production = value), by = c("region", "resource")) %>%
      dplyr::mutate(
        regional_CH4emission = regional_production * CH4.coef *
          get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['GJ_to_EJ']],
        regional_N2Oemission = regional_production * N2O.coef *
          get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['GJ_to_EJ']]
      )
  )

  regional_emission <<- regional_emission
}


#' get_energy_price
#'
#' Compute final energy price
#' @keywords internal price process
#' @return `energy_price_clean` global variable
#' @importFrom magrittr %>%
#' @export
get_energy_price <- function(GCAM_version = 'v8.2') {
  var <- scenario <- region <- value <- year <- energy_price_clean <-
    energy_price_w <- weights_sec_reg <- sector <- NULL

  check_queries("energy_price_clean", GCAM_version)

  # weighted sum of energy prices by energy consumption
  en_demand_price_map <- get(paste('en_demand_price_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))
  weights_sec_reg <- rbind(
    compute_reg_sec_weight(fe_sector_clean),
    compute_reg_sec_weight(primary_energy_clean),
    compute_reg_sec_weight(secondary_energy_clean)
  )

  energy_price_w <-
    dplyr::left_join(weights_sec_reg %>%
                       dplyr::rename(en_consumption_var = var),
                     energy_price %>%
                       dplyr::rename('en_price_var' = 'var') %>%
                       # add weights
                       left_join_strict(en_demand_price_map,
                                        mapping = paste('en_demand_price_map',GCAM_version,sep='_'),
                                        by = 'en_price_var') %>%
                       filter_variables(),
                     by = c('scenario', 'region', 'year', 'en_consumption_var'),
                     relationship = "many-to-many") %>%
    filter_variables() %>%
    dplyr::mutate(value = value * reg_sec_weight) %>%
    # compute Global values
    dplyr::group_by(scenario, var = en_price_var, year) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(region = "World")


  energy_price_clean <-
    rbind(energy_price %>%
            dplyr::filter(var %in% unique(weights_sec_reg$var)),
          energy_price_w)

  energy_price_clean <<- energy_price_clean
}


#' get_resource_extraction
#'
#' Compute resource extraction
#' @keywords internal resource
#' @return `resource_extraction_clean` global variable
#' @importFrom magrittr %>%
#' @export
get_resource_extraction <- function(GCAM_version = 'v8.2') {
  resource_extraction_clean <- NULL

  resource_extraction_clean <- check_inf(rgcam::getQuery(prj, "resource production"),
                                         dataset_name = "resource production") %>%
    left_join_strict(get(paste('res_extraction_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")),
                     by = 'resource') %>%
    dplyr::filter(var != "NoReported") %>%
    dplyr::mutate(value = value * unit_conv) %>%
    dplyr::group_by(scenario, region, year, var) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  resource_extraction_clean <<- resource_extraction_clean

}

#' get_production_price
#'
#' Compute industry production prices
#' @keywords internal price process
#' @return `production_price_clean` global variable
#' @importFrom magrittr %>%
#' @export
get_production_price <- function(GCAM_version = 'v8.2') {
  production_price_clean <- NULL

  check_queries("production_price_clean", GCAM_version)

  production_price_clean <- rbind(
    check_inf(rgcam::getQuery(prj, "iron and steel prices"),
              dataset_name = "iron and steel prices"),
    check_inf(rgcam::getQuery(prj, "chemical prices"),
              dataset_name = "chemical prices"),
    check_inf(rgcam::getQuery(prj, "ammonia and N fertilizer prices"),
              dataset_name = "ammonia and N fertilizer prices"),
    check_inf(rgcam::getQuery(prj, "aluminum prices"),
              dataset_name = "aluminum prices")
  ) %>%
    left_join_strict(get(paste('production_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")),
                     by = 'sector') %>%
    dplyr::filter(var != 'NoReported', !is.na(var)) %>%
    filter_variables() %>%
    # 1e3 $/kg = 1 $/Mt
    dplyr::mutate(value = dplyr::if_else(grepl('kg',Units),
                                         value * get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['kg_to_Mt']],
                                         value)) %>%
    # 18.6 $/GJ ammonia = 1 $/Mt ammonia -> we use this chemical as a high value chemical proxy, but it should be refined
    dplyr::mutate(value = dplyr::if_else(Units == '1975$/GJ', value * 18.6, value)) %>%
    # 1975$ to 2010$
    dplyr::mutate(value = value *
                    get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_75USD_10USD']],
                  var = paste0('Price|',var)) %>%
    dplyr::group_by(scenario,region,year,var) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  production_price_clean <<- production_price_clean
}


# Capacity and investment
# ==============================================================================================
# electricity capacity
## first check global existing capacity from IEA, calculate cf for existing capacity

#' get_cf_iea_tmp
#'
#' Calculate cf for existing capacity checking global existing capacity from IEA.
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal capacity process tmp
#' @return `cf_iea` global variable
#' @importFrom magrittr %>%
#' @export
get_cf_iea_tmp <- function(GCAM_version = 'v8.2') {
  year <- scenario <- var <- value <- period <- variable <- EJ <- cf <-
    cf_iea <- technology <- NULL

  check_queries("cf_iea", GCAM_version)

  cf_rgn_filteredReg <- filter_data_regions(get(paste('cf_rgn',GCAM_version,sep='_'), envir = asNamespace("gcamreport")), GCAM_version = GCAM_version) %>%
    interpolateGCAMdata(valuecol = 'capacity.factor')
  ya = years_in_prj[years_in_prj%%5 != 0]
  if (length(ya) == 0) ya = 2020
  iea_capacity <- get(paste('iea_capacity',GCAM_version,sep='_'), envir = asNamespace("gcamreport")) %>%
    interpolateGCAMdata(yearcol = 'period', year_to_appear = ya)

  # check if the mapping files have a mismatch
  tmp1 <- secondary_energy_clean %>%
    dplyr::filter(year == base_year_p, scenario == unique(secondary_energy_clean$scenario)[1]) %>%
    dplyr::group_by(var) %>%
    dplyr::summarise(EJ = sum(value, na.rm = T)) %>%
    dplyr::ungroup()

  tmp2 <- iea_capacity %>%
    dplyr::filter(period == base_year_p, scenario == "Current Policies Scenario") %>%
    dplyr::mutate(
      variable = gsub("Capacity\\|Electricity\\|CSP", "Capacity\\|Electricity\\|Solar\\|CSP", variable),
      variable = gsub("Capacity\\|Electricity\\|Biomass", "Capacity\\|Electricity\\|Biomass\\|w/o CCS", variable),
      variable = gsub("Capacity\\|Electricity\\|Coal", "Capacity\\|Electricity\\|Coal\\|w/o CCS", variable),
      variable = gsub("Capacity\\|Electricity\\|Gas", "Capacity\\|Electricity\\|Gas\\|w/o CCS", variable),
      variable = gsub("Capacity\\|Electricity\\|Oil", "Capacity\\|Electricity\\|Oil\\|w/o CCS", variable),
      variable = gsub("Capacity", "Secondary Energy", variable)
    )

  if (sum(unique(tmp1$var) %in% unique(tmp2$variable)) != 12) {
    handle_warning(mapping_name1 = 'iea_capacity', query_name = 'elec gen by gen tech')
  }

  cf_iea1 <-
    tmp1 %>%
    dplyr::left_join( # left_join already checked
      tmp2,
      by = c("var" = "variable")
    ) %>%
    dplyr::mutate(
      cf = EJ / (value *
                   get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['hr_per_yr']] *
                   get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['EJ_to_GWh']]),
      cf = replace(cf, cf > 1, 0.99)
    ) %>%
    dplyr::filter(!is.na(cf), !var %in% c("Secondary Energy|Electricity", "Secondary Energy|Electricity|Non-Biomass Renewables")) %>%
    left_join_strict(get(paste('capacity_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")),
                     by = "var", mapping = paste('capacity_map',GCAM_version,sep='_'), multiple = "all") %>%
    dplyr::filter(var != 'NoReported', !is.na(var))

  if (!any(grepl('Trade|Investment|Capacity|All',desired_variables.global))) {
    cf_iea2 <- cf_iea1 %>%
      filter_variables()
  } else {
    cf_iea2 <- cf_iea1
  }
  cf_iea <- cf_iea2 %>%
    dplyr::select(technology, cf) %>%
    dplyr::mutate(region = "USA", vintage = base_year_p) %>%
    tidyr::complete(tidyr::nesting(technology, cf),
                    vintage = years_in_prj[years_in_prj >= 1990],
                    region = unique(cf_rgn_filteredReg$region)
    )

  cf_iea <<- cf_iea
}

#' get_elec_cf_tmp
#'
#' Computes future capacity estimates using GCAM.
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal capacity process tmp
#' @return `elec_cf` global variable
#' @importFrom magrittr %>%
#' @export
get_elec_cf_tmp <- function(GCAM_version = 'v8.2') {
  technology <- X2100 <- cf <- region <- stub.technology <- year <-
    capacity.factor <- cf.rgn <- vintage <- elec_cf <- NULL

  check_queries("elec_cf", GCAM_version)

  cf_rgn_filteredReg <- filter_data_regions(get(paste('cf_rgn',GCAM_version,sep='_'), envir = asNamespace("gcamreport")), GCAM_version = GCAM_version) %>%
    interpolateGCAMdata(valuecol = 'capacity.factor')
  cf_iea_filteredReg <- filter_data_regions(cf_iea, GCAM_version = GCAM_version) %>%
    interpolateGCAMdata(valuecol = 'cf', yearcol = 'vintage')

  tmp1 <- get(paste('cf_gcam',GCAM_version,sep='_'), envir = asNamespace("gcamreport")) %>%
    dplyr::select(technology, cf = `2100`) %>%
    dplyr::mutate(region = "USA", vintage = 2025) %>%
    tidyr::complete(tidyr::nesting(technology, cf),
                    vintage = seq(2025, 2100, by = 5),
                    region = unique(cf_rgn_filteredReg$region)
    )

  tmp2 <- cf_rgn_filteredReg %>%
    dplyr::select(region, technology = stub.technology, vintage = year, cf.rgn = capacity.factor)

  # check if the mapping files have a mismatch
  if (sum(unique(tmp2$technology) %in% unique(tmp1$technology)) != 7) {
    handle_warning(mapping_name1 = 'A23.globaltech_capacity_factor', mapping_name2 = 'L223.StubTechCapFactor_elec')
  }

  elec_cf <-
    tmp1 %>%
    # first, replace regional cf for wind and solar
    dplyr::left_join( # left_join already checked
      tmp2,
      by = c("technology", "vintage", "region")
    ) %>%
    dplyr::mutate(cf = replace(cf, !is.na(cf.rgn), cf.rgn[!is.na(cf.rgn)])) %>%
    # second, use iea capacity consistent cf for existing vintage
    dplyr::bind_rows(cf_iea_filteredReg) %>%
    tidyr::complete(tidyr::nesting(technology, region), vintage = sort(years_in_prj[years_in_prj >= 1990])) %>%
    dplyr::group_by(technology, region) %>%
    dplyr::mutate(cf = approx_fun(vintage, cf, rule = 2)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!technology %in% c("existing coal", "add CCS retrofit"))

  elec_cf <- filter_data_regions(elec_cf, GCAM_version = GCAM_version)

  elec_cf <<- elec_cf
}

#' get_elec_capacity_tot
#'
#' Calculate total electricity capacity.
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal capacity process
#' @return `elec_capacity_tot_clean` global variable
#' @importFrom magrittr %>%
#' @export
get_elec_capacity_tot <- function(GCAM_version = 'v8.2') {
  output <- technology <- vintage <- var <- unit_conv <-
    scenario <- region <- year <- value <- gw <- elec_capacity_tot_clean <- NULL

  check_queries("elec_capacity_tot_clean", GCAM_version)
  grid_regions <- get('grid_regions', envir = asNamespace("gcamreport"))

  elec_capacity_tot_clean1 <- suppressWarnings(
    check_inf(rgcam::getQuery(prj, "elec gen by gen tech and cooling tech and vintage"),
              dataset_name = "elec gen by gen tech and cooling tech and vintage") %>%
      dplyr::filter(!output %in% c("electricity", "elect_td_bld")) %>%
      tidyr::separate(technology, into = c("technology", "vintage"), sep = ",") %>%
      dplyr::mutate(
        vintage = as.integer(sub("year=", "", vintage)),
        output = gsub("elec_", "", output)
      ) %>%
      dplyr::group_by(scenario, region, technology = output, vintage, year) %>%
      dplyr::summarise(value = sum(value, na.rm = T)) %>%
      dplyr::ungroup() %>%
      dplyr::bind_rows(check_inf(rgcam::getQuery(prj, "elec gen by gen tech and cooling tech and vintage"),
                                 dataset_name = "elec gen by gen tech and cooling tech and vintage") %>%
                         dplyr::filter(output %in% c("electricity", "elect_td_bld")) %>%
                         tidyr::separate(technology, into = c("technology", "vintage"), sep = ",") %>%
                         dplyr::mutate(vintage = as.integer(sub("year=", "", vintage))) %>%
                         dplyr::group_by(scenario, region, technology, vintage, year) %>%
                         dplyr::summarise(value = sum(value, na.rm = T)) %>%
                         dplyr::ungroup()) %>%
      # simplify technology names
      dplyr::mutate(technology = stringr::str_remove(technology,',depth=1')) %>%
      dplyr::mutate(technology = stringr::str_remove(technology,'_base')) %>%
      dplyr::mutate(technology = stringr::str_remove(technology,'_int')) %>%
      dplyr::mutate(technology = stringr::str_remove(technology,'_peak')) %>%
      dplyr::mutate(technology = stringr::str_remove(technology,'_subpeak')) %>%
      dplyr::filter(technology != 'battery') %>%
      dplyr::group_by(scenario,region,technology,vintage,year) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::ungroup() %>%
      # remove grid regions (GCAM-Europe issue)
      dplyr::filter(!region %in% grid_regions) %>%
      left_join_strict(elec_cf %>%
                         dplyr::select(-cf.rgn), by = c("region", "technology", "vintage")) %>%
      dplyr::mutate(EJ = value) %>%
      conv_EJ_GW(GCAM_version = GCAM_version) %>%
      dplyr::group_by(scenario, region, technology, year) %>%
      dplyr::summarise(value = sum(gw, na.rm = T)) %>%
      dplyr::ungroup() %>%
      left_join_strict(get(paste('capacity_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")) %>%
                         dplyr::select(-output),
                       by = c("technology"), mapping = paste('capacity_map',GCAM_version,sep='_'), multiple = "all") %>%
      dplyr::filter(var != 'NoReported', !is.na(var)))

  if (!any(grepl('Trade|Investment|Capacity|All',desired_variables.global))) {
    elec_capacity_tot_clean2 <- elec_capacity_tot_clean1 %>%
      filter_variables()
  } else {
    elec_capacity_tot_clean2 <- elec_capacity_tot_clean1
  }

  elec_capacity_tot_clean <- elec_capacity_tot_clean2 %>%
    dplyr::mutate(
      value = value * unit_conv,
      var = sub("Secondary Energy", "Capacity", var)
    ) %>%
    dplyr::group_by(scenario, region, var, year) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    tidyr::complete(tidyr::nesting(scenario, region, year),
                    var = unique(var),
                    fill = list(value = 0)
    ) %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))


  elec_capacity_tot_clean <<- elec_capacity_tot_clean
}

#' get_elec_capacity_add_tmp
#'
#' Calculate added total electricity capacity.
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal capacity process tmp
#' @return `elec_capacity_add` global variable
#' @importFrom magrittr %>%
#' @export
get_elec_capacity_add_tmp <- function(GCAM_version = 'v8.2') {
  output <- technology <- vintage <- scenario <- elec_capacity_add <-
    region <- year <- value <- gw <- EJ <- NULL

  check_queries("elec_capacity_add", GCAM_version)
  grid_regions <- get('grid_regions', envir = asNamespace("gcamreport"))

  elec_capacity_add <- suppressWarnings(
    check_inf(rgcam::getQuery(prj, "elec gen by gen tech and cooling tech and vintage"),
              dataset_name = "elec gen by gen tech and cooling tech and vintage") %>%
      dplyr::filter(!output %in% c("electricity", "elect_td_bld")) %>%
      tidyr::separate(technology, into = c("technology", "vintage"), sep = ",") %>%
      dplyr::mutate(
        vintage = as.integer(sub("year=", "", vintage)),
        output = gsub("elec_", "", output)
      ) %>%
      dplyr::group_by(scenario, region, technology = output, vintage, year) %>%
      dplyr::summarise(value = sum(value, na.rm = T)) %>%
      dplyr::ungroup() %>%
      dplyr::bind_rows(check_inf(rgcam::getQuery(prj, "elec gen by gen tech and cooling tech and vintage"),
                                 dataset_name = "elec gen by gen tech and cooling tech and vintage") %>%
                         dplyr::filter(output %in% c("electricity", "elect_td_bld")) %>%
                         tidyr::separate(technology, into = c("technology", "vintage"), sep = ",") %>%
                         dplyr::mutate(vintage = as.integer(sub("year=", "", vintage))) %>%
                         dplyr::group_by(scenario, region, technology, vintage, year) %>%
                         dplyr::summarise(value = sum(value, na.rm = T)) %>%
                         dplyr::ungroup()) %>%
      dplyr::filter(year == vintage, year > 2015) %>%
      dplyr::group_by(scenario, region, technology, year) %>%
      dplyr::summarise(value = sum(value, na.rm = T)) %>%
      dplyr::ungroup() %>%
      # remove grid regions (GCAM-Europe issue)
      dplyr::filter(!region %in% grid_regions) %>%
      # use GCAM cf for capacity additions
      dplyr::mutate(technology = stringr::str_remove(technology,',depth=1')) %>%
      dplyr::mutate(technology = stringr::str_remove(technology,'_base')) %>%
      dplyr::mutate(technology = stringr::str_remove(technology,'_int')) %>%
      dplyr::mutate(technology = stringr::str_remove(technology,'_peak')) %>%
      dplyr::mutate(technology = stringr::str_remove(technology,'_subpeak')) %>%
      dplyr::filter(technology != 'battery') %>%
      dplyr::group_by(scenario,region,technology,year) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::ungroup() %>%
      left_join_strict(elec_cf %>%
                         dplyr::select(-'cf.rgn') %>%
                         dplyr::rename(year = vintage),
                       by = c("region", "technology", "year")) %>%
      dplyr::distinct() %>%
      # use average annual additions
      dplyr::mutate(EJ = value / 5) %>%
      conv_EJ_GW(GCAM_version = GCAM_version) %>%
      dplyr::group_by(scenario, region, technology, year) %>% #
      dplyr::summarise(GW = sum(gw, na.rm = T), EJ = sum(EJ, na.rm = T)) %>%
      dplyr::ungroup()
  )

  elec_capacity_add <<- elec_capacity_add
}


#' get_refliq_capacity_add_tmp
#'
#' Calculate added total refined liquids electricity capacity.
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal capacity process tmp
#' @return `refliq_capacity_add` global variable
#' @importFrom magrittr %>%
#' @export
get_refliq_capacity_add_tmp <- function(GCAM_version = 'v8.2') {
  # TOBEADDED
  # refliq_capacity_add;get_refliq_capacity_add_tmp;refliq_cf;;refined liquids production by cooling tech and vintage

  output <- technology <- vintage <- scenario <- refliq_capacity_add <-
    region <- year <- value <- gw <- EJ <- NULL

  check_queries("refliq_capacity_add", GCAM_version)

  refliq_capacity_add <- suppressWarnings(
    check_inf(rgcam::getQuery(prj, "refined liquids production by cooling tech and vintage"),
              dataset_name = "refined liquids production by cooling tech and vintage") %>%
      tidyr::separate(technology, into = c("technology", "vintage"), sep = ",") %>%
      dplyr::mutate(
        vintage = as.integer(sub("year=", "", vintage)),
        output = gsub("elec_", "", output)
      ) %>%
      dplyr::group_by(scenario, region, technology = output, vintage, year) %>%
      dplyr::summarise(value = sum(value, na.rm = T)) %>%
      dplyr::ungroup() %>%
      dplyr::bind_rows(check_inf(rgcam::getQuery(prj, "refined liquids production by cooling tech and vintage"),
                                 dataset_name = "refined liquids production by cooling tech and vintage") %>%
                         tidyr::separate(technology, into = c("technology", "vintage"), sep = ",") %>%
                         dplyr::mutate(vintage = as.integer(sub("year=", "", vintage))) %>%
                         dplyr::group_by(scenario, region, technology, vintage, year) %>%
                         dplyr::summarise(value = sum(value, na.rm = T)) %>%
                         dplyr::ungroup()) %>%
      dplyr::filter(year == vintage, year > 2015) %>%
      dplyr::group_by(scenario, region, technology, year) %>%
      dplyr::summarise(value = sum(value, na.rm = T)) %>%
      dplyr::ungroup() %>%
      # use GCAM cf for capacity additions
      left_join_strict(elec_cf %>% #TODO - refliq_cf
                         dplyr::select(-'cf.rgn') %>%
                         dplyr::rename(year = vintage),
                       by = c("region", "technology", "year"),
                       by_message = 'technology') %>%
      dplyr::filter(var != 'NoReported', !is.na(var)) %>%
      filter_variables() %>%
      # use average annual additions
      dplyr::mutate(EJ = value / 5) %>%
      conv_EJ_GW(GCAM_version = GCAM_version) %>%
      dplyr::group_by(scenario, region, technology, year) %>% #
      dplyr::summarise(GW = sum(gw, na.rm = T), EJ = sum(EJ, na.rm = T)) %>%
      dplyr::ungroup()
  )

  refliq_capacity_add <<- refliq_capacity_add
}


#' get_hydrogen_capacity_add_tmp
#'
#' Calculate added total hydrgoen capacity.
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal capacity process tmp
#' @return `hydrogen_capacity_add` global variable
#' @importFrom magrittr %>%
#' @export
get_hydrogen_capacity_add_tmp <- function(GCAM_version = 'v8.2') {
  # TOBEADDED
  # hydrogen_capacity_add;get_hydrogen_capacity_add_tmp;hydrogen_cf;;hydrogen production by cooling tech and vintage

  output <- technology <- vintage <- scenario <- hydrogen_capacity_add <-
    region <- year <- value <- gw <- EJ <- NULL

  check_queries("hydrogen_capacity_add", GCAM_version)

  hydrogen_capacity_add <- suppressWarnings(
    check_inf(rgcam::getQuery(prj, "hydrogen production by cooling tech and vintage"),
              dataset_name = "hydrogen production by cooling tech and vintage") %>%
      tidyr::separate(technology, into = c("technology", "vintage"), sep = ",") %>%
      dplyr::mutate(
        vintage = as.integer(sub("year=", "", vintage)),
        output = gsub("H2_", "", output)
      ) %>%
      dplyr::group_by(scenario, region, technology = output, vintage, year) %>%
      dplyr::summarise(value = sum(value, na.rm = T)) %>%
      dplyr::ungroup() %>%
      dplyr::bind_rows(check_inf(rgcam::getQuery(prj, "hydrogen production by cooling tech and vintage"),
                                 dataset_name = "hydrogen production by cooling tech and vintage") %>%
                         tidyr::separate(technology, into = c("technology", "vintage"), sep = ",") %>%
                         dplyr::mutate(vintage = as.integer(sub("year=", "", vintage))) %>%
                         dplyr::group_by(scenario, region, technology, vintage, year) %>%
                         dplyr::summarise(value = sum(value, na.rm = T)) %>%
                         dplyr::ungroup()) %>%
      dplyr::filter(year == vintage, year > 2015) %>%
      dplyr::group_by(scenario, region, technology, year) %>%
      dplyr::summarise(value = sum(value, na.rm = T)) %>%
      dplyr::ungroup() %>%
      # use GCAM cf for capacity additions
      left_join_strict(elec_cf %>% # TODO - hydrogen_cf
                         dplyr::select(-'cf.rgn') %>%
                         dplyr::rename(year = vintage),
                       by = c("region", "technology", "year"),
                       by_message = 'technology') %>%
      dplyr::filter(var != 'NoReported', !is.na(var)) %>%
      filter_variables() %>%
      # use average annual additions
      dplyr::mutate(EJ = value / 5) %>%
      conv_EJ_GW(GCAM_version = GCAM_version) %>%
      dplyr::group_by(scenario, region, technology, year) %>% #
      dplyr::summarise(GW = sum(gw, na.rm = T), EJ = sum(EJ, na.rm = T)) %>%
      dplyr::ungroup()
  )

  hydrogen_capacity_add <<- hydrogen_capacity_add
}

#' get_elec_capacity_add
#'
#' Calculate final total added capacity.
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal capacity process
#' @return `elec_capacity_add_clean` global variable
#' @importFrom magrittr %>%
#' @export
get_elec_capacity_add <- function(GCAM_version = 'v8.2') {
  output <- EJ <- value <- var <- GW <- unit_conv <- scenario <- vintage <-
    region <- year <- gw <- output <- technology <- scenario <-
    elec_capacity_add_clean <- NULL

  check_queries("elec_capacity_add_clean", GCAM_version)

  # check calculations for this
  elec_capacity_add_clean <-
    elec_capacity_add %>%
    left_join_strict(get(paste('capacity_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")) %>% dplyr::select(-output),
                     by = c("technology"), mapping = paste('capacity_map',GCAM_version,sep='_'), multiple = "all") %>%
    dplyr::filter(var != 'NoReported', !is.na(var)) %>%
    dplyr::mutate(
      value = GW * unit_conv,
      var = sub("Secondary Energy", "Capacity Additions", var)
    ) %>%
    dplyr::mutate(value = dplyr::if_else(var == "Capacity Additions|Electricity|Storage Capacity",
                                         GW * 8760, # multiply by # of hours in a year
                                         value)) %>%
    dplyr::group_by(scenario, region, var, year) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    tidyr::complete(tidyr::nesting(scenario, region, year),
                    var = unique(var),
                    fill = list(value = 0)
    ) %>%
    dplyr::group_by(scenario, region, var, year) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  elec_capacity_add_clean <<- elec_capacity_add_clean
}




#' get_elec_capital
#'
#' Calculate capital cost of a newly installed plants.
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal capital process
#' @return `elec_capital_clean` global variable
#' @importFrom magrittr %>%
#' @export
get_elec_capital <- function(GCAM_version = 'v8.2') {
  year <- region <- var <- capital.overnight <- technology <-
    GW <- scenario <- output <- value <- unit_conv <- elec_capital_clean <- NULL

  check_queries("elec_capital_clean", GCAM_version)
  cf_rgn_filteredReg <- filter_data_regions(get(paste('cf_rgn',GCAM_version,sep='_'), envir = asNamespace("gcamreport")), GCAM_version = GCAM_version)


  # Capital costs from GCAM in $1975/kw -> convert to $2010/kw
  elec_capital_tmp <-
    get(paste('capital_gcam',GCAM_version,sep='_'), envir = asNamespace("gcamreport")) %>%
    dplyr::mutate(region = unique(cf_rgn_filteredReg$region)[1],
                  scenario = scenarios.global[1]) %>%
    dplyr::select(-sector) %>%
    tidyr::complete(tidyr::nesting(subsector, technology, year, capital.overnight),
                    region = unique(cf_rgn_filteredReg$region),
                    scenario = scenarios.global) %>%
    # gw * 10e6 * $/kw / 10e9 = bill$
    dplyr::mutate(value = capital.overnight * get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_75USD_10USD']]) %>%
    left_join_strict(get(paste('capacity_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")) %>%
                       dplyr::select(-output),
                     by = c("subsector", "technology"),
                     mapping = paste('capacity_map',GCAM_version,sep='_'),
                     multiple = "all") %>%
    dplyr::filter(!is.na(var), var != 'NoReported') %>%
    filter_variables() %>%
    dplyr::mutate(value = value * unit_conv,
                  var = sub("Secondary Energy", "Capital Cost", var)) %>%
    dplyr::group_by(scenario, region, var, year) %>%
    dplyr::summarise(value = mean(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  # average mean for global tech capital cost
  elec_capital_clean <- rbind(
    elec_capital_tmp,
    elec_capital_tmp %>%
      dplyr::group_by(scenario, var, year) %>%
      dplyr::summarise(value = mean(value, na.rm = T)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(region = "World")
  )

  elec_capital_clean <<- elec_capital_clean
}




#' get_elec_investment
#'
#' Calculate electricity investment = annual capacity additions * capital costs.
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal capital process
#' @return `elec_investment_clean` global variable
#' @importFrom magrittr %>%
#' @export
get_elec_investment <- function(GCAM_version = 'v8.2') {
  year <- region <- var <- capital.overnight <- technology <-
    GW <- scenario <- output <- value <- unit_conv <- elec_investment_clean <- NULL

  check_queries("elec_investment_clean", GCAM_version)

  secondary_energy_map <- get(paste('secondary_energy_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")) %>% dplyr::select(-output)
  ya = years_in_prj[years_in_prj%%5 != 0]
  if (length(ya) == 0) ya = 2020
  capital_gcam <- get(paste('capital_gcam',GCAM_version,sep='_'), envir = asNamespace("gcamreport")) %>%
    interpolateGCAMdata(valuecol = 'capital.overnight', year_to_appear = ya)

  elec_investment_clean1 <-
    # Electricity investment = annual capacity additions * capital costs
    elec_capacity_add %>%
    dplyr::filter(technology != 'hydro') %>%
    dplyr::filter(technology != 'desalinated water') %>%
    left_join_strict(
      capital_gcam,
      # dplyr::mutate(
      #   capital.overnight = replace(capital.overnight, technology == "wind_storage", capital.overnight[technology == "wind"] * .484),
      #   capital.overnight = replace(capital.overnight, technology == "CSP_storage", 760 *
      #                                 get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_19USD_75USD']]),
      #   capital.overnight = replace(capital.overnight, technology == "PV_storage", capital.overnight[technology == "PV"] * .518))
      by = c("technology", "year"), mapping = paste('capital_gcam',GCAM_version,sep='_')
    ) %>%
    # gw * 10e6 * $/kw / 10e9 = bill$
    dplyr::mutate(value = GW * capital.overnight / 1000 *
                    get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_75USD_10USD']]) %>%
    left_join_strict(secondary_energy_map, by = c("technology", "subsector"), multiple = "all") %>%
    dplyr::filter(var != 'NoReported', !is.na(var))

  if (!any(grepl('Trade|Investment|Capacity|All',desired_variables.global))) {
    elec_investment_clean2 <- elec_investment_clean1 %>%
      filter_variables()
  } else {
    elec_investment_clean2 <- elec_investment_clean1
  }

  elec_investment_clean <- elec_investment_clean2 %>%
    dplyr::mutate(
      value = value * unit_conv,
      var = sub("Secondary Energy", "Investment|Energy Supply", var)
    ) %>%
    dplyr::group_by(scenario, region, var, year) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  elec_investment_clean <<- elec_investment_clean
}

#' get_transmission_invest
#'
#' Calculate Investment in Electricity Transmission and Distribution. Scales 2020 numbers based on the average of other model results from Mcollion et al. 2018.
#' Converts 2015 values to 2010 dollars.
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal investment process
#' @return `transmission_invest_clean` global variable
#' @importFrom magrittr %>%
#' @export
get_transmission_invest <- function(GCAM_version = 'v8.2') {
  Region <- Variable <- year <- value <- var <- scenario <- share <-
    region <- invest <- rate <- NULL

  check_queries("transmission_invest_clean", GCAM_version)

  transmission2020 <-
    get(paste('investment',GCAM_version,sep='_'), envir = asNamespace("gcamreport")) %>%
    dplyr::filter(Region == "World", Variable == "Energy Supply|Electricity|Transmission and Distribution", year == 2020) %>%
    dplyr::mutate(value = value * get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_15USD_10USD']]) %>%
    dplyr::summarise(value = mean(value, na.rm = T)) %>%
    unlist()

  transmission_invest2020 <-
    elec_capacity_add_clean %>%
    dplyr::filter(var == "Capacity Additions|Electricity", year == 2020) %>%
    dplyr::group_by(scenario) %>%
    dplyr::mutate(
      share = value / sum(value, na.rm = T),
      invest = share * transmission2020
    )

  transmission_invest_clean <-
    elec_capacity_add_clean %>%
    dplyr::filter(var == "Capacity Additions|Electricity") %>%
    interpolateGCAMdata(year_to_appear = 2020) %>%
    dplyr::group_by(scenario, region) %>%
    dplyr::mutate(rate = value / value[year == 2020]) %>%
    dplyr::ungroup() %>%
    left_join_strict(
      transmission_invest2020 %>%
        dplyr::select(scenario, region, invest),
      by = c("scenario", "region")
    ) %>%
    dplyr::filter(var != 'NoReported', !is.na(var)) %>%
    dplyr::mutate(value = rate * invest, var = "Investment|Energy Supply|Electricity|Transmission and Distribution") %>%
    filter_variables() %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  transmission_invest_clean <<- transmission_invest_clean
}


#' get_resource_investment
#'
#' Calculate investment of resource production.
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal investment process
#' @return `resource_investment_clean` global variable
#' @importFrom magrittr %>%
#' @export
get_resource_investment <- function(GCAM_version = 'v8.2') {
  resource <- technology <- vintage <- year <- scenario <- region <- rate <-
    value <- Region <- Variable <- fuel <- production <- share <- invest <- NULL

  check_queries("resource_investment_clean", GCAM_version)

  # Investment of resource production
  resource_addition <- suppressWarnings(
    check_inf(rgcam::getQuery(prj, "resource production by tech and vintage"),
              dataset_name = "resource production by tech and vintage") %>%
      dplyr::filter(resource %in% c("coal", "natural gas", "crude oil", "unconventional oil", "uranium")) %>%
      tidyr::separate(technology, into = c("technology", "vintage"), sep = ",") %>%
      dplyr::mutate(vintage = as.integer(sub("year=", "", vintage))) %>%
      dplyr::filter(year > 2010) %>%
      dplyr::mutate(
        resource = sub("crude oil", "oil", resource),
        resource = sub("unconventional oil", "oil", resource)
      ) %>%
      dplyr::group_by(scenario, fuel = resource, region, year) %>%
      dplyr::summarise(production = sum(value, na.rm = T)) %>%
      dplyr::ungroup() %>%
      # expand the uranium regions, since only USA was present (it is a global market)
      tidyr::complete(tidyr::nesting(scenario, fuel, year), region = available_regions(print = F, GCAM_version)[available_regions(print = F, GCAM_version) != 'World'],
                      fill = list(production = 0))
  )
  # fix Uranium regions
  if ('uranium' %in% unique(resource_addition$fuel)) {
    resource_addition <- resource_addition %>%
      dplyr::group_by(scenario, fuel, year) %>%
      dplyr::mutate(production = dplyr::if_else(fuel == 'uranium',
                                                production[region == 'USA'],
                                                production)) %>%
      dplyr::ungroup() %>%
      filter_data_regions(GCAM_version = GCAM_version)
  } else {
    resource_addition <- resource_addition %>%
      filter_data_regions(GCAM_version = GCAM_version)
  }

  # scale 2015 number - average of other model results from Mcollion et al. 2018
  extraction2015 <-
    get(paste('investment',GCAM_version,sep='_'), envir = asNamespace("gcamreport")) %>%
    dplyr::filter(Region == "World", Variable == "Extraction and Conversion - Fossil Fuels", year == 2015) %>%
    dplyr::mutate(value = value * get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_15USD_10USD']]) %>%
    dplyr::summarise(value = mean(value, na.rm = T)) %>%
    unlist()

  extraction2020 <-
    get(paste('investment',GCAM_version,sep='_'), envir = asNamespace("gcamreport")) %>%
    # Use the nearest year to the final GCAM base year from the investment dataset
    dplyr::filter(Region == "World", Variable == "Extraction and Conversion - Fossil Fuels", year == unique(year)[which.min(abs(unique(year) - base_year_p))] ) %>%
    dplyr::mutate(value = value * get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_15USD_10USD']]) %>%
    dplyr::summarise(value = mean(value, na.rm = T)) %>%
    unlist()

  resource_investment2015 <-
    resource_addition %>%
    dplyr::filter(year == 2015) %>%
    left_join_strict(
      check_inf(rgcam::getQuery(prj, "regional primary energy prices"),
                dataset_name = "regional primary energy prices") %>%
        dplyr::mutate(fuel = sub("regional ", "", fuel)) %>%
        # add uranium (global market)
        rbind(check_inf(rgcam::getQuery(prj, "prices of all markets"),
                        dataset_name = "prices of all markets") %>%
                dplyr::filter(grepl('uranium', market)) %>%
                dplyr::mutate(fuel = 'uranium') %>%
                dplyr::select(-market) %>%
                # from 1975$/kg to 1975$/EJ; 1EJ = 0.08314kg
                dplyr::mutate(value = value / 0.08314) %>%
                tidyr::expand_grid(region = available_regions(print = F, GCAM_version)[available_regions(print = F, GCAM_version) != 'World'])) %>%
        filter_data_regions(GCAM_version = GCAM_version),
      by = c("scenario", "region", "year", "fuel")
    ) %>%
    dplyr::mutate(value = production * value) %>%
    dplyr::group_by(scenario, resource = fuel, region) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(scenario) %>%
    dplyr::mutate(
      share = value / sum(value, na.rm = T),
      invest = share * extraction2015
    ) %>%
    dplyr::ungroup()

  resource_investment2020 <-
    resource_addition %>%
    dplyr::filter(year == base_year_p) %>%
    left_join_strict(
      check_inf(rgcam::getQuery(prj, "regional primary energy prices"),
                dataset_name = "regional primary energy prices") %>%
        dplyr::mutate(fuel = sub("regional ", "", fuel)) %>%
        # add uranium (global market)
        rbind(check_inf(rgcam::getQuery(prj, "prices of all markets"),
                        dataset_name = "prices of all markets") %>%
                dplyr::filter(grepl('uranium', market)) %>%
                dplyr::mutate(fuel = 'uranium') %>%
                dplyr::select(-market) %>%
                # from 1975$/kg to 1975$/GJ; 1GJ = 83.14kg
                dplyr::mutate(value = value / 83.14) %>%
                tidyr::expand_grid(region = available_regions(print = F, GCAM_version)[available_regions(print = F, GCAM_version) != 'World'])) %>%
        filter_data_regions(GCAM_version = GCAM_version),
      by = c("scenario", "region", "year", "fuel")
    ) %>%
    dplyr::mutate(value = production * value) %>%
    dplyr::group_by(scenario, resource = fuel, region) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(scenario) %>%
    dplyr::mutate(
      share = value / sum(value, na.rm = T),
      invest = share * extraction2020
    ) %>%
    dplyr::ungroup()

  reg <- dplyr::if_else(desired_regions.global == "All" | "China" %in% desired_regions.global, "China", desired_regions.global[1])[1]

  resource_investment <-
    resource_addition %>%
    dplyr::filter(year != base_year_p) %>%
    dplyr::rename(resource = fuel) %>%
    dplyr::group_by(scenario, resource) %>%
    dplyr::mutate(rate = production / production[year == 2015 & region == reg]) %>%
    dplyr::ungroup() %>%
    left_join_strict(
      resource_investment2015 %>%
        dplyr::filter(region == reg) %>%
        dplyr::select(scenario, resource, invest),
      by = c("scenario", "resource")
    ) %>%
    dplyr::bind_rows(resource_addition %>%
                       dplyr::filter(year == base_year_p) %>%
                       dplyr::rename(resource = fuel) %>%
                       dplyr::group_by(scenario, resource) %>%
                       dplyr::mutate(rate = production / production[region == reg]) %>%
                       dplyr::ungroup() %>%
                       left_join_strict(
                         resource_investment2020 %>%
                           dplyr::filter(region == reg) %>%
                           dplyr::select(scenario, resource, invest),
                         by = c("scenario", "resource")
                       )) %>%
    dplyr::mutate(
      value = invest * rate,
      resource = sub("coal", "Coal", resource),
      resource = sub("natural gas", "Gas", resource),
      resource = sub("oil", "Oil", resource),
      resource = sub("uranium", "Uranium", resource),
      var = paste0("Investment|Energy Supply|Extraction|", resource)
    ) %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  resource_investment_clean <-
    resource_investment %>%
    dplyr::group_by(scenario, region, year) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(var = "Investment|Energy Supply|Extraction|Fossil") %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns)) %>%
    dplyr::bind_rows(resource_investment) # %>% dplyr::group_by(scenario, var, year) %>% dplyr::summarise(value = sum(value)) %>% tidyr::spread(year, value)


  # add parent category
  resource_investment_clean <- rbind(
    resource_investment_clean,
    resource_investment_clean %>%
      dplyr::mutate(var = "Investment|Energy Supply") %>%
      dplyr::group_by(scenario, region, var, year) %>%
      dplyr::summarise(value = sum(value)) %>%
      dplyr::ungroup()
  )


  resource_investment_clean <<- resource_investment_clean
}


#' get_total_investment
#'
#' Calculate investment of resource production + extraction.
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal investment process
#' @return `total_investment_clean` global variable
#' @importFrom magrittr %>%
#' @export
get_total_investment <- function(GCAM_version = 'v8.2') {
  total_investment_clean <- NULL

  check_queries("total_investment_clean", GCAM_version)

  total_investment_clean <- dplyr::bind_rows(
    resource_investment_clean,
    elec_investment_clean,
    nonelec_investment_clean) %>%
    dplyr::filter(var == 'Investment|Energy Supply') %>%
    dplyr::group_by(scenario,region,year,var) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  # Remove aggregated value from invest tibbles,
  # now that summed value is in total_investment_clean
  resource_investment_clean <<- resource_investment_clean %>%
    dplyr::filter(var != 'Investment|Energy Supply')

  elec_investment_clean <<- elec_investment_clean %>%
    dplyr::filter(var != 'Investment|Energy Supply')

  nonelec_investment_clean <<- nonelec_investment_clean %>%
    dplyr::filter(var != 'Investment|Energy Supply')


  total_investment_clean <<- total_investment_clean
}


#' get_nonelec_investment
#'
#' Calculate investment for non-electricity energy supply sectors (hydrogen,
#' refining/liquids, gas processing) using GCAM's native "Capital investment
#' demands by tech" query. Values are converted from 1975$/timestep to
#' billion 2010$/yr.
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal investment process
#' @return `nonelec_investment_clean` global variable
#' @importFrom magrittr %>%
#' @export
get_nonelec_investment <- function(GCAM_version = 'v8.2') {
  sector <- subsector <- technology <- var <- value <-
    nonelec_investment_clean <- NULL

  check_queries("nonelec_investment_clean", GCAM_version)

  inv_map <- get(paste('nonelec_investment_map', GCAM_version, sep = '_'),
                 envir = asNamespace("gcamreport"))

  raw <- check_inf(
    rgcam::getQuery(prj, "capital investment demands by tech"),
    dataset_name = "capital investment demands by tech"
  ) %>%
    dplyr::filter(sector %in% unique(inv_map$sector),
                  year %in% years_in_prj)

  # timestep length: 5 years for all future periods
  # annualize and convert 1975$ -> billion 2010$
  conv <- get(paste('convert', GCAM_version, sep = '_'),
              envir = asNamespace("gcamreport"))[['conv_75USD_10USD']]

  nonelec_investment_clean <- raw %>%
    dplyr::inner_join(inv_map, by = c("sector", "subsector", "technology")) %>%
    dplyr::mutate(value = value * conv / 5) %>%
    tidyr::pivot_longer(cols = dplyr::starts_with("var"),
                        values_to = "var", values_drop_na = TRUE) %>%
    dplyr::filter(var != "") %>%
    dplyr::select(-name) %>%
    dplyr::group_by(scenario, region, var, year) %>%
    dplyr::summarise(value = sum(value, na.rm = TRUE)) %>%
    dplyr::ungroup() %>%
    dplyr::select(dplyr::all_of(gcamreport::long_columns))

  nonelec_investment_clean <<- nonelec_investment_clean
}


# Transport
# ==============================================================================================
#' get_transport_sales
#'
#' Computes the regional transport vehicles sales.
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @return `trn_sales_clean` global variable.
#' @keywords internal transport
#' @importFrom magrittr %>%
#' @export
get_transport_sales <- function(GCAM_version = 'v8.2') {
  value <- trn_sales_clean <- NULL

  check_queries('trn_sales_clean', GCAM_version)

  # get transport service
  trn_serv <- check_inf(rgcam::getQuery(prj, "transport service output by tech and vintage"),
                        dataset_name = "transport service output by tech and vintage") %>%
    tidyr::separate(technology, into = c("technology", "vintage"), sep = ",") %>%
    dplyr::mutate(vintage = as.integer(sub("year=", "", vintage))) %>%
    dplyr::filter(vintage <= year) %>%    ##Only vintages from the model year or before will be in existence
    dplyr::rename("mode" = "subsector")

  trn_regions <- unique(trn_serv$region)

  # Use the UCD dataset which has load factors and vehicle miles traveled values
  ucd_core_values <- get(paste('ucd_core',GCAM_version,sep='_'), envir = asNamespace("gcamreport")) %>%
    left_join_error_no_match(get(paste('ucd_size_class',GCAM_version,sep='_'), envir = asNamespace("gcamreport")),
                             by = c('UCD_region', 'mode', 'size.class')) %>%
    # Both types of rail simply called 'Rail' causing errors later
    dplyr::mutate(rev.mode = dplyr::if_else(rev.mode == "Rail",
                                            dplyr::if_else(UCD_sector == "Passenger",
                                                           "Passenger Rail", dplyr::if_else(UCD_sector == "Freight",
                                                                                            "Freight Rail", rev.mode)), rev.mode)) %>%
    dplyr::select(-UCD_fuel, -UCD_sector, -mode, -size.class)

  ucd_techs <- unique(ucd_core_values$UCD_technology)[!(unique(ucd_core_values$UCD_technology) == "All")]

  # Annual vehicle, is the same across all fuels of cars, so explicitly show that:
  ucd_core_loads <- ucd_core_values %>%
    dplyr::filter(variable == "annual travel per vehicle") %>%
    dplyr::mutate(UCD_technology = ifelse(UCD_technology == "All", ucd_techs[1], UCD_technology)) %>%
    tidyr::complete(tidyr::nesting(UCD_region, rev_size.class, rev.mode, variable, unit, year, value), UCD_technology = ucd_techs) %>%
    dplyr::bind_rows(ucd_core_values %>%
                       dplyr::filter(variable == "load factor"))


  ucd_core_gcamRegions <- ucd_core_loads %>%
    dplyr::group_by(UCD_region, rev_size.class, rev.mode, variable, UCD_technology, unit, year) %>%
    dplyr::summarise(value=mean(value, na.rm = T)) %>%
    dplyr::ungroup()

  ## Check that
  ## a) annual travel per vehicle is in vkt/(vehicle*yr)
  ## b) load factors are in pass/vehicle and tonnes/vehicle
  check <- unique(ucd_core_loads %>%
                    dplyr::select(-UCD_region, -year, -value, -UCD_technology))
  if(check %>% dplyr::filter(variable == 'annual travel per vehicle') %>% dplyr::pull(unit) %>% unique() != 'vkt/veh/yr'){
    stop("ERROR: The `Stock|Transportation` variable has a units mismatch. The `annual travel per vehicle` units should be specified as 'vkt/veh/yr'.")
  }
  if(!all(check %>% dplyr::filter(variable == 'load factor') %>% dplyr::pull(unit) %>% unique() %in% c('pers/veh', 'tonnes/veh'))){
    stop("ERROR: The `Stock|Transportation` variable has a units mismatch. The `load factor` units should be specified as 'pers/veh' or 'tonnes/veh'.")
  }

  ucd_core_gcamRegions <- ucd_core_gcamRegions %>%
    dplyr::select(-unit) %>%
    tidyr::pivot_wider(names_from = "variable", values_from = "value")

  # Assume vkt values for trucks
  ucd_core_gcamRegions <- ucd_core_gcamRegions %>%
    dplyr::mutate(`annual travel per vehicle` = dplyr::case_when(
      rev_size.class == "Light truck" ~ 20000,
      rev_size.class == "Medium truck" ~ 35000,
      rev_size.class == "Heavy truck" ~ 50000,
      TRUE ~ `annual travel per vehicle`
    ))

  # See which transportation modes in GCAM are "vintaged",
  # as in show new sales and track vintages through the years
  vintaged_modes = trn_serv %>%
    dplyr::filter(year != vintage,
                  value != 0) %>%
    dplyr::select(sector, mode, Units) %>%
    dplyr::distinct()

  # Map UCD data based on GCAM region mapping
  region_mapping_ucd <- get(paste('region_mapping_ucd',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))
  region_mapping_ucd$GCAM_region <- trimws(region_mapping_ucd$GCAM_region)
  region_mapping_ucd$UCD_region <- trimws(region_mapping_ucd$UCD_region)

  trn_sales_clean <- trn_serv %>%
    # only look at new sales in each year
    dplyr::filter(year == vintage) %>%
    # dplyr left_join due to missing transport modes
    dplyr::left_join(region_mapping_ucd, by = c("region"="GCAM_region")) %>%
    dplyr::left_join(ucd_core_gcamRegions,
                     by = c("UCD_region", "mode"="rev_size.class", "year", "technology"="UCD_technology")) %>%
    dplyr::filter(!(is.na(`annual travel per vehicle`)),
                  !(is.na(`load factor`)),
                  mode %in% unique(vintaged_modes$mode)) %>%
    # Assume constant sales during the 5 year period
    dplyr::mutate(value=(value / `load factor` / `annual travel per vehicle` / 5),
                  Units = "million vehicles") %>%
    left_join_strict(get(paste('transport_sales_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")),
                     by = c("sector", "mode", "technology"), relationship = "many-to-many") %>%
    dplyr::filter(!is.na(var)) %>%
    filter_variables() %>%
    dplyr::mutate(value = value * unit_conv) %>%
    dplyr::group_by(scenario, region, year, var, Units) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(all_of(gcamreport::long_columns))

  trn_sales_clean <<- trn_sales_clean


}

#' get_transport_stock
#'
#' Computes the regional transport vehicles stock.
#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @return `trn_stock_clean` global variable.
#' @keywords internal transport
#' @importFrom magrittr %>%
#' @export
get_transport_stock <- function(GCAM_version = 'v8.2') {
  value <- trn_stock_clean <- NULL

  check_queries('trn_stock_clean', GCAM_version)


  # get transport service
  trn_serv <- check_inf(rgcam::getQuery(prj, "transport service output by tech and vintage"),
                        dataset_name = "transport service output by tech and vintage") %>%
    tidyr::separate(technology, into = c("technology", "vintage"), sep = ",") %>%
    dplyr::mutate(vintage = as.integer(sub("year=", "", vintage))) %>%
    dplyr::filter(vintage <= year) %>%    ##Only vintages from the model year or before will be in existence
    dplyr::rename("mode" = "subsector")

  trn_regions <- unique(trn_serv$region)

  # Use the UCD dataset which has load factors and vehicle miles traveled values
  ucd_core_values <- get(paste('ucd_core',GCAM_version,sep='_'), envir = asNamespace("gcamreport")) %>%
    left_join_error_no_match(get(paste('ucd_size_class',GCAM_version,sep='_'), envir = asNamespace("gcamreport")),
                             by = c('UCD_region', 'mode', 'size.class')) %>%
    # Both types of rail simply called 'Rail' causing errors later
    dplyr::mutate(rev.mode = dplyr::if_else(rev.mode == "Rail",
                                            dplyr::if_else(UCD_sector == "Passenger",
                                                           "Passenger Rail", dplyr::if_else(UCD_sector == "Freight",
                                                                                            "Freight Rail", rev.mode)), rev.mode)) %>%
    dplyr::select(-UCD_fuel, -UCD_sector, -mode, -size.class)

  ucd_techs <- unique(ucd_core_values$UCD_technology)[!(unique(ucd_core_values$UCD_technology) == "All")]

  # Annual vehicle, is the same across all fuels of cars, so explicity show that:
  ucd_core_loads <- ucd_core_values %>%
    dplyr::filter(variable == "annual travel per vehicle") %>%
    dplyr::mutate(UCD_technology = ifelse(UCD_technology == "All", ucd_techs[1], UCD_technology)) %>%
    tidyr::complete(tidyr::nesting(UCD_region, rev_size.class, rev.mode, variable, unit, year, value), UCD_technology = ucd_techs) %>%
    dplyr::bind_rows(ucd_core_values %>%
                       dplyr::filter(variable == "load factor"))


  ucd_core_gcamRegions <- ucd_core_loads %>%
    dplyr::group_by(UCD_region, rev_size.class, rev.mode, variable, UCD_technology, unit, year) %>%
    dplyr::summarise(value=mean(value, na.rm = T)) %>%
    dplyr::ungroup()

  ## Check that
  ## a) annual travel per vehicle is in vkt/(vehicle*yr)
  ## b) load factors are in pass/vehicle and tonnes/vehicle
  check <- unique(ucd_core_loads %>%
                    dplyr::select(-UCD_region, -year, -value, -UCD_technology))
  if(check %>% dplyr::filter(variable == 'annual travel per vehicle') %>% dplyr::pull(unit) %>% unique() != 'vkt/veh/yr'){
    stop("ERROR: The `Stock|Transportation` variable has a units mismatch. The `annual travel per vehicle` units should be specified as 'vkt/veh/yr'.")
  }
  if(!all(check %>% dplyr::filter(variable == 'load factor') %>% dplyr::pull(unit) %>% unique() %in% c('pers/veh', 'tonnes/veh'))){
    stop("ERROR: The `Stock|Transportation` variable has a units mismatch. The `load factor` units should be specified as 'pers/veh' or 'tonnes/veh'.")
  }


  ucd_core_gcamRegions <- ucd_core_gcamRegions %>%
    dplyr::select(-unit) %>%
    tidyr::pivot_wider(names_from = "variable", values_from = "value")

  # Assume vkt values for trucks
  ucd_core_gcamRegions <- ucd_core_gcamRegions %>%
    dplyr::mutate(`annual travel per vehicle` = dplyr::case_when(
      rev_size.class == "Light truck" ~ 20000,
      rev_size.class == "Medium truck" ~ 35000,
      rev_size.class == "Heavy truck" ~ 50000,
      TRUE ~ `annual travel per vehicle`
    ))

  # Map UCD data based on GCAM region mapping
  region_mapping_ucd <- get(paste('region_mapping_ucd',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))
  region_mapping_ucd$GCAM_region <- trimws(region_mapping_ucd$GCAM_region)
  region_mapping_ucd$UCD_region <- trimws(region_mapping_ucd$UCD_region)

  trn_stock_clean <- trn_serv %>%
    # dplyr left_join due to missing transport modes
    dplyr::left_join(region_mapping_ucd, by = c("region"="GCAM_region")) %>%
    dplyr::left_join(ucd_core_gcamRegions,
                     by = c("UCD_region", "mode"="rev_size.class", "year", "technology"="UCD_technology")) %>%
    dplyr::filter(!(is.na(`annual travel per vehicle`)),
                  !(is.na(`load factor`))) %>%
    dplyr::mutate(value=(value / `load factor` / `annual travel per vehicle`),
                  Units = "million vehicles") %>%
    left_join_strict(get(paste('transport_stock_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")),
                     by = c("sector", "mode", "technology"), relationship = "many-to-many") %>%
    dplyr::filter(!is.na(var)) %>%
    filter_variables() %>%
    dplyr::mutate(value = value * unit_conv) %>%
    dplyr::group_by(scenario, region, year, var, Units) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(all_of(gcamreport::long_columns))

  trn_stock_clean <<- trn_stock_clean
}


#########################################################################
#                        BIND TO TEMPLATE FUNCTIONS                     #
#########################################################################
#' do_bind_results
#'
#' Binds results and saves them to an output file.#'
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @param all_tier1 If `TRUE` (not default), introduce all Tier 1 Variables (as 0) in the standardized report.
#' @keywords internal process
#' @return Saved results in an output file.
#' @importFrom magrittr %>%
#' @export
do_bind_results <- function(GCAM_version = 'v8.2', all_tier1 = F) {
  region <- var <- scenario <- year <- value <- . <- na.omit <- Region <- Variable <- NULL

  vars <- .myGlobals$variables.global[.myGlobals$variables.global$required == TRUE, "name"]
  GCAM_DATA <-
    dplyr::bind_rows(lapply(vars, function(x) get(x))) %>%
    dplyr::mutate(
      region = gsub("Global", "World", region),
      region = gsub("global", "World", region)
    )
  GCAM_DATA <- GCAM_DATA %>%
    filter_data_regions(GCAM_version = GCAM_version) %>%
    rbind(GCAM_DATA %>%
            dplyr::filter(region == 'World'))

  # Calculate global total
  GCAM_DATA_WORLD <-
    GCAM_DATA %>%
    dplyr::filter(
      region != "World",
      # excl. price and costs variables - already calculated global value
      !grepl("Price\\|Capital Cost", var),
      !grepl("Price\\|Carbon", var),
      !grepl("Price\\|", var),# agriculture, final/primary/secondary energy
      # excl. Temperature|Forcing|Concentration
      !grepl("Forcing", var),
      !grepl("Temperature\\|Global Mean", var),
      !grepl("Concentration\\|CO2", var),
      # food intake & availability
      !grepl("Food Intake", var),
      !grepl("Food Availability", var),
      # shares
      !grepl("\\[Share\\]", var),
      # yield
      !grepl("Yield", var),
      # growth rates
      !grepl("Rate", var)
    ) %>%
    filter_variables() %>%
    dplyr::group_by(scenario, year, var) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(region = "World")

  GCAM_DATA_wGLOBAL <-
    GCAM_DATA_WORLD %>%
    dplyr::bind_rows(GCAM_DATA %>% dplyr::filter(region != "World")) %>%
    dplyr::bind_rows(GCAM_DATA %>% dplyr::filter(region == "World" & !var %in% unique(GCAM_DATA_WORLD$var))) %>%
    tidyr::complete(tidyr::nesting(scenario, region, var), year = available_reporting_years) %>%
    tidyr::replace_na(list(value = 0)) %>%
    dplyr::distinct(.)

  # filter to final_year.global
  GCAM_DATA_wGLOBAL <- GCAM_DATA_wGLOBAL %>% dplyr::filter(year <= final_year.global)
  report_pre <-
    get(paste('template',GCAM_version,sep='_'), envir = asNamespace("gcamreport")) %>%
    dplyr::inner_join(
      GCAM_DATA_wGLOBAL %>%
        na.omit() %>%
        tidyr::pivot_wider(names_from = "year", values_from = "value"),
      by = c("Variable" = "var"), multiple = "all"
    ) %>%
    dplyr::distinct() %>%
    dplyr::rename(Region = region) %>%
    dplyr::rename(Scenario = scenario)

  # Add year columns if not present
  missing_cols <- setdiff(reporting_columns.global, colnames(report_pre))
  for (col in missing_cols) {
    report_pre <- report_pre %>% dplyr::mutate(!!col := NA)
  }

  report <- report_pre %>%
    dplyr::select(dplyr::all_of(reporting_columns.global)) %>%
    # Drop variables we don't report
    dplyr::filter(!is.na(Region)) %>%
    dplyr::filter(Variable %in% unique(get(paste('template',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['Variable']]))

  # Add "Other" category when variables present as reportable (Internal_variable column not empty in the template)
  missing_var <- get(paste('template',GCAM_version,sep='_'), envir = asNamespace("gcamreport")) %>%
    dplyr::filter(!Variable %in% unique(report$Variable),
                  grepl('Other', Variable),
                  !is.na(Internal_variable))
  year_cols <- names(report)[sapply(names(report), function(x) grepl("^\\d{4}$", x))]
  zero_df <- as.data.frame(matrix(0, nrow = 1, ncol = length(year_cols)))
  year_cols -> colnames(zero_df)

  report <- report %>%
    rbind(missing_var %>%
            dplyr::distinct(Variable, Unit) %>%
            dplyr::mutate(Model = unique(report$Model)[1],
                          Scenario = unique(report$Scenario)[1],
                          Region = unique(report$Region)[1]) %>%
            tidyr::complete(tidyr::nesting(Variable, Unit),
                            Model = unique(report$Model),
                            Scenario = unique(report$Scenario),
                            Region = unique(report$Region)) %>%
            cbind(zero_df))

  # Filter user selected variables
  if (!(length(desired_variables.global) == 1 && desired_variables.global == "All")) {
    report <- report %>%
      dplyr::filter(Variable %in% desired_variables.global)
  }

  # Add all Tier 1 variables; if not present, set them as 0. Set also to 0 the
  # missing region-variable combinations
  if (all_tier1) {
    tier1_variables <- get(paste('template',GCAM_version,sep='_'), envir = asNamespace("gcamreport")) %>%
      # only Tier 1 variables
      dplyr::filter(Tier == 1)

    missing_tier1_variables <- tier1_variables %>%
      # only variables not already present in the standardized report
      dplyr::anti_join(report, by = c("Variable")) %>%
      # select only some variables (the required ones)
      dplyr::filter(grepl('Carbon Capture|Carbon Removal|Emissions',Variable))

    if (nrow(missing_tier1_variables) > 0) {
      # add missing variables
      report_extended <- tidyr::crossing(
        Model = unique(report$Model),
        Scenario = unique(report$Scenario),
        Region = unique(report$Region),
        missing_tier1_variables %>%
          dplyr::select(Variable, Unit) %>%
          dplyr::distinct()
      )
      year_cols <- setdiff(names(report), c("Model", "Scenario", "Region", "Variable", "Unit"))
      missing_tier1_data <- tibble::as_tibble(matrix(0, nrow = nrow(report_extended), ncol = length(year_cols)))
      names(missing_tier1_data) <- year_cols

      report_extended2 <- dplyr::bind_rows(
        report,
        dplyr::bind_cols(report_extended, missing_tier1_data)
      )

      # complete Region-Variable missing pairs (with 0s)
      report_complete <- report_extended2 %>%
        tidyr::complete(tidyr::nesting(Model, Scenario, Region),
                        Variable = unique(report_extended2$Variable),
                        fill = list(year_cols = 0)) %>%
        dplyr::group_by(Variable) %>%
        dplyr::mutate(Unit = dplyr::if_else(is.na(Unit), dplyr::first(na.omit(Unit)), Unit)) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(across(where(is.numeric), ~tidyr::replace_na(., 0))) %>%
        dplyr::arrange(Model, Scenario, Variable, Region)

      report <- report_complete

    }
  }


  report <<- report
}

#########################################################################
#                       CHECKS AND VETTING FUNCTIONS                    #
#########################################################################

#' do_check_inf
#'
#' Verify standardized dataset does not contain Inf values
#' @keywords internal check
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @return A message confirming the success of the vetting process.
#' @importFrom magrittr %>%
#' @export
do_check_inf <- function(GCAM_version = 'v8.2') {
  # Check vetting results from SM
  report_inf_summary <- report %>%
    dplyr::filter(dplyr::if_any(`2005`:dplyr::last_col(), ~ is.infinite(.)))

  # output
  if (nrow(report_inf_summary) == 0) {
    res <- list(
      message = "Inf variables: OK",
      summary = report_inf_summary
    )
  } else {
    res <- list(
      message = "Inf variables: ERROR",
      summary = as.data.frame(report_inf_summary)
    )
  }
  return(res)

}


#' do_check_na
#'
#' Verify standardized dataset does not contain NA values
#' @keywords internal check
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @return A message confirming the success of the vetting process.
#' @importFrom magrittr %>%
#' @export
do_check_na <- function(GCAM_version = 'v8.2') {
  # Check vetting results from SM
  report_na_summary <- report %>%
    dplyr::filter(dplyr::if_any(`2005`:last_col(), ~ is.na(.)))

  # output
  if (nrow(report_na_summary) == 0) {
    res <- list(
      message = "NA variables: OK",
      summary = report_na_summary
    )
  } else {
    res <- list(
      message = "NA variables: ERROR",
      summary = as.data.frame(report_na_summary)
    )
  }
  return(res)

}


#' do_check_vetting
#'
#' Verify vetting and produce plot.
#' @keywords internal check
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @return A message confirming the success of the vetting process.
#' @import ggplot2
#' @importFrom magrittr %>%
#' @export
do_check_vetting <- function(GCAM_version = 'v8.2') {
  year <- value <- Model <- Variable <- Unit <- Scenario <- Region <-
    adj_var <- adj_var2 <- region <- Range <- variable <- value_vet <-
    unit_vet <- check <- type <- NULL

  # Check vetting results from SM
  final_data_long_check <- report %>%
    tidyr::gather(year, value, -Model, -Variable, -Unit, -Scenario, -Region) %>%
    dplyr::rename(
      region = Region,
      variable = Variable
    ) %>%
    dplyr::filter(Scenario == scenarios.global[1]) %>%
    dplyr::mutate(year = as.integer(year))

  check_vet <- gcamreport::global_vet_values %>%
    dplyr::select(variable = adj_var, adj_var2, region, year, value, unit, range = Range) %>%
    dplyr::rename(
      unit_vet = unit,
      value_vet = value
    ) %>%
    # dplyr left_join since you an vet only some items
    dplyr::left_join(final_data_long_check, by = c("variable", "region", "year")) %>%
    tidyr::unnest(value) %>%
    dplyr::mutate(value = dplyr::if_else(grepl("Traditional", variable), value * -1, value)) %>%
    dplyr::select(Scenario, variable = adj_var2, region, year, value, unit = Unit, value_vet, unit_vet, range) %>%
    # Adjust for Solar&Wind and biomass
    dplyr::group_by(Scenario, variable, region, year, unit, unit_vet, range) %>%
    dplyr::summarise(
      value = sum(value),
      value_vet = mean(value_vet)
    ) %>%
    dplyr::ungroup() %>%
    # dplyr::mutate(unit_vet = as.character(unit_vet)) %>%
    dplyr::mutate(
      value_vet = dplyr::if_else(unit_vet == "bcm", value_vet *
                                   get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['bcm_to_EJ']], value_vet),
      unit_vet = dplyr::if_else(unit_vet == "bcm", "EJ/yr", unit_vet)
    ) %>%
    dplyr::mutate(
      diff = (value / value_vet) - 1,
      check = dplyr::if_else(abs(diff) > range, "ERROR", "OK")
    )

  check_vet_summary <- check_vet %>%
    dplyr::rename(
      "computed_value" = "value",
      "expected_value (vetting)" = "value_vet",
      "confidance_range" = "range"
    )

  ## plot
  check_vet_plot <- check_vet %>%
    dplyr::select(-year, -range, -diff, -check, -unit_vet) %>%
    tidyr::gather(type, value, -variable, -Scenario, -unit, -region)

  ggplot2::ggplot(check_vet_plot, ggplot2::aes(x = variable, y = value, fill = type)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::facet_wrap(~variable, scales = "free") +
    ggplot2::labs(x = "", y = "value") +
    ggplot2::theme_classic() +
    ggplot2::theme(
      axis.text.x = ggplot2::element_blank(),
      legend.position = "bottom",
      strip.text = ggplot2::element_text(size = 5),
      legend.title = ggplot2::element_blank()
    )
  if (!dir.exists(file.path(here::here(), "output"))) {
    dir.create(file.path(here::here(), "output"))
  }
  if (!dir.exists(file.path(here::here(), "output", "figure"))) {
    dir.create(file.path(here::here(), "output", "figure"))
  }
  ggplot2::ggsave(file.path(here::here(), "output", "figure", "vetting.tiff"), ggplot2::last_plot(), "tiff", dpi = 200)

  # output
  if (nrow(check_vet_summary[check_vet_summary$check == "ERROR", ]) == 0) {
    res <- list(
      message = "Vetting variables: OK",
      summary = check_vet_summary
    )
  } else if (nrow(check_vet_summary[is.na(check_vet_summary$check), ]) > 0) {
    res <- list(
      message = "Vetting variables: Vetting only performed on some variables",
      summary = check_vet_summary
    )
  } else {
    res <- list(
      message = "Vetting variables: ERROR",
      summary = as.data.frame(check_vet_summary)
    )
  }
  return(res)
}

#########################################################################
#                            INTERNAL FUNCTIONS                         #
#########################################################################

#' update_template
#'
#' Update the template file by incorporating new reported variables and removing unreported ones.
#' @param GCAM_version Name of the GCAM compatible version. Run `available_GCAM_versions()` to see the list of supported options.
#' @keywords internal template
#' @return Updated template saved as both .rda and .csv files in the `inst/extdata` folder.
update_template <- function(GCAM_version = 'v8.2') {
  as_output <- Internal_variable <- Variable <- NULL

  data <- merge(get(paste('template',GCAM_version,sep='_'), envir = asNamespace("gcamreport")),
                data.frame(Variable = unique(report$Variable)) %>%
                  dplyr::mutate("as_output" = TRUE),
                by = "Variable", all = TRUE
  ) %>%
    dplyr::select(colnames(get(paste('template',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))), as_output) %>%
    # if the variable was not given as output, set NA as Internal_variable
    dplyr::mutate(Internal_variable = dplyr::if_else(is.na(as_output), NA, Internal_variable)) %>%
    # if there is a variable given as output but not recorded as so, print it
    dplyr::mutate(
      Variables_outputed_but_not_recorded =
        dplyr::if_else(is.na(Internal_variable) & !is.na(as_output), Variable, NA)
    )
  print(paste0("New variables that can be reported: ", unique(data$Variables_outputed_but_not_recorded)))
  print(paste0(
    "Old variables that are no longer reported: ",
    dplyr::anti_join(
      get(paste('template',GCAM_version,sep='_'), envir = asNamespace("gcamreport")) %>%
        dplyr::filter(!is.na(Internal_variable) & Internal_variable != "") %>%
        dplyr::select(Variable),
      data %>%
        dplyr::filter(!is.na(Internal_variable) & Internal_variable != "") %>%
        dplyr::select(Variable)
    )
  ))

  template <- data %>%
    dplyr::select(colnames(get(paste('template',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))))

  utils::write.csv(template,
                   file = file.path(here::here(), "inst/extdata", "template/reporting_template.csv"),
                   row.names = FALSE
  )
  usethis::use_data(template, overwrite = T)
}

options(summarise.inform = FALSE)

#########################################################################
#                           ANCILLARY FUNCTIONS                         #
#########################################################################


#' filter_desired_regions
#'
#' Return vector of the desired regions available in the loaded project
#' @param des_reg vector of the user desired regions
#' @return vector of the desired regions available in the loaded project
#' @export
filter_desired_regions <- function(des_reg) {
  r <- 1
  rmax <- length(rgcam::listQueries(prj))
  while (r <= rmax) {
    tmp <- rgcam::getQuery(prj, rgcam::listQueries(prj)[r])
    if ("region" %in% colnames(tmp)) {
      des_reg <- unique(tmp$region)
      return(des_reg)
    }
    r <- r + 1
  }
  warning("Desired regions could not be filtered through the loaded project data. The standardize report will contain the regions specified by the user.")
  return(des_reg)
}



#' transform_to_xml
#'
#' Return xml document from a given queries list
#' @param parsed_queries_list parsed list of queries
#' @return xml document containing the given queries list
#' @keywords internal utils
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
#' Return the vector elements starting with the specified parameters
#' @param vector vector to check
#' @param pattern pattern to consider
#' @return subvector of `vector` whose elements start with `pattern`
#' @importFrom magrittr %>%
#' @keywords internal utils
#' @export
start_with_pattern <- function(vector, pattern) {
  matching_elements <- vector[substr(vector, 1, nchar(pattern)) == pattern]
  return(matching_elements)
}


#' filter_loading_regions
#'
#' filter the desired regions of a GCAM project
#' @param data dataframe to be filtered
#' @param desired_regions desired regions to consider. By default, 'All'. Otherwise, specify a vector with all the considered regions.
#' To know all possible regions, run `available_regions()`. ATTENTION: the considered regions will make up "World".
#' In case the project dataset needs to be created, it will be produced with only the specified regions.
#' @param variable dataset variable information
#' @return filtered dataframe
#' @importFrom magrittr %>%
#' @keywords internal utils
#' @export
filter_loading_regions <- function(data, desired_regions = "All", variable) {
  market <- region <- NULL

  if (!(identical(desired_regions, "All"))) {
    # the variable CO2 prices does not contain "region", but "markets". Now we
    # filter for all market items that do not contain the desired regions
    if (variable %in% c("CO2 prices", "supply of all markets")) {
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
    } else if (!(variable %in% c(
      "CO2 concentrations", "global mean temperature",
      "total climate forcing"
    ))) {
      # check the desired regions are available in the data
      avail_reg <- unique(data$region)
      if (!desired_regions %in% avail_reg) {
        not_avail <- setdiff(desired_regions, avail_reg)
        if (length(not_avail) == 1) stop("The desired region ", paste(not_avail, collapse = ""), " is not available in the loaded project. In detail, it is not availabe in the query '", variable, "'.")
        if (length(not_avail) > 1) stop("The desired regions ", paste(not_avail, collapse = ", "), " are not available in the loaded project. In detail, they are not availabe in the query '", variable, "'.")      }
      data <- data %>%
        dplyr::filter(region %in% desired_regions)
    }
  }

  return(data)
}


#' filter_variables
#'
#' filter the desired regions of a GCAM project
#' @param data dataframe to be filtered
#' @param variable variable that requires this data
#' @return filtered dataframe
#' @importFrom magrittr %>%
#' @keywords internal utils
#' @export
filter_variables <- function(data, variable) {
  var <- NULL

  if (variable %in% variables.global[variables.global$required == TRUE, ]$name) {
    if (!(length(desired_variables) == 1 && desired_variables == "All")) {
      if ("var" %in% colnames(data)) {
        data <- data %>%
          dplyr::filter(var %in% desired_variables)
      }
    }
  }

  return(invisible(data))
}


#' gather_map
#'
#' Formats all maps into a long table
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
#' Covert GHG to CO2e
#' @param data dataset
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @param GWP_version Global Warming Potential values version: AR5 (default), AR6, or AR4.
#' @importFrom magrittr %>%
#' @keywords internal conversion
#' @export
conv_ghg_co2e <- function(data, GCAM_version = 'v7.0', GWP_version = 'AR5') {
  ghg <- variable <- value <- GWP <- NULL

  # GHG emission conversion
  res <- suppressWarnings(
    data %>%
      # aggregate by ghg (exclude sector)
      tidyr::separate(ghg, into = c("variable", "sector"), sep = "_", fill = "right") %>%
      dplyr::filter(variable %in% get(paste('GHG_gases',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))) %>%
      dplyr::left_join(get(paste('ghg_GWP',GWP_version,sep='_'), envir = asNamespace("gcamreport")), by = c("variable" = "GHG_gases", "sector")) %>%
      dplyr::mutate(value = value * GWP, Units = "CO2e") %>%
      dplyr::filter(!is.na(value)) %>% # remove NAs due to unexisting subsectors
      dplyr::select(-GWP)
  )

  return(res)
}


#' conv_EJ_GW
#'
#' Covert EJ to GW
#' @param data dataset
#' @param cf conversion factor
#' @param EJ EJ amount
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @importFrom magrittr %>%
#' @keywords internal conversion
#' @export
conv_EJ_GW <- function(data, cf, EJ, GCAM_version = "v7.0") {
  data %>%
    dplyr::mutate(gw = EJ / (cf *
                        get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['hr_per_yr']] *
                        get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['EJ_to_GWh']]))
}

#' approx_fun
#'
#' Interpolation function
#' @param year year to consider
#' @param value values to extrapolate from
#' @param rule number of points to extrapolate
#' @importFrom magrittr %>%
#' @keywords internal utils
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


#########################################################################
#                         LOAD QUERIES FUNCTIONS                        #
#########################################################################

# Scioeconomics
# ==============================================================================================
#' get_population
#'
#' Get population query and change units to [million].
#' @keywords internal population
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @return population_clean global variable
#' @importFrom magrittr %>%
#' @export
get_population <- function(GCAM_version = "v7.0") {
  value <- NULL

  population_clean <<-
    rgcam::getQuery(prj, "population by region") %>%
    dplyr::mutate(
      value = value *
        get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_thousand_million']],
      var = "Population"
    ) %>%
    dplyr::select(all_of(gcamreport::long_columns))
}


#' get_gdp_ppp
#'
#' Get GDP (PPP) query, compute regional GDP, and change units to [10USD].
#' @keywords internal GDP
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @return GDP_PPP_clean global variable
#' @importFrom magrittr %>%
#' @export
get_gdp_ppp <- function(GCAM_version = "v7.0") {
  value <- pop_mill <- NULL

  GDP_PPP_clean <<-
    rgcam::getQuery(prj, "GDP per capita PPP by region") %>%
    dplyr::left_join(population_clean %>% dplyr::rename(pop_mill = value), by = c("scenario", "region", "year")) %>%
    dplyr::mutate(
      value = value * pop_mill * get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_90USD_10USD']],      var = "GDP|PPP"
    ) %>%
    dplyr::select(all_of(gcamreport::long_columns))
}


#' get_gdp_mer
#'
#' Get GDP (MER) query and change units to [10USD].
#' @keywords internal GDP
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @return GDP_MER_clean global variable
#' @importFrom magrittr %>%
#' @export
get_gdp_mer <- function(GCAM_version = "v7.0") {
  value <- NULL

  GDP_MER_clean <<-
    rgcam::getQuery(prj, "GDP MER by region") %>%
    dplyr::mutate(
      value = value *
        get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_million_billion']] *
        get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_90USD_10USD']],
      var = "GDP|MER"
    ) %>%
    dplyr::select(all_of(gcamreport::long_columns))
}

# Climate and emissions
# ==============================================================================================
#' get_forcing
#'
#' Get World's forcing query.
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @keywords internal forcing
#' @return forcing_clean global variable
#' @importFrom magrittr %>%
#' @export
get_forcing <- function(GCAM_version = "v7.0") {
  year <- NULL

  forcing_clean <<-
    rgcam::getQuery(prj, "total climate forcing") %>%
    dplyr::filter(year %in% get(paste('GCAM_years',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))) %>%
    dplyr::mutate(var = "Forcing", region = "World") %>%
    dplyr::select(all_of(gcamreport::long_columns))
}


#' get_temperature
#'
#' Get World's mean temperature query.
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @keywords internal temperature
#' @return global_temp_clean global variable
#' @importFrom magrittr %>%
#' @export
get_temperature <- function(GCAM_version = "v7.0") {
  year <- NULL

  global_temp_clean <<-
    rgcam::getQuery(prj, "global mean temperature") %>%
    dplyr::filter(year %in% get(paste('GCAM_years',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))) %>%
    dplyr::mutate(var = "Temperature|Global Mean", region = "World") %>%
    dplyr::select(all_of(gcamreport::long_columns))
}


#' get_co2_concentration
#'
#' Get World's CO2 concentration query.
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @keywords internal co2
#' @return co2_concentration_clean global variable
#' @importFrom magrittr %>%
#' @export
get_co2_concentration <- function(GCAM_version = "v7.0") {
  year <- NULL

  co2_concentration_clean <<-
    rgcam::getQuery(prj, "CO2 concentrations") %>%
    dplyr::filter(year %in% get(paste('GCAM_years',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))) %>%
    dplyr::mutate(var = "Concentration|CO2", region = "World") %>%
    dplyr::select(all_of(gcamreport::long_columns))
}


#' get_co2
#'
#' Get World's CO2 emissions query.
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @keywords internal co2
#' @return co2_clean global variable
#' @importFrom magrittr %>%
#' @export
get_co2 <- function(GCAM_version = "v7.0") {
  value <- unit_conv <- scenario <- region <- year <- var <- NULL

  co2_clean <<-
    tibble::as_tibble(rgcam::getQuery(prj, "CO2 emissions by sector (no bio) (excluding resource production)")) %>%
    dplyr::left_join(filter_variables(get(paste('co2_sector_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")), "co2_clean"), by = "sector", multiple = "all") %>%
    dplyr::mutate(value = value * unit_conv) %>%
    dplyr::group_by(scenario, region, year, var) %>% #
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(all_of(gcamreport::long_columns))
}


# Get CO2 emissions by tech, to break out ships vs rail vs aviation
# and to get Emissions|CO2|Energy| Coal vs Gas vs Oil.
# Must create CO2 emissions by tech (no bio) output first to be consistent. There is no query for this

# Apply bio negative emissions by joining by sector and by sector (no bio) and finding share of non-bio emissions

#' get_nonbio_tmp
#'
#' Get no bio CO2 emissions query by sector.
#' @keywords internal co2
#' @return nonbio_share global variable
#' @importFrom magrittr %>%
#' @export
get_nonbio_tmp <- function() {
  value.y <- value.x <- NULL

  nonbio_share <<-
    rgcam::getQuery(prj, "CO2 emissions by sector (excluding resource production)") %>%
    dplyr::left_join(rgcam::getQuery(prj, "CO2 emissions by sector (no bio) (excluding resource production)"), by = c("region", "scenario", "year", "sector", "Units")) %>%
    dplyr::mutate(
      value.y = dplyr::if_else(is.na(value.y), value.x, value.y),
      percent = value.y / value.x
    ) %>%
    dplyr::select(-value.x, -value.y)
}


#' get_co2_tech_nobio_tmp
#'
#' Get no bio CO2 emissions query by sector and techonolgy.
#' @keywords internal co2 tmp
#' @return co2_tech_nobio global variable
#' @importFrom magrittr %>%
#' @export
get_co2_tech_nobio_tmp <- function() {
  value <- percent <- NULL

  co2_tech_nobio <<-
    rgcam::getQuery(prj, "CO2 emissions by tech (excluding resource production)") %>%
    dplyr::left_join(nonbio_share, by = c("region", "scenario", "year", "sector", "Units")) %>%
    dplyr::mutate(value = value * percent) %>%
    dplyr::select(-percent)
}


#' get_co2_tech_emissions_tmp
#'
#' Get no bio CO2 emissions query by sector, subsector, and techonolgy.
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @keywords internal co2 tmp
#' @return co2_tech_emissions global variable
#' @importFrom magrittr %>%
#' @export
get_co2_tech_emissions_tmp <- function(GCAM_version = "v7.0") {
  var <- value <- unit_conv <- scenario <- region <- year <- NULL

  co2_tech_emissions <<-
    co2_tech_nobio %>%
    dplyr::left_join(filter_variables(get(paste('co2_tech_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")), "co2_tech_emissions"),
              by = c("sector", "subsector", "technology"), multiple = "all") %>%
    dplyr::filter(!is.na(var)) %>%
    dplyr::mutate(value = value * unit_conv) %>%
    dplyr::group_by(scenario, region, year, var) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(all_of(gcamreport::long_columns))
}


# Iron and Steel Emissions: for Emissions|CO2|Coal, Gas, Oil
# Find which input has the greatest share for each IRONSTL tech (between coal, gas, oil)

#' get_iron_steel_map
#'
#' Get iron and steel emissions.
#' @keywords internal iron steel
#' @return iron_steel_map global variable
#' @importFrom magrittr %>%
#' @export
get_iron_steel_map <- function() {
  sector <- input <- value <- Units <- scenario <- NULL

  iron_steel_map <<-
    rgcam::getQuery(prj, "industry final energy by tech and fuel") %>%
    dplyr::filter(
      sector == "iron and steel",
      input %in% c("wholesale gas", "refined liquids industrial", "delivered coal")
    ) %>%
    dplyr::mutate(
      max = max(value),
      save = dplyr::if_else(value == max, 1, 0)
    ) %>%
    dplyr::filter(save == 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(-save, -max, -Units, -scenario, -value)
}


#' get_co2_iron_steel
#'
#' Get iron and steel CO2 emissions.
#' @keywords internal iron steel co2
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @return co2_tech_ironsteel global variable
#' @importFrom magrittr %>%
#' @export
get_co2_iron_steel <- function(GCAM_version = "v7.0") {
  sector <- input <- value <- scenario <- region <- year <- var <- na.omit <- NULL

  co2_tech_ironsteel <<-
    co2_tech_nobio %>% # Using redistributed bio version
    dplyr::filter(sector == "iron and steel") %>%
    dplyr::left_join(filter_variables(iron_steel_map, "co2_tech_ironsteel"), by = c("sector", "subsector", "technology", "year", "region")) %>%
    dplyr::mutate(
      input = stringr::str_replace(input, "wholesale gas", "Emissions|CO2|Energy|Gas"),
      input = stringr::str_replace(input, "refined liquids industrial", "Emissions|CO2|Energy|Oil"),
      input = stringr::str_replace(input, "delivered coal", "Emissions|CO2|Energy|Coal")
    ) %>%
    dplyr::rename(var = input) %>%
    dplyr::mutate(value = value *
             get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_C_CO2']]) %>%
    dplyr::group_by(scenario, region, year, var) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    na.omit() %>%
    dplyr::select(all_of(gcamreport::long_columns))
}


#' get_lu_co2
#'
#' Get land use CO2 emissions.
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @keywords internal lu co2
#' @return LU_carbon_clean global variable
#' @importFrom magrittr %>%
#' @export
get_lu_co2 <- function(GCAM_version = "v7.0") {
  year <- scenario <- region <- value <- var <- NULL

  LU_carbon_clean <<-
    # Land use CO2
    rgcam::getQuery(prj, "LUC emissions by region") %>%
    dplyr::filter(year %in% get(paste('GCAM_years',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))) %>%
    dplyr::group_by(scenario, region, year) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      value = value * get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_C_CO2']],
      var = "Emissions|CO2|AFOLU"
    ) %>%
    dplyr::select(all_of(gcamreport::long_columns)) %>%
    dplyr::group_by(scenario, var, year)
}


#' get_co2_emissions
#'
#' Combine CO2 emission queries.
#' @keywords internal co2 process
#' @return co2_emissions_clean global variable
#' @importFrom magrittr %>%
#' @export
get_co2_emissions <- function() {
  scenario <- region <- year <- var <- value <- NULL

  co2_emissions_clean <<-
    dplyr::bind_rows(co2_clean, LU_carbon_clean, co2_tech_emissions) %>%
    dplyr::group_by(scenario, region, year, var) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(all_of(gcamreport::long_columns))
}

#' get_total_co2_emissions
#'
#' Compute total CO2 emission.
#' @keywords internal co2 process
#' @return tot_co2_clean global variable
#' @importFrom magrittr %>%
#' @export
get_total_co2_emissions <- function() {
  var <- scenario <- region <- year <- value <- NULL

  tot_co2_clean <<-
    dplyr::bind_rows(co2_clean %>% dplyr::filter(var == "Emissions|CO2|Energy and Industrial Processes"), LU_carbon_clean) %>%
    dplyr::group_by(scenario, region, year) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(var = "Emissions|CO2") %>%
    dplyr::select(all_of(gcamreport::long_columns))
}


#' get_nonco2_emissions
#'
#' Get non CO2 emissions query.
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @keywords internal nonco2
#' @return nonco2_clean global variable
#' @importFrom magrittr %>%
#' @export
get_nonco2_emissions <- function(GCAM_version = "v7.0") {
  value <- unit_conv <- scenario <- region <- year <- var <- NULL

  nonco2_clean <<-
    rgcam::getQuery(prj, "nonCO2 emissions by sector (excluding resource production)") %>%
    dplyr::left_join(filter_variables(get(paste('nonco2_emis_sector_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")), "nonco2_clean"),
              by = c("ghg", "sector"), multiple = "all") %>%
    dplyr::bind_rows(rgcam::getQuery(prj, "nonCO2 emissions by resource production") %>%
      dplyr::left_join(filter_variables(get(paste('nonco2_emis_resource_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")), "nonco2_clean"),
                by = c("ghg", "resource"), multiple = "all")) %>%
    dplyr::mutate(value = value * unit_conv) %>%
    dplyr::group_by(scenario, region, year, var) %>% #
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(all_of(gcamreport::long_columns))
}

#' get_fgas
#'
#' Compute F-Gases emissions.
#' @keywords internal f-gases process
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @param GWP_version Global Warming Potential values version: AR5 (default), AR6, or AR4.
#' @return f_gas_clean global variable
#' @importFrom magrittr %>%
#' @export
get_fgas <- function(GCAM_version = "v7.0", GWP_version = 'AR5') {
  ghg <- variable <- scenario <- region <- year <- value <- NULL

  f_gas_clean <<-
    rgcam::getQuery(prj, "nonCO2 emissions by region") %>%
    conv_ghg_co2e(GWP_version = GWP_version) %>%
    dplyr::filter(variable %in% get(paste('F_GASES',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))) %>%
    dplyr::group_by(scenario, region, year) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(var = "Emissions|F-Gases") %>%
    dplyr::select(all_of(gcamreport::long_columns))
}


#' get_ghg
#'
#' Get total GHG emissions.
#' @keywords internal ghg
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @param GWP_version Global Warming Potential values version: AR5 (default), AR6, or AR4.
#' @return ghg_clean global variable
#' @importFrom magrittr %>%
#' @export
get_ghg <- function(GCAM_version = "v7.0", GWP_version = 'AR5') {
  ghg <- variable <- scenario <- region <- year <- value <- NULL

  ghg_all <<-
    rgcam::getQuery(prj, "nonCO2 emissions by region") %>%
    conv_ghg_co2e(GWP_version = GWP_version) %>%
    dplyr::filter(variable %in% get(paste('GHG_GASES',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))) %>%
    dplyr::bind_rows(LU_carbon_clean) %>%
    dplyr::group_by(scenario, region, year) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(var = "Emissions|Kyoto Gases") %>%
    dplyr::select(all_of(gcamreport::long_columns))
}


#' get_ghg_sector
#'
#' Get sectorial GHG emissions.
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @param GWP_version Global Warming Potential values version: AR5 (default), AR6, or AR4.
#' @keywords internal ghg
#' @return ghg_sector_clean global variable
#' @importFrom magrittr %>%
#' @export
get_ghg_sector <- function(GCAM_version = "v7.0", GWP_version = 'AR5') {
  ghg <- resource <- subresource <- sector <- variable <- scenario <-
    region <- var <- year <- value <- NULL

  ghg_sector_clean <<-
    rgcam::getQuery(prj, "nonCO2 emissions by sector (excluding resource production)") %>%
    dplyr::filter(!grepl("CO2", ghg),) %>%
    dplyr::bind_rows(rgcam::getQuery(prj, "nonCO2 emissions by resource production") %>%
      dplyr::rename(sector = resource) %>%
      dplyr::select(-subresource)) %>%
    dplyr::bind_rows(rgcam::getQuery(prj, "CO2 emissions by sector (no bio) (excluding resource production)") %>%
      dplyr::mutate(ghg = "CO2")) %>%
    dplyr::mutate(subsector = sector) %>%
    conv_ghg_co2e(GWP_version = GWP_version) %>%
    dplyr::filter(variable %in% get(paste('GHG_gases',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))) %>%
    dplyr::rename(ghg = variable) %>%
    dplyr::left_join(filter_variables(get(paste('kyoto_sector_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")), "ghg_sector_clean"), by = c("ghg", "subsector", "sector"), multiple = "all") %>%
    dplyr::select(all_of(gcamreport::long_columns)) %>%
    dplyr::bind_rows(
      LU_carbon_clean %>%
        dplyr::mutate(var = "Emissions|Kyoto Gases"),
      LU_carbon_clean %>%
        dplyr::mutate(var = "Emissions|Kyoto Gases|AFOLU")
    ) %>%
    dplyr::group_by(scenario, region, var, year) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup()
}


#' get_co2_sequestration
#'
#' Get carbon sequestration.
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @keywords internal co2
#' @return co2_sequestration_clean global variable
#' @importFrom magrittr %>%
#' @export
get_co2_sequestration <- function(GCAM_version = "v7.0") {
  scenario <- region <- year <- var <- value <- unit_conv <- NULL

  co2_sequestration_clean <<- suppressWarnings(
    rgcam::getQuery(prj, "CO2 sequestration by tech") %>%
      dplyr::left_join(filter_variables(get(paste('carbon_seq_tech_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")), "co2_sequestration_clean"), by = c("sector", "technology"), multiple = "all") %>%
      tidyr::complete(tidyr::nesting(scenario, region, year),
        var = unique(var),
        fill = list(value = 0)
      ) %>%
      dplyr::filter(!is.na(var)) %>% # , var!= "Carbon Sequestration|Feedstocks",var != "Carbon Sequestration|Feedstocks|Liquids") %>%
      dplyr::mutate(value = value * unit_conv) %>%
      dplyr::group_by(scenario, region, year, var) %>% #
      dplyr::summarise(value = sum(value, na.rm = T)) %>%
      dplyr::ungroup() %>% # tidyr::spread(year, value) -> d
      dplyr::select(all_of(gcamreport::long_columns))
  )
}


# Agriculture and land use
# ==============================================================================================
#' get_ag_demand
#'
#' Get agricultural demand.
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @keywords internal ag
#' @return ag_demand_clean global variable
#' @importFrom magrittr %>%
#' @export
get_ag_demand <- function(GCAM_version = "v7.0") {
  sector <- input <- var <- value <- unit_conv <- scenario <- region <- year <- NULL

  ag_demand_clean <<-
    dplyr::bind_rows(
      rgcam::getQuery(prj, "demand balances by crop commodity"),
      rgcam::getQuery(prj, "demand balances by meat and dairy commodity")
    ) %>%
    # Adjust OtherMeat_Fish
    dplyr::mutate(sector = dplyr::if_else(sector == "FoodDemand_NonStaples" & input == "OtherMeat_Fish", "OtherMeat_Fish", sector)) %>%
    dplyr::left_join(filter_variables(get(paste('ag_demand_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")), "ag_demand_clean"),
              by = c("sector"), multiple = "all") %>%
    dplyr::filter(!is.na(var)) %>%
    dplyr::mutate(value = value * unit_conv) %>%
    dplyr::group_by(scenario, region, year, var) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(all_of(gcamreport::long_columns))
}


#' get_ag_production
#'
#' Get agricultural production.
#' @keywords internal ag
#' @return ag_production_clean global variable
#' @importFrom magrittr %>%
#' @export
get_ag_production <- function() {
  Units <- scenario <- region <- year <- var <- value <- NULL

  ag_production_clean <<-
    rgcam::getQuery(prj, "ag production by crop type") %>%
    dplyr::filter(Units == "Mt") %>%
    # Forests produce in units of billion m3 and biomass produces in EJ. We'll need to find a conversion factor to include it
    # 1 m3 = .001 tons
    dplyr::mutate(var = "Agricultural Production") %>%
    dplyr::group_by(scenario, region, year, var) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(all_of(gcamreport::long_columns))
}


#' get_land
#'
#' Get land use area.
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @keywords internal ag
#' @return land_clean global variable
#' @importFrom magrittr %>%
#' @export
get_land <- function(GCAM_version = "v7.0") {
  value <- unit_conv <- scenario <- region <- year <- var <- NULL

  land_clean <<-
    rgcam::getQuery(prj, "aggregated land allocation") %>%
    dplyr::left_join(filter_variables(get(paste('land_use_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")), "land_clean"), by = c("landleaf"), multiple = "all") %>%
    dplyr::mutate(value = value * unit_conv) %>%
    dplyr::group_by(scenario, region, year, var) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup()
}


# Primary Energy
# ==============================================================================================
#' get_primary_energy
#'
#' Get primary energy consumption by tech.
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @keywords internal energy
#' @return primary_energy_clean global variable
#' @importFrom magrittr %>%
#' @export
get_primary_energy <- function(GCAM_version = "v7.0") {
  fuel <- Units <- year <- var <- value <- unit_conv <- scenario <- region <- NULL

  primary_energy_clean <<-
    rgcam::getQuery(prj, "primary energy consumption with CCS by region (direct equivalent)") %>%
    dplyr::filter(
      !grepl("water", fuel),
      Units == "EJ"
    ) %>%
    dplyr::left_join(filter_variables(get(paste('primary_energy_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")), "primary_energy_clean"), by = c("fuel"), multiple = "all") %>%
    dplyr::filter(!is.na(var)) %>%
    dplyr::mutate(value = value * unit_conv) %>%
    dplyr::group_by(scenario, region, year, var) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    tidyr::complete(tidyr::nesting(scenario, region, year),
      var = unique(var),
      fill = list(value = 0)
    ) %>%
    dplyr::select(all_of(gcamreport::long_columns))
}


#' get_energy_trade_prod
#'
#' Get energy trade.
#' @keywords internal energy
#' @return energy_trade_prod global variable
#' @importFrom magrittr %>%
#' @export
get_energy_trade_prod <- function() {
  Units <- resource <- scenario <- region <- year <- value <- NULL

  energy_trade_prod <<-
    rgcam::getQuery(prj, "resource production") %>%
    dplyr::filter(Units == "EJ") %>%
    dplyr::filter(resource %in% c("coal", "natural gas", "crude oil", "unconventional oil")) %>%
    dplyr::mutate(
      resource = sub("crude oil", "oil", resource),
      resource = sub("unconventional oil", "oil", resource)
    ) %>%
    dplyr::group_by(scenario, resource, region, year) %>%
    dplyr::summarise(production = sum(value)) %>%
    dplyr::ungroup()
}


#' get_energy_trade_tmp
#'
#' Get energy trade supply. Query used to compute other variables.
#' @keywords internal energy tmp
#' @return energy_trade_supply global variable
#' @importFrom magrittr %>%
#' @export
get_energy_trade_tmp <- function() {
  market <- resource <- scenario <- region <- year <- value <- NULL

  energy_trade_supply <<- suppressWarnings(
    rgcam::getQuery(prj, "supply of all markets") %>%
      dplyr::filter(grepl("regional coal", market) | grepl("regional natural gas", market) | grepl("regional oil", market)) %>%
      tidyr::separate(market, into = c("region", "resource"), sep = "regional ", fill = "right") %>%
      dplyr::filter(resource != "oilpalm", resource != "oilcrop") %>%
      dplyr::group_by(scenario, resource, region, year) %>%
      dplyr::summarise(demand = sum(value)) %>%
      dplyr::ungroup()
  )
}


#' get_energy_trade
#'
#' Get energy trade supply.
#' @keywords internal energy tmp
#' @return energy_trade_clean global variable
#' @importFrom magrittr %>%
#' @export
get_energy_trade <- function() {
  production <- demand <- resource <- NULL

  energy_trade_clean <<-
    energy_trade_prod %>%
    dplyr::left_join(energy_trade_supply, by = c("scenario", "resource", "region", "year")) %>%
    dplyr::mutate(
      value = production - demand,
      resource = sub("coal", "Coal", resource),
      resource = sub("natural gas", "Gas", resource),
      resource = sub("oil", "Oil", resource),
      var = paste0("Trade|Primary Energy|", resource, "|Volume")
    ) %>%
    filter_variables(variable = "energy_trade_clean") %>%
    dplyr::select(all_of(gcamreport::long_columns))
}

# Secondary Energy
# ==============================================================================================
#' get_elec_gen_tech
#'
#' Get electricity generation
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @keywords internal electricity
#' @return elec_gen_tech_clean global variable
#' @importFrom magrittr %>%
#' @export
get_elec_gen_tech <- function(GCAM_version = "v7.0") {
  var <- value <- unit_conv <- scenario <- region <- year <- NULL

  elec_gen_tech_clean <<-
    rgcam::getQuery(prj, "elec gen by gen tech") %>%
    dplyr::left_join(filter_variables(get(paste('elec_gen_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")), "elec_gen_tech_clean"), by = c("output", "subsector", "technology"), multiple = "all") %>%
    dplyr::filter(!is.na(var)) %>%
    dplyr::mutate(value = value * unit_conv) %>%
    dplyr::group_by(scenario, region, year, var) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    tidyr::complete(tidyr::nesting(scenario, region, year),
      var = unique(var),
      fill = list(value = 0)
    ) %>%
    dplyr::select(all_of(gcamreport::long_columns))
}


#' get_secondary_solids
#'
#' Get secondary solids
#' @keywords internal energy
#' @return secondary_solids global variable
#' @importFrom magrittr %>%
#' @export
get_secondary_solids <- function() {
  input <- scenario <- region <- year <- value <- NULL

  secondary_solids <<-
    rgcam::getQuery(prj, "inputs by sector") %>%
    dplyr::filter(input %in% c("delivered biomass", "delivered coal")) %>%
    dplyr::group_by(scenario, region, year, input) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(var = ifelse(input == "delivered biomass", "Secondary Energy|Solids|Biomass",
      "Secondary Energy|Solids|Coal"
    )) %>%
    dplyr::bind_rows(rgcam::getQuery(prj, "inputs by sector") %>%
      dplyr::filter(input %in% c("delivered biomass", "delivered coal")) %>%
      dplyr::group_by(scenario, region, year) %>%
      dplyr::summarise(value = sum(value, na.rm = T)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(var = "Secondary Energy|Solids")) %>%
    filter_variables(variable = "secondary_solids") %>%
    dplyr::select(all_of(gcamreport::long_columns))
}


#' get_se_gen_tech
#'
#' Get other secondary energy production
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @keywords internal energy
#' @return se_gen_tech_clean global variable
#' @importFrom magrittr %>%
#' @export
get_se_gen_tech <- function(GCAM_version = "v7.0") {
  var <- value <- unit_conv <- scenario <- region <- year <- NULL

  se_gen_tech_clean <<-
    dplyr::bind_rows(
      rgcam::getQuery(prj, "gas production by tech"),
      rgcam::getQuery(prj, "hydrogen production by tech"),
      rgcam::getQuery(prj, "refined liquids production by tech")
    ) %>%
    dplyr::left_join(filter_variables(get(paste('se_gen_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")), "se_gen_tech_clean"), by = c("sector", "subsector", "technology"), multiple = "all") %>%
    dplyr::filter(!is.na(var)) %>%
    dplyr::mutate(value = value * unit_conv) %>%
    dplyr::group_by(scenario, region, year, var) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    tidyr::complete(tidyr::nesting(scenario, region, year),
      var = unique(var),
      fill = list(value = 0)
    ) %>%
    dplyr::select(all_of(gcamreport::long_columns)) %>%
    dplyr::bind_rows(secondary_solids)
}


# Final Energy
# ==============================================================================================
# demand by sector by technology

#' get_fe_sector_tmp
#'
#' Get final energy demand by sector
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @keywords internal energy tmp
#' @return fe_sector global variable
#' @importFrom magrittr %>%
#' @export
get_fe_sector_tmp <- function(GCAM_version = "v7.0") {
  var <- value <- unit_conv <- scenario <- region <- year <- NULL

  fe_sector <<-
    rgcam::getQuery(prj, "final energy consumption by sector and fuel") %>%
    dplyr::left_join(filter_variables(get(paste('final_energy_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")), "fe_sector"),
              by = c("sector", "input"), multiple = "all") %>%
    dplyr::filter(!is.na(var)) %>%
    dplyr::mutate(value = value * unit_conv) %>%
    dplyr::group_by(scenario, region, year, var) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    tidyr::complete(tidyr::nesting(scenario, region, year),
      var = unique(var),
      fill = list(value = 0)
    ) %>%
    dplyr::select(all_of(gcamreport::long_columns))
}


#' get_fe_transportation_tmp
#'
#' Get mode-specific transport final energy to break out rail, ship, and domestic air.
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @keywords internal energy  tmp
#' @return fe_transportation global variable
#' @importFrom magrittr %>%
#' @export
get_fe_transportation_tmp <- function(GCAM_version = "v7.0") {
  var <- value <- unit_conv <- scenario <- region <- year <- NULL

  fe_transportation <<-
    rgcam::getQuery(prj, "transport final energy by mode and fuel") %>%
    dplyr::left_join(filter_variables(get(paste('transport_final_en_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")), "fe_transportation"), by = c("sector", "input", "mode"), multiple = "all") %>%
    dplyr::filter(!is.na(var)) %>%
    dplyr::mutate(value = value * unit_conv) %>%
    dplyr::group_by(scenario, region, year, var) %>%
    dplyr::summarise(value = sum(value,
      na.rm = T
    )) %>%
    dplyr::ungroup() %>%
    tidyr::complete(tidyr::nesting(scenario, region, year),
      var = unique(var),
      fill = list(value = 0)
    ) %>%
    dplyr::select(all_of(gcamreport::long_columns))
}


#' get_fe_sector
#'
#' Compute final energy.
#' GPK 1/6/2022 - because some reporting categories are mapped from both the sector-level and subsector-level queries,
#' we need a step of aggregation here. For example, international and domestic air are both assigned to
#' aviation; international is mapped from the sector-level query, and domestic is mapped from the subsector (mode).
#' Without this step there would be duplicate entries with different data for the same reporting categories.
#' @keywords internal energy process
#' @return fe_sector_clean global variable
#' @importFrom magrittr %>%
#' @export
get_fe_sector <- function() {
  scenario <- region <- var <- year <- value <- NULL

  fe_sector_clean <<-
    dplyr::bind_rows(fe_sector, fe_transportation) %>%
    dplyr::group_by(scenario, region, var, year) %>%
    dplyr::summarise(value = sum(value)) %>%
    dplyr::ungroup()
}


# Energy Service ----------------------------------------------------------

#' get_energy_service_transportation
#'
#' Get transport.
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @keywords internal energy
#' @return energy_service_transportation_clean global variable
#' @importFrom magrittr %>%
#' @export
get_energy_service_transportation <- function(GCAM_version = "v7.0") {
  var <- value <- unit_conv <- scenario <- region <- year <- NULL

  energy_service_transportation_clean <<-
    rgcam::getQuery(prj, "transport service output by mode") %>%
    dplyr::left_join(filter_variables(get(paste('transport_en_service',GCAM_version,sep='_'), envir = asNamespace("gcamreport")), "energy_service_transportation_clean"), by = c("sector", "mode"), multiple = "all") %>%
    dplyr::filter(!is.na(var)) %>%
    dplyr::mutate(value = value * unit_conv) %>%
    dplyr::group_by(scenario, region, year, var) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(all_of(gcamreport::long_columns))
}


#' get_energy_service_buildings
#'
#' Get ES buildings.
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @keywords internal energy
#' @return energy_service_buildings_clean global variable
#' @importFrom magrittr %>%
#' @export
get_energy_service_buildings <- function(GCAM_version = "v7.0") {
  var <- value <- unit_conv <- scenario <- region <- year <- NULL

  energy_service_buildings_clean <<-
    rgcam::getQuery(prj, "building floorspace") %>%
    dplyr::left_join(filter_variables(get(paste('buildings_en_service',GCAM_version,sep='_'), envir = asNamespace("gcamreport")), "energy_service_buildings_clean"), by = c("building"), multiple = "all") %>%
    dplyr::filter(!is.na(var)) %>%
    dplyr::mutate(value = value * unit_conv) %>%
    dplyr::group_by(scenario, region, year, var) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(all_of(gcamreport::long_columns))
}



# industry production
# could add chemicals but they're in terms of EJ, need to also add cement

#' get_industry_production
#'
#' Get industry production.
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @keywords internal energy
#' @return industry_production_clean global variable
#' @importFrom magrittr %>%
#' @export
get_industry_production <- function(GCAM_version = "v7.0") {
  var <- scenario <- region <- year <- value <- NULL

  industry_production_clean <<-
    rgcam::getQuery(prj, "industry primary output by sector") %>%
    dplyr::left_join(filter_variables(get(paste('production_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")), "industry_production_clean"), by = c("sector")) %>%
    # filter variables that are in terms of Mt
    dplyr::filter(var %in% c("Production|Cement", "Production|Steel", "Production|Non-ferrous metals")) %>%
    dplyr::group_by(scenario, region, var, year) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(all_of(gcamreport::long_columns))
}

#' get_iron_steel_imports
#'
#' Get iron steel imports
#' @keywords internal industry tmp
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @return iron_steel_imports global variable
#' @importFrom magrittr %>%
#' @export
get_iron_steel_imports <- function(GCAM_version = "v7.0") {
  var <- scenario <- region <- year <- value <- NULL

  iron_steel_imports <<-
    rgcam::getQuery(prj, "regional iron and steel sources") %>%
    dplyr::filter(subsector == "domestic iron and steel") %>%
    dplyr::left_join(filter_variables(get(paste('iron_steel_trade_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")), "iron_steel_clean"), by = c("sector")) %>%
    # filter variables that are in terms of Mt
    dplyr::group_by(scenario, region, var, year) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(all_of(gcamreport::long_columns))
}

#' get_iron_steel_exports
#'
#' Get iron steel production.
#' @keywords internal industry tmp
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @return iron_steel_exports global variable
#' @importFrom magrittr %>%
#' @export
get_iron_steel_exports <- function(GCAM_version = "v7.0") {
  var <- scenario <- region <- year <- value <- NULL

  iron_steel_exports <<-
    rgcam::getQuery(prj, "traded iron and steel") %>%
    dplyr::left_join(filter_variables(get(paste('iron_steel_trade_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")), "iron_steel_clean"), by = c("sector")) %>%
    # extract region
    dplyr::mutate(region = stringr::str_replace_all(subsector, " traded iron and steel", "")) %>%
    dplyr::filter(region %in% desired_regions) %>%
    # filter variables that are in terms of Mt
    dplyr::group_by(scenario, region, var, year) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(all_of(gcamreport::long_columns))
}

#' get_iron_steel_clean
#'
#' Get iron steel imports & exports
#' @keywords internal industry
#' @return iron_steel_clean global variable
#' @importFrom magrittr %>%
#' @export
get_iron_steel_clean <- function() {
  iron_steel_clean <<- dplyr::bind_rows(
    iron_steel_imports,
    iron_steel_exports
  )
}


# Prices
# ==============================================================================================
#' get_ag_prices_wld_tmp
#'
#' Get ag price index.
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @keywords internal ag tmp
#' @return ag_prices_wld global variable
#' @importFrom magrittr %>%
#' @export
get_ag_prices_wld_tmp <- function(GCAM_version = "v7.0") {
  var <- scenario <- sector <- year <- value <- NULL

  ag_prices_wld <<-
    rgcam::getQuery(prj, "prices by sector") %>%
    dplyr::left_join(filter_variables(get(paste('ag_prices_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")), "ag_prices_wld"), by = c("sector")) %>%
    dplyr::filter(!is.na(var)) %>%
    dplyr::group_by(scenario, sector, year) %>%
    dplyr::summarise(value = mean(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(region = "World")
}

#' get_ag_prices
#'
#' Calculate average mean for ag global index
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @keywords internal ag
#' @return ag_prices_clean global variable
#' @importFrom magrittr %>%
#' @export
get_ag_prices <- function(GCAM_version = "v7.0") {
  var <- scenario <- region <- sector <- value <- unit_conv <- year <- NULL

  ag_prices_clean <<-
    rgcam::getQuery(prj, "prices by sector") %>%
    dplyr::bind_rows(ag_prices_wld) %>%
    dplyr::left_join(filter_variables(get(paste('ag_prices_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")), "ag_prices_clean"), by = c("sector")) %>%
    dplyr::filter(!is.na(var)) %>%
    dplyr::group_by(scenario, region, sector) %>%
    dplyr::mutate(value = value * unit_conv / value[year == 2015]) %>%
    dplyr::ungroup() %>%
    # do the mean by variable
    dplyr::group_by(scenario, region, var, year, ) %>%
    dplyr::summarise(value = mean(value)) %>%
    dplyr::ungroup() %>%
    # rearrange dataset
    dplyr::select(all_of(gcamreport::long_columns))
}


# carbon price
# sectoral CO2 prices are the same for all scenarios except for d_rap
# CO2 prices are global except for d_delfrag

# calculate co2 price for all scenarios except for d_rap and d_delfrag

#' get_price_var_tmp
#'
#' Get price variables to compute carbon price.
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @keywords internal internal tmp process
#' @return price_var global variable
#' @importFrom magrittr %>%
#' @export
get_price_var_tmp <- function(GCAM_version = "v7.0") {
  price_var <<-
    unique(filter_variables(get(paste('co2_market_frag_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")), "price_var")$var)
}



#' filter_data_regions
#'
#' filter the desired regions of some data with "regions" column
#' @keywords internal internal tmp process
#' @return data containing only the desired regions
#' @importFrom magrittr %>%
#' @export
filter_data_regions <- function(data) {
  region <- NULL

  if (!(identical(desired_regions, "All"))) {
    data <- data %>%
      dplyr::filter(region %in% desired_regions)
  }

  return(data)
}


#' get_regions_tmp
#'
#' Get regions to compute carbon price.
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @keywords internal internal tmp process
#' @return regions global variable
#' @importFrom magrittr %>%
#' @export
get_regions_tmp <- function(GCAM_version = "v7.0") {
  CO2_market_filteredReg <- filter_data_regions(get(paste('CO2_market',GCAM_version,sep='_'), envir = asNamespace("gcamreport")))
  regions.global <<-
    unique(CO2_market_filteredReg$region)
}


#' get_regions_weight_tmp
#'
#' Get regions weights to compute carbon price.
#' @keywords internal internal tmp process
#' @return region_weight global variable
#' @importFrom magrittr %>%
#' @export
get_regions_weight_tmp <- function() {
  var <- scenario <- year <- value <- NULL

  # for scenarios w/ different regional carbon prices, weigh regional price by final energy to get global CO2 price
  region_weight <<-
    fe_sector_clean %>%
    dplyr::filter(var == "Final Energy") %>%
    dplyr::group_by(scenario, year) %>%
    dplyr::mutate(weight = value / sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-value, -var)
}


#' get_co2_price_global_tmp
#'
#' Get global co2 price.
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @keywords internal co2 tmp
#' @return co2_price_global & regions global variable
#' @importFrom magrittr %>%
#' @export
get_co2_price_global_tmp <- function(GCAM_version = "v7.0") {
  market <- value <- co2_price_global_pre <- regions <- NULL

  co2_price_global_pre <-
    rgcam::getQuery(prj, "CO2 prices") %>%
    dplyr::filter(market %in% c("WorldCO2","globalCO2","GlobalCO2","worldCO2"))

  if (nrow(co2_price_global_pre) > 1) {
    co2_price_global <<-
      tibble::as_tibble(co2_price_global_pre) %>%
      dplyr::mutate(value = value /
               get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_C_CO2']] *
               get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_90USD_10USD']]
             ) %>%
      dplyr::mutate(market = gsub("global|Global|world|World", "", market)) %>%
      dplyr::left_join(filter_variables(get(paste('co2_market_frag_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")), "co2_price_global"), by = "market", multiple = "all") %>%
      tidyr::expand_grid(tibble::tibble(region = unique(fe_sector_clean$region))) %>%
      dplyr::select(all_of(gcamreport::long_columns))
  } else {
    co2_price_global <<- NULL
  }
}


#' get_co2_price_share
#'
#' Get co2 price between CO2 and CO2_ETS. If only one CO2 type present, share = 1;
#' otherwise, each type has the share corresponding to the last historical year
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @keywords internal co2 tmp
#' @return co2_price_share_byreg and co2_price_share_bysec global variables
#' @importFrom magrittr %>%
#' @export
get_co2_price_share <- function(GCAM_version = "v7.0") {
  var <- year <- region <- value <- . <- sector <-
    CO2 <- scenario <- share_CO2_ETS <- NULL

  co2_price_share_byreg <<- co2_clean %>%
    dplyr::filter(
      var == "Emissions|CO2|Energy and Industrial Processes",
      year == get(paste('last_historical_year',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))
    ) %>%
    # compute the World CO2 emissions
    dplyr::group_by(scenario, var, year) %>%
    dplyr::mutate(global_value = sum(value)) %>%
    dplyr::ungroup() %>%
    # compute shares
    dplyr::mutate(share_CO2 = value / global_value) %>%
    dplyr::select(scenario, region, year, share_CO2)

  co2_price_share_bysec <<- co2_clean %>%
    dplyr::filter(year == get(paste('last_historical_year',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))) %>%
    # dplyr::select only reported sectors and do a right join, so that all sectors are present,
    # even if the value is NA
    right_join(expand.grid(
      var = c(
        "Emissions|CO2|Energy and Industrial Processes",
        "Emissions|CO2|Energy|Demand|Industry",
        "Emissions|CO2|Energy|Demand|Transportation",
        "Emissions|CO2|Energy|Demand|Residential and Commercial",
        "Emissions|CO2|Energy|Supply"
      ),
      region = unique(co2_clean$region),
      scenario = unique(co2_clean$scenario),
      year = get(paste('last_historical_year',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))
    )) %>%
    dplyr::mutate(value = dplyr::if_else(is.na(value), 0, value)) %>%
    # compute the share
    dplyr::mutate(sector = sub(".*\\|([^|]+)$", "\\1", var)) %>%
    dplyr::select(-var) %>%
    dplyr::distinct(.) %>%
    # compute the World CO2 emissions
    dplyr::group_by(scenario, sector, year) %>%
    dplyr::mutate(global_value = sum(value)) %>%
    dplyr::ungroup() %>%
    # compute shares
    dplyr::mutate(share_CO2 = value / global_value) %>%
    dplyr::select(scenario, region, year, sector, share_CO2)

}



#' get_co2_price_fragmented_tmp
#'
#' Get fragmented co2 price.
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @keywords internal co2 tmp
#' @return co2_price_fragmented global variable
#' @importFrom magrittr %>%
#' @export
get_co2_price_fragmented_tmp <- function(GCAM_version = "v7.0") {
  market <- Units <- regions <- year <- value <- market_adj <- scenario <-
    region <- CO2 <- sector <- var <- NULL

  co2_price_fragmented_pre <<-
    rgcam::getQuery(prj, "CO2 prices") %>%
    dplyr::filter(!grepl("LUC", market)) %>%
    dplyr::filter(!grepl("global|Global|world|World", market)) %>%
    dplyr::filter(Units == "1990$/tC")

  if (nrow(co2_price_fragmented_pre) > 1) {
    CO2_market_filteredReg <- filter_data_regions(get(paste('CO2_market',GCAM_version,sep='_'), envir = asNamespace("gcamreport")))

    co2_price_fragmented <<-
      co2_price_fragmented_pre %>%
      dplyr::left_join(CO2_market_filteredReg, by = c("market"), multiple = "all") %>%
      dplyr::filter(stats::complete.cases(.)) %>%
      dplyr::mutate(value = value /
               get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_C_CO2']] *
               get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_90USD_10USD']]
             ) %>%
      dplyr::mutate(market_adj = "CO2") %>%
      # consider the value sum of by market
      dplyr::group_by(Units, scenario, year, market, region) %>%
      dplyr::mutate(value = sum(value)) %>%
      dplyr::ungroup() %>%
      dplyr::select(-market) %>%
      tidyr::pivot_wider(names_from = "market_adj", values_from = "value") %>%
      dplyr::left_join(
        co2_price_share_bysec %>%
          dplyr::select(-year),
        by = c("scenario", "region")
      ) %>%
      dplyr::mutate(value = CO2) %>%
      dplyr::select(Units, scenario, year, region, value, CO2, sector) %>%
      dplyr::left_join(filter_variables(get(paste('co2_market_frag_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")), "co2_price_fragmented"), by = "sector", multiple = "all") %>%
      dplyr::filter(stats::complete.cases(.)) %>%
      tidyr::complete(tidyr::nesting(scenario, var, year, market, Units), region = regions.global, fill = list(value = 0)) %>%
      dplyr::select(all_of(gcamreport::long_columns))
  } else {
    co2_price_fragmented <<- NULL
  }
}


#' get_co2_price
#'
#' Get co2 price.
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @keywords internal co2
#' @return co2_price_clean global variable
#' @importFrom magrittr %>%
#' @export
get_co2_price <- function(GCAM_version = "v7.0") {
  co2_price_clean_pre <- region <- var <- year <- NULL

  co2_price_clean_pre <-
    dplyr::bind_rows(co2_price_global, co2_price_fragmented)

  if (nrow(co2_price_clean_pre) < 1) {
    co2_price_clean <<-
      tibble::tibble(var = unique(filter_variables(get(paste('co2_market_frag_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")), "co2_price_clean")$var)) %>%
      tidyr::expand_grid(tibble::tibble(scenario = unique(fe_sector_clean$scenario))) %>%
      tidyr::expand_grid(tibble::tibble(year = unique(fe_sector_clean$year))) %>%
      tidyr::expand_grid(tibble::tibble(region = c(unique(fe_sector_clean$region), "Global"))) %>%
      dplyr::mutate(value = 0) %>%
      dplyr::select(all_of(gcamreport::long_columns))
  } else {
    co2_price_regional <- co2_price_clean_pre %>%
      tidyr::complete(tidyr::nesting(region, var, year), scenario = unique(fe_sector_clean$scenario), fill = list(value = 0))

    # compute Global value using the emission weights
    co2_price_world <- co2_price_regional %>%
      dplyr::left_join(co2_price_share_bysec %>%
                         dplyr::left_join(filter_variables(gcamreport::co2_market_frag_map, "co2_price_fragmented"),
                                          by = "sector", multiple = "all") %>%
                         dplyr::select(-sector,-market,-year),
                       by = c('region','scenario','var')) %>%
      dplyr::mutate(weighted_value = value * share_CO2) %>%
      dplyr::group_by(scenario, var, year) %>%
      dplyr::summarise(value = sum(weighted_value)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(region = "Global")

    co2_price_clean <<- co2_price_regional %>%
      rbind(co2_price_world) %>%
      dplyr::select(all_of(gcamreport::long_columns))
  }
}


#' get_gov_revenue_sector
#'
#' Get overall carbon revenue.
#' @keywords internal revenue
#' @return gov_revenue_sector global variable
#' @importFrom magrittr %>%
#' @export
get_gov_revenue_sector <- function() {
  var <- sector <- value <- emiss <- NULL

  gov_revenue_sector <<-
    co2_clean %>%
    dplyr::mutate(
      sector = ifelse(var == "Emissions|CO2|Energy|Demand|Industry", "Carbon|Demand|Industry", NA),
      sector = ifelse(var == "Emissions|CO2|Energy|Demand|Residential and Commercial", "Carbon|Demand|Buildings", sector),
      sector = ifelse(var == "Emissions|CO2|Energy|Demand|Transportation", "Carbon|Demand|Transport", sector),
      sector = ifelse(var == "Emissions|CO2|Energy|Supply|Electricity", "Carbon|Supply", sector),
      emiss = value
    ) %>%
    dplyr::select(-var, -value) %>%
    dplyr::filter(!is.na(sector)) %>%
    dplyr::left_join(
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
      var = paste0("Revenue|Government|Tax|", sector)
    )
}

#' get_gov_revenue
#'
#' Get overall carbon revenue.
#' @keywords internal revenue
#' @return gov_revenue_all global variable
#' @importFrom magrittr %>%
#' @export
get_gov_revenue <- function() {
  scenario <- region <- year <- value <- NULL

  gov_revenue_clean <<-
    gov_revenue_sector %>%
    dplyr::bind_rows(gov_revenue_sector %>%
      dplyr::group_by(scenario, region, year) %>%
      dplyr::summarise(value = sum(value, na.rm = T)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(var = "Revenue|Government|Tax|Carbon", )) %>%
    dplyr::mutate(value = value / 1000) %>%
    dplyr::select(all_of(gcamreport::long_columns))
}


#' get_prices_subsector
#'
#' Get energy prices - primary, secondary, and final
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @keywords internal prices
#' @return prices_subsector global variable
#' @importFrom magrittr %>%
#' @export
get_prices_subsector <- function(GCAM_version = "v7.0") {
  Units <- subsector <- var <- PrimaryFuelCO2Coef.name <- PrimaryFuelCO2Coef <- NULL

  prices_subsector <<-
    rgcam::getQuery(prj, "prices by sector") %>%
    dplyr::select(-Units) %>%
    dplyr::left_join(filter_variables(get(paste('energy_prices_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")), "prices_subsector") %>%
      dplyr::filter(is.na(subsector)) %>%
      unique(), by = c("sector"), relationship = "many-to-many") %>%
    dplyr::bind_rows(rgcam::getQuery(prj, "costs by subsector") %>%
      dplyr::left_join(
        filter_variables(get(paste('energy_prices_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")), "prices_subsector") %>%
          unique(),
        by = c("sector", "subsector"), relationship = "many-to-many"
      )) %>%
    dplyr::filter(!is.na(var)) %>%
    # read in carbon content in kg C per GJ -> convert to tC per GJ
    dplyr::left_join(
      get(paste('carbon_content',GCAM_version,sep='_'), envir = asNamespace("gcamreport")) %>%
        dplyr::filter(grepl("biomass", PrimaryFuelCO2Coef.name)),
      by = c("region", "sector" = "PrimaryFuelCO2Coef.name")
    ) %>%
    dplyr::mutate(PrimaryFuelCO2Coef = PrimaryFuelCO2Coef / 1000) %>%
    tidyr::replace_na(list(PrimaryFuelCO2Coef = 0))
}


#' get_energy_price_fragmented
#'
#' Get energy prices fragmented, join by region since price is different.
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @keywords internal prices process
#' @return energy_price_fragmented global variable
#' @importFrom magrittr %>%
#' @export
get_energy_price_fragmented <- function(GCAM_version = "v7.0") {
  var <- market <- scenario <- region <- year <-
    value <- PrimaryFuelCO2Coef <- price_C <- unit_conv <- NULL

  CO2_market_filteredReg <- filter_data_regions(get(paste('CO2_market',GCAM_version,sep='_'), envir = asNamespace("gcamreport")))

  energy_price_fragmented <<-
    prices_subsector %>%
    dplyr::filter(!is.na(var)) %>%
    dplyr::left_join(rgcam::getQuery(prj, "CO2 prices") %>%
      dplyr::filter(!grepl("LUC", market)) %>%
      dplyr::left_join(CO2_market_filteredReg, by = c("market"), relationship = "many-to-many") %>%
      dplyr::select(scenario, region, year, price_C = value), by = c("scenario", "region", "year"),
      relationship = "many-to-many") %>%
    tidyr::replace_na(list(price_C = 0)) %>%
    # remove carbon price (subsidy) 1990$/tC from biomass 1975$/GJ
    dplyr::mutate(
      price_C = PrimaryFuelCO2Coef * price_C *
        get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_90USD_10USD']],
      value = value * unit_conv *
        get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_75USD_10USD']] + price_C
    ) %>%
    dplyr::select(all_of(gcamreport::long_columns))
}

#' get_total_revenue
#'
#' Compute total revenue: total production * global price.
#' @keywords internal revenue
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @return total_revenue global variable
#' @importFrom magrittr %>%
#' @export
get_total_revenue <- function(GCAM_version = "v7.0") {
  resource <- scenario <- year <- value <- sector <- resource_price <-
    total_production <- NULL

  total_revenue <<-
    rgcam::getQuery(prj, "resource production") %>%
    dplyr::filter(resource %in% c("coal", "crude oil", "natural gas")) %>%
    dplyr::group_by(scenario, resource, year) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(total_production = value) %>%
    dplyr::left_join(
      rgcam::getQuery(prj, "prices by sector") %>%
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
}

#' get_regional_emission
#'
#' Compute regional nonCO2 emission: regional production * nonCO2 coef.
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @keywords internal nonco2
#' @return regional_emission global variable
#' @importFrom magrittr %>%
#' @export
get_regional_emission <- function(GCAM_version = "v7.0") {
  year <- Non.CO2 <- emiss.coef <- CH4 <- N2O <- CH4.coef <- N2O.coef <-
    region <- resource <- value <- regional_production <- NULL

  regional_emission <<- suppressWarnings(
    get(paste('nonCO2_content',GCAM_version,sep='_'), envir = asNamespace("gcamreport")) %>%
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
      dplyr::left_join(rgcam::getQuery(prj, "resource production") %>%
        dplyr::filter(resource %in% c("coal", "crude oil", "natural gas")) %>%
        dplyr::rename(regional_production = value), by = c("region", "resource")) %>%
      dplyr::mutate(
        regional_CH4emission = regional_production * CH4.coef *
          get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['GJ_to_EJ']],
        regional_N2Oemission = regional_production * N2O.coef *
          get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['GJ_to_EJ']]
      )
  )
}


#' get_energy_price_tmp
#'
#' Bind regional oil, gas, coal prices with other energy prices
#' @keywords internal price tmp
#' @return energy_price global variable
#' @importFrom magrittr %>%
#' @export
get_energy_price_tmp <- function() {
  energy_price <<-
    energy_price_fragmented %>%
    dplyr::select(all_of(gcamreport::long_columns))
}


#' get_energy_price
#'
#' Compute final energy price
#' @keywords internal price process
#' @return energy_price_clean global variable
#' @importFrom magrittr %>%
#' @export
get_energy_price <- function() {
  var <- scenario <- region <- value <- year <- NULL

  energy_price_clean <<-
    energy_price %>%
    dplyr::filter(grepl("Residential\\|Electricity", var) |
      grepl("Residential\\|Gas", var) |
      grepl("Primary Energy\\|Coal", var) |
      grepl("Primary Energy\\|Biomass", var) |
      grepl("Primary Energy\\|Gas", var) |
      grepl("Primary Energy\\|Oil", var) |
      grepl("Secondary Energy\\|Electricity", var)) %>%
    dplyr::mutate(var = paste(var, "Index", sep = "|")) %>%
    dplyr::group_by(scenario, region, var) %>%
    dplyr::mutate(value = value / value[year == 2015]) %>%
    dplyr::ungroup() %>%
    dplyr::select(all_of(gcamreport::long_columns)) %>%
    dplyr::bind_rows(energy_price)

  # sum by energy price var
  energy_price_clean <<-
    energy_price_clean %>%
    dplyr::group_by(scenario, region, var, year) %>%
    dplyr::mutate(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    unique()

  # average mean for global primary energy price
  energy_price_clean <<-
    energy_price_clean %>%
    dplyr::group_by(scenario, var, year) %>%
    dplyr::summarise(value = mean(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(region = "World") %>%
    dplyr::bind_rows(energy_price_clean)
}

# Capacity and investment
# ==============================================================================================
# electricity capacity
## first check global existing capacity from IEA, calculate cf for existing capacity

#' get_cf_iea_tmp
#'
#' Calculate cf for existing capacity checking global existing capacity from IEA.
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @keywords internal capacity process tmp
#' @return cf_iea global variable
#' @importFrom magrittr %>%
#' @export
get_cf_iea_tmp <- function(GCAM_version = "v7.0") {
  year <- scenario <- var <- value <- period <- variable <- EJ <- cf <- technology <- NULL

  cf_rgn_filteredReg <- filter_data_regions(get(paste('cf_rgn',GCAM_version,sep='_'), envir = asNamespace("gcamreport")))

  cf_iea <<-
    elec_gen_tech_clean %>%
    dplyr::filter(year == 2020, scenario == unique(elec_gen_tech_clean$scenario)[1]) %>%
    dplyr::group_by(var) %>%
    dplyr::summarise(EJ = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(
      get(paste('iea_capacity',GCAM_version,sep='_'), envir = asNamespace("gcamreport")) %>%
        dplyr::filter(period == 2020, scenario == "Current Policies Scenario") %>%
        dplyr::mutate(
          variable = gsub("Capacity\\|Electricity\\|CSP", "Capacity\\|Electricity\\|Solar\\|CSP", variable),
          variable = gsub("Capacity\\|Electricity\\|Biomass", "Capacity\\|Electricity\\|Biomass\\|w/o CCS", variable),
          variable = gsub("Capacity\\|Electricity\\|Coal", "Capacity\\|Electricity\\|Coal\\|w/o CCS", variable),
          variable = gsub("Capacity\\|Electricity\\|Gas", "Capacity\\|Electricity\\|Gas\\|w/o CCS", variable),
          variable = gsub("Capacity\\|Electricity\\|Oil", "Capacity\\|Electricity\\|Oil\\|w/o CCS", variable),
          variable = gsub("Capacity", "Secondary Energy", variable)
        ),
      by = c("var" = "variable")
    ) %>%
    dplyr::mutate(
      cf = EJ / (value *
                   get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['hr_per_yr']] *
                   get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['EJ_to_GWh']]),
      cf = replace(cf, cf > 1, 0.99)
    ) %>%
    dplyr::filter(!is.na(cf), !var %in% c("Secondary Energy|Electricity", "Secondary Energy|Electricity|Non-Biomass Renewables")) %>%
    dplyr::left_join(filter_variables(get(paste('capacity_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")), "cf_iea"), by = "var", multiple = "all") %>%
    dplyr::select(technology, cf) %>%
    dplyr::mutate(region = "USA", vintage = 2020) %>%
    tidyr::complete(tidyr::nesting(technology, cf),
      vintage = c(1990, seq(2005, 2020, by = 5)),
      region = unique(cf_rgn_filteredReg$region)
    )
}

#' get_elec_cf_tmp
#'
#' Calculate future capacity using GCAM
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @keywords internal capacity process tmp
#' @return elec_cf global variable
#' @importFrom magrittr %>%
#' @export
get_elec_cf_tmp <- function(GCAM_version = "v7.0") {
  technology <- X2100 <- cf <- region <- stub.technology <- year <-
    capacity.factor <- cf.rgn <- vintage <- NULL

  cf_rgn_filteredReg <- filter_data_regions(get(paste('cf_rgn',GCAM_version,sep='_'), envir = asNamespace("gcamreport")))
  cf_iea_filteredReg <- filter_data_regions(cf_iea)

  elec_cf <-
    get(paste('cf_gcam',GCAM_version,sep='_'), envir = asNamespace("gcamreport")) %>%
    dplyr::select(technology, cf = X2100) %>%
    dplyr::mutate(region = "USA", vintage = 2025) %>%
    tidyr::complete(tidyr::nesting(technology, cf),
      vintage = seq(2025, 2100, by = 5),
      region = unique(cf_rgn_filteredReg$region)
    ) %>%
    # first, replace regional cf for wind and solar
    dplyr::left_join(
      cf_rgn_filteredReg %>%
        dplyr::select(region, technology = stub.technology, vintage = year, cf.rgn = capacity.factor),
      by = c("technology", "vintage", "region")
    ) %>%
    dplyr::mutate(cf = replace(cf, !is.na(cf.rgn), cf.rgn[!is.na(cf.rgn)])) %>%
    # second, use iea capacity consistent cf for existing vintage
    dplyr::bind_rows(cf_iea_filteredReg) %>%
    tidyr::complete(tidyr::nesting(technology, region), vintage = c(1990, seq(2005, 2100, by = 5))) %>%
    dplyr::group_by(technology, region) %>%
    dplyr::mutate(cf = approx_fun(vintage, cf, rule = 2)) %>%
    dplyr::ungroup() %>%
    dplyr::filter(!technology %in% c("existing coal", "add CCS retrofit"))

  elec_cf <<- filter_data_regions(elec_cf)
}

#' get_elec_capacity_tot
#'
#' Calculate electricity total capacity
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @keywords internal capacity process
#' @return elec_capacity_tot_clean global variable
#' @importFrom magrittr %>%
#' @export
get_elec_capacity_tot <- function(GCAM_version = "v7.0") {
  output <- technology <- vintage <- var <- unit_conv <-
    scenario <- region <- year <- value <- gw <- NULL

  elec_capacity_tot_clean <<- suppressWarnings(
    rgcam::getQuery(prj, "elec gen by gen tech and cooling tech and vintage") %>%
      dplyr::filter(!output %in% c("electricity", "elect_td_bld")) %>%
      tidyr::separate(technology, into = c("technology", "vintage"), sep = ",") %>%
      dplyr::mutate(
        vintage = as.integer(sub("year=", "", vintage)),
        output = gsub("elec_", "", output)
      ) %>%
      dplyr::group_by(scenario, region, technology = output, vintage, year) %>%
      dplyr::summarise(value = sum(value, na.rm = T)) %>%
      dplyr::ungroup() %>%
      dplyr::bind_rows(rgcam::getQuery(prj, "elec gen by gen tech and cooling tech and vintage") %>%
        dplyr::filter(output %in% c("electricity", "elect_td_bld")) %>%
        tidyr::separate(technology, into = c("technology", "vintage"), sep = ",") %>%
        dplyr::mutate(vintage = as.integer(sub("year=", "", vintage))) %>%
        dplyr::group_by(scenario, region, technology, vintage, year) %>%
        dplyr::summarise(value = sum(value, na.rm = T)) %>%
        dplyr::ungroup()) %>%
      dplyr::left_join(elec_cf, by = c("region", "technology", "vintage")) %>%
      dplyr::mutate(EJ = value) %>%
      conv_EJ_GW() %>%
      dplyr::group_by(scenario, region, technology, year) %>%
      dplyr::summarise(value = sum(gw, na.rm = T)) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(filter_variables(get(paste('capacity_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")), "elec_capacity_tot_clean") %>% dplyr::select(-output), by = c("technology"), multiple = "all") %>%
      dplyr::filter(!is.na(var)) %>%
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
      dplyr::select(all_of(gcamreport::long_columns))
  )
}

#' get_elec_capacity_add_tmp
#'
#' Calculate added total capacity
#' @keywords internal capacity process tmp
#' @return elec_capacity_add global variable
#' @importFrom magrittr %>%
#' @export
get_elec_capacity_add_tmp <- function() {
  output <- technology <- vintage <- scenario <-
    region <- year <- value <- gw <- EJ <- NULL

  elec_capacity_add <<- suppressWarnings(
    rgcam::getQuery(prj, "elec gen by gen tech and cooling tech and vintage") %>%
      dplyr::filter(!output %in% c("electricity", "elect_td_bld")) %>%
      tidyr::separate(technology, into = c("technology", "vintage"), sep = ",") %>%
      dplyr::mutate(
        vintage = as.integer(sub("year=", "", vintage)),
        output = gsub("elec_", "", output)
      ) %>%
      dplyr::group_by(scenario, region, technology = output, vintage, year) %>%
      dplyr::summarise(value = sum(value, na.rm = T)) %>%
      dplyr::ungroup() %>%
      dplyr::bind_rows(rgcam::getQuery(prj, "elec gen by gen tech and cooling tech and vintage") %>%
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
      # use GCAM cf for capacity additions
      dplyr::left_join(elec_cf,
        by = c("region", "technology", "year" = "vintage")
      ) %>%
      # use average annual additions
      dplyr::mutate(EJ = value / 5) %>%
      conv_EJ_GW() %>%
      dplyr::group_by(scenario, region, technology, year) %>% #
      dplyr::summarise(GW = sum(gw, na.rm = T), EJ = sum(EJ, na.rm = T)) %>%
      dplyr::ungroup()
  )
}

#' get_elec_capacity_add
#'
#' Calculate added total capacity
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @keywords internal capacity process
#' @return elec_capacity_add_clean global variable
#' @importFrom magrittr %>%
#' @export
get_elec_capacity_add <- function(GCAM_version = "v7.0") {
  output <- EJ <- value <- var <- GW <- unit_conv <- scenario <- vintage <-
    region <- year <- gw <- output <- technology <- scenario <- NULL

  # check calculations for this
  elec_capacity_add_clean <<-
    elec_capacity_add %>%
    dplyr::left_join(get(paste('capacity_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")) %>% dplyr::select(-output), by = c("technology"), multiple = "all") %>%
    dplyr::filter(!var %in% c(
      "Secondary Energy|Electricity|Hydro",
      "Secondary Energy|Electricity|Storage Capacity"
    )) %>%
    dplyr::mutate(
      value = GW * unit_conv,
      var = sub("Secondary Energy", "Capacity Additions", var)
    ) %>%
    dplyr::bind_rows(elec_capacity_add %>%
      dplyr::left_join(get(paste('capacity_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")) %>% dplyr::select(-output), by = c("technology"), multiple = "all") %>%
      dplyr::filter(var == "Secondary Energy|Electricity|Storage Capacity") %>%
      dplyr::mutate(
        value = GW * 8760, # multiply by # of hours in a year
        var = sub("Secondary Energy", "Capacity Additions", var)
      )) %>%
    filter_variables(variable = "elec_capacity_add_clean") %>%
    dplyr::group_by(scenario, region, var, year) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    tidyr::complete(tidyr::nesting(scenario, region, year),
      var = unique(var),
      fill = list(value = 0)
    ) %>%
    dplyr::select(all_of(gcamreport::long_columns))
}


#' get_elec_capital
#'
#' Calculate electricity capital
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @keywords internal capital process
#' @return elec_capital_clean global variable
#' @importFrom magrittr %>%
#' @export
get_elec_capital <- function(GCAM_version = "v7.0") {
  sector <- subsector <- technology <- year <- capital.overnight <- output <-
    var <- value <- unit_conv <- scenario <- region <- NULL

  cf_rgn_filteredReg <- filter_data_regions(get(paste('cf_rgn',GCAM_version,sep='_'), envir = asNamespace("gcamreport")))

  # Capital costs from GCAM in $1975/kw -> convert to $2010/kw
  elec_capital <-
    get(paste('capital_gcam',GCAM_version,sep='_'), envir = asNamespace("gcamreport")) %>%
    dplyr::mutate(region = "USA", scenario = scenarios.global[1]) %>%
    dplyr::select(-sector) %>%
    tidyr::complete(tidyr::nesting(subsector, technology, year, capital.overnight),
      region = unique(cf_rgn_filteredReg$region), scenario = scenarios.global
    ) %>%
    # gw * 10e6 * $/kw / 10e9 = bill$
    dplyr::mutate(value = capital.overnight *
             get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_75USD_10USD']]) %>%
    dplyr::left_join(filter_variables(get(paste('elec_gen_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")), "elec_capital") %>% dplyr::select(-output), by = c("subsector", "technology"), multiple = "all")

  elec_capital_clean <<-
    filter_data_regions(elec_capital) %>%
    dplyr::filter(!is.na(var), var != "Secondary Energy|Electricity|Electricity Storage") %>%
    dplyr::mutate(
      value = value * unit_conv,
      var = sub("Secondary Energy", "Capital Cost", var)
    ) %>%
    dplyr::group_by(scenario, region, var, year) %>%
    dplyr::summarise(value = mean(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(all_of(gcamreport::long_columns))

  # average mean for global tech capital cost
  elec_capital_clean <<-
    elec_capital_clean %>%
    dplyr::group_by(scenario, var, year) %>%
    dplyr::summarise(value = mean(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(region = "World") %>%
    dplyr::bind_rows(elec_capital_clean)
}

#' get_elec_investment
#'
#' Calculate electricity investment = annual capacity additions * capital costs
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @keywords internal capital process
#' @return elec_investment_clean global variable
#' @importFrom magrittr %>%
#' @export
get_elec_investment <- function(GCAM_version = "v7.0") {
  year <- region <- var <- capital.overnight <- technology <-
    GW <- scenario <- output <- value <- unit_conv <- NULL

  elec_investment_clean <<-
    # Electricity investment = annual capacity additions * capital costs
    elec_capacity_add %>%
    dplyr::left_join(
      get(paste('capital_gcam',GCAM_version,sep='_'), envir = asNamespace("gcamreport")) %>%
        dplyr::mutate(
          capital.overnight = replace(capital.overnight, technology == "wind_storage", capital.overnight[technology == "wind"] * .484),
          capital.overnight = replace(capital.overnight, technology == "CSP_storage", 760 *
                                        get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_19USD_75USD']]),
          capital.overnight = replace(capital.overnight, technology == "PV_storage", capital.overnight[technology == "PV"] * .518)
        ),
      by = c("technology", "year")
    ) %>%
    # gw * 10e6 * $/kw / 10e9 = bill$
    dplyr::mutate(value = GW * capital.overnight / 1000 *
             get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_75USD_10USD']]) %>%
    dplyr::left_join(filter_variables(get(paste('elec_gen_map',GCAM_version,sep='_'), envir = asNamespace("gcamreport")), "elec_investment_clean") %>% dplyr::select(-output), by = c("technology"), multiple = "all") %>%
    dplyr::filter(!is.na(var)) %>%
    dplyr::mutate(
      value = value * unit_conv,
      var = sub("Secondary Energy", "Investment|Energy Supply", var)
    ) %>%
    dplyr::group_by(scenario, region, var, year) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(all_of(gcamreport::long_columns))
}

#' get_transmission_invest
#'
#' Calculate investment electricity transmission and distribution
#' scale 2020 number - average of other model results from Mcollion et al. 2018
#' Need to convert 2015 to 2010 $
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @keywords internal investment process
#' @return transmission_invest_clean global variable
#' @importFrom magrittr %>%
#' @export
get_transmission_invest <- function(GCAM_version = "v7.0") {
  Region <- Variable <- year <- value <- var <- scenario <- share <-
    region <- invest <- rate <- NULL

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

  transmission_invest_clean <<-
    elec_capacity_add_clean %>%
    dplyr::filter(var == "Capacity Additions|Electricity") %>%
    dplyr::group_by(scenario, region) %>%
    dplyr::mutate(rate = value / value[year == 2020]) %>%
    dplyr::left_join(
      transmission_invest2020 %>%
        dplyr::select(scenario, region, invest),
      by = c("scenario", "region")
    ) %>%
    dplyr::mutate(value = rate * invest, var = "Investment|Energy Supply|Electricity|Transmission and Distribution") %>%
    dplyr::select(all_of(gcamreport::long_columns))
}


#' get_CCS_invest
#'
#' Calculate CSS investment
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @keywords internal investment process
#' @return CCS_invest_clean global variable
#' @importFrom magrittr %>%
#' @export
get_CCS_invest <- function(GCAM_version = "v7.0") {
  Region <- Variable <- year <- value <- var <- scenario <- share <- region <-
    invest <- rate <- NULL

  # use last available year if 2040 is not present in the data
  yy <- ifelse(max(unique(co2_sequestration_clean$year)) >= 2040, 2040, max(unique(co2_sequestration_clean$year)))

  CCS2040 <-
    get(paste('investment',GCAM_version,sep='_'), envir = asNamespace("gcamreport")) %>%
    dplyr::filter(Region == "World", Variable == "CCS", year == yy) %>%
    dplyr::mutate(value = value * get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_15USD_10USD']]) %>%
    dplyr::summarise(value = mean(value, na.rm = T)) %>%
    unlist()

  CCS_invest2040 <-
    co2_sequestration_clean %>%
    dplyr::filter(var == "Carbon Sequestration|CCS", year == yy) %>%
    dplyr::group_by(scenario) %>%
    dplyr::mutate(
      share = value / sum(value, na.rm = T),
      invest = share * CCS2040
    )

  CCS_invest_clean <<-
    co2_sequestration_clean %>%
    dplyr::filter(var == "Carbon Sequestration|CCS") %>%
    dplyr::group_by(scenario, region) %>%
    dplyr::mutate(rate = value / value[year == yy]) %>%
    dplyr::left_join(
      CCS_invest2040 %>%
        dplyr::select(scenario, region, invest),
      by = c("scenario", "region")
    ) %>%
    dplyr::mutate(value = rate * invest, var = "Investment|Energy Supply|CO2 Transport and Storage") %>%
    dplyr::select(all_of(gcamreport::long_columns))
}

#' get_resource_investment
#'
#' Calculate investment of resource production
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @keywords internal investment process
#' @return resource_investment_clean global variable
#' @importFrom magrittr %>%
#' @export
get_resource_investment <- function(GCAM_version = "v7.0") {
  resource <- technology <- vintage <- year <- scenario <- region <- rate <-
    value <- Region <- Variable <- fuel <- production <- share <- invest <- NULL

  # Investment of resource production
  resource_addition <- suppressWarnings(
    rgcam::getQuery(prj, "resource production by tech and vintage") %>%
      dplyr::filter(resource %in% c("coal", "natural gas", "crude oil", "unconventional oil")) %>%
      tidyr::separate(technology, into = c("technology", "vintage"), sep = ",") %>%
      dplyr::mutate(vintage = as.integer(sub("year=", "", vintage))) %>%
      dplyr::filter(year > 2010) %>%
      dplyr::mutate(
        resource = sub("crude oil", "oil", resource),
        resource = sub("unconventional oil", "oil", resource)
      ) %>%
      dplyr::group_by(scenario, resource, region, year) %>%
      dplyr::summarise(production = sum(value, na.rm = T)) %>%
      dplyr::ungroup()
  )

  # scale 2015 number - average of other model results from Mcollion et al. 2018
  extraction2015 <-
    get(paste('investment',GCAM_version,sep='_'), envir = asNamespace("gcamreport")) %>%
    dplyr::filter(Region == "World", Variable == "Extraction and Conversion - Fossil Fuels", year == 2015) %>%
    dplyr::mutate(value = value * get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_15USD_10USD']]) %>%
    dplyr::summarise(value = mean(value, na.rm = T)) %>%
    unlist()

  extraction2020 <-
    get(paste('investment',GCAM_version,sep='_'), envir = asNamespace("gcamreport")) %>%
    dplyr::filter(Region == "World", Variable == "Extraction and Conversion - Fossil Fuels", year == 2020) %>%
    dplyr::mutate(value = value * get(paste('convert',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))[['conv_15USD_10USD']]) %>%
    dplyr::summarise(value = mean(value, na.rm = T)) %>%
    unlist()

  resource_investment2015 <-
    resource_addition %>%
    dplyr::filter(year == 2015) %>%
    dplyr::left_join(
      rgcam::getQuery(prj, "regional primary energy prices") %>%
        dplyr::mutate(fuel = sub("regional ", "", fuel)),
      by = c("scenario", "region", "year", "resource" = "fuel")
    ) %>%
    dplyr::mutate(value = production * value) %>%
    dplyr::group_by(scenario, resource, region) %>%
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
    dplyr::filter(year == 2020) %>%
    dplyr::left_join(
      rgcam::getQuery(prj, "regional primary energy prices") %>%
        dplyr::mutate(fuel = sub("regional ", "", fuel)),
      by = c("scenario", "region", "year", "resource" = "fuel")
    ) %>%
    dplyr::mutate(value = production * value) %>%
    dplyr::group_by(scenario, resource, region) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(scenario) %>%
    dplyr::mutate(
      share = value / sum(value, na.rm = T),
      invest = share * extraction2020
    ) %>%
    dplyr::ungroup()

  reg <- dplyr::if_else(desired_regions == "All" | "China" %in% desired_regions, "China", desired_regions[1])[1]

  resource_investment <-
    resource_addition %>%
    dplyr::filter(year != 2020) %>%
    dplyr::group_by(scenario, resource) %>%
    dplyr::mutate(rate = production / production[year == 2015 & region == reg]) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(
      resource_investment2015 %>%
        dplyr::filter(region == reg) %>%
        dplyr::select(scenario, resource, invest),
      by = c("scenario", "resource")
    ) %>%
    dplyr::bind_rows(resource_addition %>%
      dplyr::filter(year == 2020) %>%
      dplyr::group_by(scenario, resource) %>%
      dplyr::mutate(rate = production / production[region == reg]) %>%
      dplyr::ungroup() %>%
      dplyr::left_join(
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
      var = paste0("Investment|Energy Supply|Extraction|", resource)
    ) %>%
    dplyr::select(all_of(gcamreport::long_columns))

  resource_investment_clean <<-
    resource_investment %>%
    dplyr::group_by(scenario, region, year) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(var = "Investment|Energy Supply|Extraction|Fossil") %>%
    dplyr::select(all_of(gcamreport::long_columns)) %>%
    dplyr::bind_rows(resource_investment) # %>% dplyr::group_by(scenario, var, year) %>% dplyr::summarise(value = sum(value)) %>% tidyr::spread(year, value)
}

#########################################################################
#                        BIND TO TEMPLATE FUNCTIONS                     #
#########################################################################
#' do_bind_results
#'
#' Bind and save results
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @keywords internal process
#' @return Save results in an output file.
#' @importFrom magrittr %>%
#' @export
do_bind_results <- function(GCAM_version = "v7.0") {
  region <- var <- scenario <- year <- value <- . <- na.omit <- Region <- Variable <- NULL

  vars <- variables.global[variables.global$required == TRUE, "name"]
  GCAM_DATA <-
    dplyr::bind_rows(lapply(vars, function(x) get(x))) %>%
    dplyr::mutate(
      region = gsub("Global", "World", region),
      region = gsub("global", "World", region)
    )

  # Calculate global total
  GCAM_DATA_WORLD <-
    GCAM_DATA %>%
    dplyr::filter(
      region != "World", # excl. Temperature|Forcing|Concentration
      # excl. price and costs variables - already calculated global average
      !grepl("Price|Capital Cost", var)
    ) %>%
    dplyr::group_by(scenario, year, var) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(region = "World")

  GCAM_DATA_wGLOBAL <-
    GCAM_DATA_WORLD %>%
    dplyr::bind_rows(GCAM_DATA %>% dplyr::filter(!(region == "World" & var %in% unique(GCAM_DATA_WORLD$var)))) %>%
    tidyr::complete(tidyr::nesting(scenario, region, var), year = get(paste('reporting_years',GCAM_version,sep='_'), envir = asNamespace("gcamreport"))) %>%
    tidyr::replace_na(list(value = 0)) %>%
    dplyr::distinct(.)

  # filter to final_year.global
  GCAM_DATA_wGLOBAL <- GCAM_DATA_wGLOBAL %>% dplyr::filter(year <= final_year.global)


  report_pre <<-
    get(paste('template',GCAM_version,sep='_'), envir = asNamespace("gcamreport")) %>%
    inner_join(
      GCAM_DATA_wGLOBAL %>%
        na.omit() %>%
        tidyr::pivot_wider(names_from = "year", values_from = "value"),
      by = c("Variable" = "var"), multiple = "all"
    ) %>%
    #  dplyr::left_join(reporting_scen %>% dplyr::select(GCAM_scenario, Scenario),
    #            by = c("scenario" = "GCAM_scenario")) %>%
    dplyr::rename(Region = region) %>%
    #  dplyr::rename(Model = ?..Model) %>%
    dplyr::rename(Scenario = scenario)

  # Add year columns if not present
  missing_cols <- setdiff(reporting_columns.global, colnames(report_pre))
  for (col in missing_cols) {
    report_pre <- report_pre %>% dplyr::mutate(!!col := NA)
  }

  report <<- report_pre %>%
    dplyr::select(all_of(reporting_columns.global)) %>%
    dplyr::filter(!is.na(Region)) # Drop variables we don't report

  if (!(length(desired_variables) == 1 && desired_variables == "All")) {
    report <<- report %>%
      dplyr::filter(Variable %in% desired_variables)
  }
}

#########################################################################
#                       CHECKS AND VETTING FUNCTIONS                    #
#########################################################################
#' do_check_trade
#'
#' Check global trade is zero.
#' @keywords internal check
#' @return Verification message indicating if the process was successful.
#' @importFrom magrittr %>%
#' @export
do_check_trade <- function() {
  scenario <- resource <- year <- production <- demand <- check <- NULL

  if (exists("energy_trade_prod")) {
    # check global total is zero
    trade <- energy_trade_prod %>%
      dplyr::left_join(energy_trade_supply, by = c("scenario", "resource", "region", "year")) %>%
      dplyr::group_by(scenario, resource, year) %>%
      dplyr::summarise(
        production = sum(production, na.rm = T),
        demand = sum(demand, na.rm = T)
      ) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(
        diff = (production - demand) / production,
        check = dplyr::if_else(abs(diff) > 1, "ERROR", "OK")
      )

    check_trade_summary <- trade %>%
      dplyr::rename("percentual_diff_between_production_and_demand" = "diff")

    if (nrow(trade %>% dplyr::filter(check == "ERROR")) > 0) {
      res <- list(
        message = "Trade flows: ERROR",
        summary = as.data.frame(check_trade_summary)
      )
    } else {
      res <- list(
        message = "Trade flows: OK",
        summary = check_trade_summary
      )
    }
  } else {
    res <- list(
      message = "Trade flows: Vetting not performed",
      summary = "To check the trade flows, consider introducing `Trade*` as desired variable."
    )
  }
  return(res)
}


#' do_check_vetting
#'
#' Verify vetting and produce plot.
#' @keywords internal check
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @return Verification message indicating if the process was successful.
#' @import ggplot2
#' @importFrom magrittr %>%
#' @export
do_check_vetting <- function(GCAM_version = "v7.0") {
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

  ggplot(check_vet_plot, aes(x = variable, y = value, fill = type)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~variable, scales = "free") +
    labs(x = "", y = "value") +
    theme_classic() +
    theme(
      axis.text.x = element_blank(),
      legend.position = "bottom",
      strip.text = element_text(size = 5),
      legend.title = element_blank()
    )
  if (!dir.exists(file.path(here::here(), "output"))) {
    dir.create(file.path(here::here(), "output"))
  }
  if (!dir.exists(file.path(here::here(), "output", "figure"))) {
    dir.create(file.path(here::here(), "output", "figure"))
  }
  ggsave(file.path(here::here(), "output", "figure", "vetting.tiff"), last_plot(), "tiff", dpi = 200)

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
#' Update the template file. This function checks for new reported variables,
#' as well as cleans no-reported variables
#' @param GCAM_version main GCAM compatible version: v7.0 (default) or v6.0.
#' @keywords internal template
#' @return Updated template as .rda and as csv in the inst/extdata folder
update_template <- function(GCAM_version = "v7.0") {
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

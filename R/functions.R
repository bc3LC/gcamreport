options(dplyr.summarise.inform = FALSE)

#########################################################################
#                           ANCILLARY FUNCTIONS                         #
#########################################################################


#' filter_desired_regions
#'
#' Return vector of the desired regions available in the loaded project
#' @param des_reg vector of the user desired regions
#' @return vector of the desired regions available in the loaded project
#' @importFrom rgcam getQuery
#' @export
filter_desired_regions <- function(des_reg) {
  r <- 1
  rmax <- length(listQueries(prj))
  while (r <= rmax) {
    tmp <- getQuery(prj, listQueries(prj)[r])
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
#' @importFrom xml2 read_xml
#' @keywords internal utils
#' @export
transform_to_xml <- function(parsed_queries_list) {
  queries <- lapply(parsed_queries_list, function(query) {
    query_title <- query$title
    query_xml <- paste("<aQuery>\n  <all-regions/>\n", query$query, "</aQuery>\n", sep = "")
    return(query_xml)
  })
  xml_string <- paste("<queries>", paste(queries, collapse = ""), "</queries>", sep = "")
  xml_doc <- read_xml(xml_string)
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
#' Filter the desired regions of a GCAM project
#' @param data dataframe to be filtered
#' @param desired_regions desired regions to consider. By default, 'All'. Otherwise, specify a vector with all the considered regions.
#' To know all possible regions, run `available_regions()`. ATTENTION: the considered regions will make up "World".
#' In case the project dataset needs to be created, it will be produced with only the specified regions.
#' @param variable dataset variable information
#' @return filtered dataframe
#' @importFrom dplyr mutate filter select
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
        mutate(region = sapply(
          strsplit(as.character(market), pattern),
          function(x) x[1]
        )) %>%
        filter(region %in% desired_regions_tmp) %>%
        select(-region)
    } else if (!(variable %in% c(
      "CO2 concentrations", "global mean temperature",
      "total climate forcing"
    ))) {
      # check the desired regions are available in the data
      avail_reg <- unique(data$region)
      if (!desired_regions %in% avail_reg) {
        not_avail <- setdiff(desired_regions, avail_reg)
        if (length(not_avail) == 1) stop("The desired region ", paste(not_avail, collapse = ""), " is not available in the loaded project. In detail, it is not availabe in the query '", v, "'.")
        if (length(not_avail) > 1) stop("The desired regions ", paste(not_avail, collapse = ", "), " are not available in the loaded project. In detail, they are not availabe in the query '", v, "'.")
      }
      data <- data %>%
        filter(region %in% desired_regions)
    }
  }

  return(data)
}


#' filter_variables
#'
#' Filter the desired regions of a GCAM project
#' @param data dataframe to be filtered
#' @param variable variable that requires this data
#' @return filtered dataframe
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @keywords internal utils
#' @export
filter_variables <- function(data, variable) {
  var <- NULL

  if (variable %in% variables.global[variables.global$required == TRUE, ]$name) {
    if (!(length(desired_variables) == 1 && desired_variables == "All")) {
      if ("var" %in% colnames(data)) {
        data <- data %>%
          filter(var %in% desired_variables)
      }
    }
  }

  return(invisible(data))
}


#' gather_map
#'
#' Formats all maps into a long table
#' @keywords internal
#' @importFrom tidyr gather
#' @importFrom dplyr select filter
#' @export
gather_map <- function(df) {
  . <- identifier <- var <- NULL

  untouched_cols <- names(df) %>% .[!grepl("var", names(df))]
  df %>%
    gather(identifier, var, -untouched_cols) %>%
    select(-identifier) %>%
    filter(!is.na(var), var != "") %>%
    return()
}


#' conv_ghg_co2e
#'
#' Covert GHG to CO2e
#' @param data dataset
#' @importFrom dplyr filter left_join mutate select
#' @importFrom tidyr separate
#' @importFrom magrittr %>%
#' @keywords internal conversion
#' @export
conv_ghg_co2e <- function(data) {
  ghg <- variable <- value <- GWP <- NULL

  # GHG emission conversion
  res <- suppressWarnings(
    data %>%
      separate(ghg, into = c("variable", "sector"), sep = "_", fill = "right") %>%
      filter(variable %in% gcamreport::GHG_gases) %>%
      left_join(gcamreport::GWP_adjuster, by = c("variable" = "GHG_gases")) %>%
      mutate(value = value * GWP, Units = "CO2e") %>%
      select(-GWP)
  )

  return(res)
}


#' conv_EJ_GW
#'
#' Covert EJ to GW
#' @param data dataset
#' @param cf conversion factor
#' @param EJ EJ amount
#' @importFrom dplyr mutate
#' @importFrom magrittr %>%
#' @keywords internal conversion
#' @export
conv_EJ_GW <- function(data, cf, EJ) {
  data %>%
    mutate(gw = EJ / (cf * gcamreport::convert$hr_per_yr * gcamreport::convert$EJ_to_GWh))
}

#' approx_fun
#'
#' Interpolation function
#' @param year year to consider
#' @param value values to extrapolate from
#' @param rule number of points to extrapolate
#' @importFrom stats approx
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
#' @return population_clean global variable
#' @importFrom rgcam getQuery
#' @importFrom dplyr mutate select
#' @importFrom magrittr %>%
#' @export
get_population <- function() {
  value <- NULL

  population_clean <<-
    getQuery(prj, "population by region") %>%
    mutate(
      value = value * gcamreport::convert$conv_thousand_million,
      var = "Population"
    ) %>%
    select(all_of(gcamreport::long_columns))
}


#' get_gdp_ppp
#'
#' Get GDP (PPP) query, compute regional GDP, and change units to [10USD].
#' @keywords internal GDP
#' @return GDP_PPP_clean global variable
#' @importFrom rgcam getQuery
#' @importFrom dplyr mutate select left_join rename
#' @importFrom magrittr %>%
#' @export
get_gdp_ppp <- function() {
  value <- pop_mill <- NULL

  GDP_PPP_clean <<-
    getQuery(prj, "GDP per capita PPP by region") %>%
    left_join(population_clean %>% rename(pop_mill = value), by = c("scenario", "region", "year")) %>%
    mutate(
      value = value * pop_mill * gcamreport::convert$conv_90USD_10USD,
      var = "GDP|PPP"
    ) %>%
    select(all_of(gcamreport::long_columns))
}


#' get_gdp_mer
#'
#' Get GDP (MER) query and change units to [10USD].
#' @keywords internal GDP
#' @return GDP_MER_clean global variable
#' @importFrom rgcam getQuery
#' @importFrom dplyr mutate select
#' @importFrom magrittr %>%
#' @export
get_gdp_mer <- function() {
  value <- NULL

  GDP_MER_clean <<-
    getQuery(prj, "GDP MER by region") %>%
    mutate(
      value = value * gcamreport::convert$conv_million_billion * gcamreport::convert$conv_90USD_10USD,
      var = "GDP|MER"
    ) %>%
    select(all_of(gcamreport::long_columns))
}

# Climate and emissions
# ==============================================================================================
#' get_forcing
#'
#' Get World's forcing query.
#' @keywords internal forcing
#' @return forcing_clean global variable
#' @importFrom rgcam getQuery
#' @importFrom dplyr mutate select filter
#' @importFrom magrittr %>%
#' @export
get_forcing <- function() {
  year <- NULL

  forcing_clean <<-
    getQuery(prj, "total climate forcing") %>%
    filter(year %in% gcamreport::GCAM_years) %>%
    mutate(var = "Forcing", region = "World") %>%
    select(all_of(gcamreport::long_columns))
}


#' get_temperature
#'
#' Get World's mean temperature query.
#' @keywords internal temperature
#' @return global_temp_clean global variable
#' @importFrom rgcam getQuery
#' @importFrom dplyr mutate select filter
#' @importFrom magrittr %>%
#' @export
get_temperature <- function() {
  year <- NULL

  global_temp_clean <<-
    getQuery(prj, "global mean temperature") %>%
    filter(year %in% gcamreport::GCAM_years) %>%
    mutate(var = "Temperature|Global Mean", region = "World") %>%
    select(all_of(gcamreport::long_columns))
}


#' get_co2_concentration
#'
#' Get World's CO2 concentration query.
#' @keywords internal co2
#' @return co2_concentration_clean global variable
#' @importFrom rgcam getQuery
#' @importFrom dplyr mutate select filter
#' @importFrom magrittr %>%
#' @export
get_co2_concentration <- function() {
  year <- NULL

  co2_concentration_clean <<-
    getQuery(prj, "CO2 concentrations") %>%
    filter(year %in% gcamreport::GCAM_years) %>%
    mutate(var = "Concentration|CO2", region = "World") %>%
    select(all_of(gcamreport::long_columns))
}


#' get_co2
#'
#' Get World's CO2 emissions query.
#' @keywords internal co2
#' @return co2_clean global variable
#' @importFrom tibble as_tibble
#' @importFrom rgcam getQuery
#' @importFrom dplyr mutate select group_by ungroup left_join summarise
#' @importFrom magrittr %>%
#' @export
get_co2 <- function() {
  value <- unit_conv <- scenario <- region <- year <- var <- NULL

  co2_clean <<-
    as_tibble(getQuery(prj, "CO2 emissions by sector (no bio) (excluding resource production)")) %>%
    left_join(filter_variables(gcamreport::co2_sector_map, "co2_clean"), by = "sector", multiple = "all") %>%
    mutate(value = value * unit_conv) %>%
    group_by(scenario, region, year, var) %>% #
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    select(all_of(gcamreport::long_columns))
}


#' get_co2_ets
#'
#' Get World's CO2 ETS emissions query.
#' @keywords internal co2
#' @return co2_ets_by reg and co2_ets_bysec global variables
#' @importFrom tibble as_tibble
#' @importFrom rgcam getQuery
#' @importFrom dplyr filter mutate select left_join group_by summarise ungroup
#' @importFrom magrittr %>%
#' @export
get_co2_ets <- function() {
  ghg <- value <- year <- unit_conv <- scenario <- region <- var <- NULL

  co2_ets_byreg <<-
    as_tibble(getQuery(prj, "nonCO2 emissions by region")) %>%
    filter(ghg == "CO2_ETS") %>%
    # change units to CO2 equivalent and set the variable
    mutate(
      value = value * gcamreport::convert$CO2_equivalent,
      var = "Emissions|CO2_ETS|Energy and Industrial Processes"
    ) %>%
    select(all_of(gcamreport::long_columns))
  co2_ets_bysec <<-
    as_tibble(getQuery(prj, "nonCO2 emissions by sector (excluding resource production)")) %>%
    filter(ghg == "CO2_ETS") %>%
    # change units to CO2 equivalent and group by sector
    left_join(filter_variables(gcamreport::co2_ets_sector_map, "co2_ets_bysec"), by = "sector", multiple = "all") %>%
    mutate(value = value * unit_conv) %>%
    group_by(scenario, region, year, var) %>% #
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    select(all_of(gcamreport::long_columns))
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
#' @importFrom rgcam getQuery
#' @importFrom dplyr if_else mutate select left_join
#' @importFrom magrittr %>%
#' @export
get_nonbio_tmp <- function() {
  value.y <- value.x <- NULL

  nonbio_share <<-
    getQuery(prj, "CO2 emissions by sector (excluding resource production)") %>%
    left_join(getQuery(prj, "CO2 emissions by sector (no bio) (excluding resource production)"), by = c("region", "scenario", "year", "sector", "Units")) %>%
    mutate(
      value.y = if_else(is.na(value.y), value.x, value.y),
      percent = value.y / value.x
    ) %>%
    select(-value.x, -value.y)
}


#' get_co2_tech_nobio_tmp
#'
#' Get no bio CO2 emissions query by sector and techonolgy.
#' @keywords internal co2 tmp
#' @return co2_tech_nobio global variable
#' @importFrom rgcam getQuery
#' @importFrom dplyr if_else mutate select left_join
#' @importFrom magrittr %>%
#' @export
get_co2_tech_nobio_tmp <- function() {
  value <- percent <- NULL

  co2_tech_nobio <<-
    getQuery(prj, "CO2 emissions by tech (excluding resource production)") %>%
    left_join(nonbio_share, by = c("region", "scenario", "year", "sector", "Units")) %>%
    mutate(value = value * percent) %>%
    select(-percent)
}


#' get_co2_tech_emissions_tmp
#'
#' Get no bio CO2 emissions query by sector, subsector, and techonolgy.
#' @keywords internal co2 tmp
#' @return co2_tech_emissions global variable
#' @importFrom dplyr left_join filter mutate group_by summarise ungroup select
#' @importFrom magrittr %>%
#' @export
get_co2_tech_emissions_tmp <- function() {
  var <- value <- unit_conv <- scenario <- region <- year <- NULL

  co2_tech_emissions <<-
    co2_tech_nobio %>%
    left_join(filter_variables(gcamreport::co2_tech_map, "co2_tech_emissions"), by = c("sector", "subsector", "technology"), multiple = "all") %>%
    filter(!is.na(var)) %>%
    mutate(value = value * unit_conv) %>%
    group_by(scenario, region, year, var) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    select(all_of(gcamreport::long_columns))
}


# Iron and Steel Emissions: for Emissions|CO2|Coal, Gas, Oil
# Find which input has the greatest share for each IRONSTL tech (between coal, gas, oil)

#' get_iron_steel_map
#'
#' Get iron and steel emissions.
#' @keywords internal iron steel
#' @return iron_steel_map global variable
#' @importFrom rgcam getQuery
#' @importFrom dplyr if_else mutate select ungroup filter
#' @importFrom magrittr %>%
#' @export
get_iron_steel_map <- function() {
  sector <- input <- value <- Units <- scenario <- NULL

  iron_steel_map <<-
    getQuery(prj, "industry final energy by tech and fuel") %>%
    filter(
      sector == "iron and steel",
      input %in% c("wholesale gas", "refined liquids industrial", "delivered coal")
    ) %>%
    mutate(
      max = max(value),
      save = if_else(value == max, 1, 0)
    ) %>%
    filter(save == 1) %>%
    ungroup() %>%
    select(-save, -max, -Units, -scenario, -value)
}


#' get_co2_iron_steel
#'
#' Get iron and steel CO2 emissions.
#' @keywords internal iron steel co2
#' @return co2_tech_ironsteel global variable
#' @importFrom dplyr mutate select group_by ungroup filter left_join rename summarise
#' @importFrom stringr str_replace
#' @importFrom magrittr %>%
#' @export
get_co2_iron_steel <- function() {
  sector <- input <- value <- scenario <- region <- year <- var <- na.omit <- NULL

  co2_tech_ironsteel <<-
    co2_tech_nobio %>% # Using redistributed bio version
    filter(sector == "iron and steel") %>%
    left_join(filter_variables(iron_steel_map, "co2_tech_ironsteel"), by = c("sector", "subsector", "technology", "year", "region")) %>%
    mutate(
      input = str_replace(input, "wholesale gas", "Emissions|CO2|Energy|Gas"),
      input = str_replace(input, "refined liquids industrial", "Emissions|CO2|Energy|Oil"),
      input = str_replace(input, "delivered coal", "Emissions|CO2|Energy|Coal")
    ) %>%
    rename(var = input) %>%
    mutate(value = value * gcamreport::convert$conv_C_CO2) %>%
    group_by(scenario, region, year, var) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    na.omit() %>%
    select(all_of(gcamreport::long_columns))
}


#' get_lu_co2
#'
#' Get land use CO2 emissions.
#' @keywords internal lu co2
#' @return LU_carbon_clean global variable
#' @importFrom rgcam getQuery
#' @importFrom dplyr mutate select group_by ungroup filter summarise
#' @importFrom magrittr %>%
#' @export
get_lu_co2 <- function() {
  year <- scenario <- region <- value <- var <- NULL

  LU_carbon_clean <<-
    # Land use CO2
    getQuery(prj, "LUC emissions by region") %>%
    filter(year %in% gcamreport::GCAM_years) %>%
    group_by(scenario, region, year) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    mutate(
      value = value * gcamreport::convert$conv_C_CO2,
      var = "Emissions|CO2|AFOLU"
    ) %>%
    select(all_of(gcamreport::long_columns)) %>%
    group_by(scenario, var, year)
}


#' get_co2_emissions
#'
#' Combine CO2 emission queries.
#' @keywords internal co2 process
#' @return co2_emissions_clean global variable
#' @importFrom dplyr select group_by ungroup summarise bind_rows
#' @importFrom magrittr %>%
#' @export
get_co2_emissions <- function() {
  scenario <- region <- year <- var <- value <- NULL

  co2_emissions_clean <<-
    bind_rows(co2_clean, LU_carbon_clean, co2_tech_emissions) %>%
    group_by(scenario, region, year, var) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    select(all_of(gcamreport::long_columns))
}

#' get_total_co2_emissions
#'
#' Compute total CO2 emission.
#' @keywords internal co2 process
#' @return tot_co2_clean global variable
#' @importFrom dplyr select group_by ungroup summarise bind_rows mutate
#' @importFrom magrittr %>%
#' @export
get_total_co2_emissions <- function() {
  var <- scenario <- region <- year <- value <- NULL

  tot_co2_clean <<-
    bind_rows(co2_clean %>% filter(var == "Emissions|CO2|Energy and Industrial Processes"), LU_carbon_clean) %>%
    group_by(scenario, region, year) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    mutate(var = "Emissions|CO2") %>%
    select(all_of(gcamreport::long_columns))
}


#' get_nonco2_emissions
#'
#' Get non CO2 emissions query.
#' @keywords internal nonco2
#' @return nonco2_clean global variable
#' @importFrom rgcam getQuery
#' @importFrom dplyr select group_by ungroup summarise bind_rows mutate left_join
#' @importFrom magrittr %>%
#' @export
get_nonco2_emissions <- function() {
  value <- unit_conv <- scenario <- region <- year <- var <- NULL

  nonco2_clean <<-
    getQuery(prj, "nonCO2 emissions by sector (excluding resource production)") %>%
    left_join(filter_variables(gcamreport::nonco2_emis_sector_map, "nonco2_clean"), by = c("ghg", "sector"), multiple = "all") %>%
    bind_rows(getQuery(prj, "nonCO2 emissions by resource production") %>%
      left_join(filter_variables(gcamreport::nonco2_emis_resource_map, "nonco2_clean"), by = c("ghg", "resource"), multiple = "all")) %>%
    mutate(value = value * unit_conv) %>%
    group_by(scenario, region, year, var) %>% #
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    select(all_of(gcamreport::long_columns))
}

#' get_fgas
#'
#' Compute F-Gases emissions.
#' @keywords internal f-gases process
#' @return f_gas_clean global variable
#' @importFrom rgcam getQuery
#' @importFrom dplyr select group_by ungroup summarise mutate filter
#' @importFrom magrittr %>%
#' @export
get_fgas <- function() {
  ghg <- variable <- scenario <- region <- year <- value <- NULL

  f_gas_clean <<-
    getQuery(prj, "nonCO2 emissions by region") %>%
    filter(!grepl("CO2_ETS", ghg)) %>%
    conv_ghg_co2e() %>%
    filter(variable %in% gcamreport::F_GASES) %>%
    group_by(scenario, region, year) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    mutate(var = "Emissions|F-Gases") %>%
    select(all_of(gcamreport::long_columns))
}


#' get_ghg
#'
#' Get total GHG emissions.
#' @keywords internal ghg
#' @return ghg_clean global variable
#' @importFrom rgcam getQuery
#' @importFrom dplyr select group_by ungroup summarise mutate filter bind_rows
#' @importFrom magrittr %>%
#' @export
get_ghg <- function() {
  ghg <- variable <- scenario <- region <- year <- value <- NULL

  ghg_all <<-
    getQuery(prj, "nonCO2 emissions by region") %>%
    filter(!grepl("CO2_ETS", ghg)) %>%
    conv_ghg_co2e() %>%
    filter(variable %in% gcamreport::GHG_gases) %>%
    bind_rows(LU_carbon_clean) %>%
    group_by(scenario, region, year) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    mutate(var = "Emissions|Kyoto Gases") %>%
    select(all_of(gcamreport::long_columns))
}


#' get_ghg_sector
#'
#' Get sectorial GHG emissions.
#' @keywords internal ghg
#' @return ghg_sector_clean global variable
#' @importFrom rgcam getQuery
#' @importFrom dplyr select group_by ungroup summarise mutate filter bind_rows rename left_join
#' @importFrom magrittr %>%
#' @export
get_ghg_sector <- function() {
  ghg <- resource <- subresource <- sector <- variable <- scenario <-
    region <- var <- year <- value <- NULL

  ghg_sector_clean <<-
    getQuery(prj, "nonCO2 emissions by sector (excluding resource production)") %>%
    filter(!grepl("CO2", ghg), !grepl("CO2_ETS", ghg)) %>%
    bind_rows(getQuery(prj, "nonCO2 emissions by resource production") %>%
      rename(sector = resource) %>%
      select(-subresource)) %>%
    bind_rows(getQuery(prj, "CO2 emissions by sector (no bio) (excluding resource production)") %>%
      mutate(ghg = "CO2")) %>%
    mutate(subsector = sector) %>%
    conv_ghg_co2e() %>%
    filter(variable %in% gcamreport::GHG_gases) %>%
    rename(ghg = variable) %>%
    left_join(filter_variables(gcamreport::kyoto_sector_map, "ghg_sector_clean"),
      by = c("ghg", "subsector", "sector"), multiple = "all"
    ) %>%
    select(all_of(gcamreport::long_columns)) %>%
    bind_rows(
      LU_carbon_clean %>%
        mutate(var = "Emissions|Kyoto Gases"),
      LU_carbon_clean %>%
        mutate(var = "Emissions|Kyoto Gases|AFOLU")
    ) %>%
    group_by(scenario, region, var, year) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup()
}


#' get_co2_sequestration
#'
#' Get carbon sequestration.
#' @keywords internal co2
#' @return co2_sequestration_clean global variable
#' @importFrom tidyr complete nesting spread
#' @importFrom rgcam getQuery
#' @importFrom dplyr select group_by ungroup summarise mutate filter left_join
#' @importFrom magrittr %>%
#' @export
get_co2_sequestration <- function() {
  scenario <- region <- year <- var <- value <- unit_conv <- NULL

  co2_sequestration_clean <<- suppressWarnings(
    getQuery(prj, "CO2 sequestration by tech") %>%
      left_join(filter_variables(gcamreport::carbon_seq_tech_map, "co2_sequestration_clean"), by = c("sector", "technology"), multiple = "all") %>%
      complete(nesting(scenario, region, year),
        var = unique(var),
        fill = list(value = 0)
      ) %>%
      filter(!is.na(var)) %>% # , var!= "Carbon Sequestration|Feedstocks",var != "Carbon Sequestration|Feedstocks|Liquids") %>%
      mutate(value = value * unit_conv) %>%
      group_by(scenario, region, year, var) %>% #
      summarise(value = sum(value, na.rm = T)) %>%
      ungroup() %>% # spread(year, value) -> d
      select(all_of(gcamreport::long_columns))
  )
}


# Agriculture and land use
# ==============================================================================================
#' get_ag_demand
#'
#' Get agricultural demand.
#' @keywords internal ag
#' @return ag_demand_clean global variable
#' @importFrom rgcam getQuery
#' @importFrom dplyr select group_by ungroup summarise mutate filter left_join bind_rows if_else
#' @importFrom magrittr %>%
#' @export
get_ag_demand <- function() {
  sector <- input <- var <- value <- unit_conv <- scenario <- region <- year <- NULL

  ag_demand_clean <<-
    bind_rows(
      getQuery(prj, "demand balances by crop commodity"),
      getQuery(prj, "demand balances by meat and dairy commodity")
    ) %>%
    # Adjust OtherMeat_Fish
    mutate(sector = if_else(sector == "FoodDemand_NonStaples" & input == "OtherMeat_Fish", "OtherMeat_Fish", sector)) %>%
    left_join(filter_variables(gcamreport::ag_demand_map, "ag_demand_clean"), by = c("sector"), multiple = "all") %>%
    filter(!is.na(var)) %>%
    mutate(value = value * unit_conv) %>%
    group_by(scenario, region, year, var) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    select(all_of(gcamreport::long_columns))
}


#' get_ag_production
#'
#' Get agricultural production.
#' @keywords internal ag
#' @return ag_production_clean global variable
#' @importFrom rgcam getQuery
#' @importFrom dplyr select group_by ungroup summarise mutate filter
#' @importFrom magrittr %>%
#' @export
get_ag_production <- function() {
  Units <- scenario <- region <- year <- var <- value <- NULL

  ag_production_clean <<-
    getQuery(prj, "ag production by crop type") %>%
    filter(Units == "Mt") %>%
    # Forests produce in units of billion m3 and biomass produces in EJ. We'll need to find a conversion factor to include it
    # 1 m3 = .001 tons
    mutate(var = "Agricultural Production") %>%
    group_by(scenario, region, year, var) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    select(all_of(gcamreport::long_columns))
}


#' get_land
#'
#' Get land use area.
#' @keywords internal ag
#' @return land_clean global variable
#' @importFrom rgcam getQuery
#' @importFrom dplyr group_by ungroup summarise mutate left_join
#' @importFrom magrittr %>%
#' @export
get_land <- function() {
  value <- unit_conv <- scenario <- region <- year <- var <- NULL

  land_clean <<-
    getQuery(prj, "aggregated land allocation") %>%
    left_join(filter_variables(gcamreport::land_use_map, "land_clean"), by = c("landleaf"), multiple = "all") %>%
    mutate(value = value * unit_conv) %>%
    group_by(scenario, region, year, var) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup()
}


# Primary Energy
# ==============================================================================================
#' get_primary_energy
#'
#' Get primary energy consumption by tech.
#' @keywords internal energy
#' @return primary_energy_clean global variable
#' @importFrom tidyr complete nesting
#' @importFrom rgcam getQuery
#' @importFrom dplyr select group_by ungroup summarise mutate filter left_join
#' @importFrom magrittr %>%
#' @export
get_primary_energy <- function() {
  fuel <- Units <- year <- var <- value <- unit_conv <- scenario <- region <- NULL

  primary_energy_clean <<-
    getQuery(prj, "primary energy consumption with CCS by region (direct equivalent)") %>%
    filter(
      !grepl("water", fuel),
      Units == "EJ"
    ) %>%
    left_join(filter_variables(gcamreport::primary_energy_map, "primary_energy_clean"), by = c("fuel"), multiple = "all") %>%
    filter(!is.na(var)) %>%
    mutate(value = value * unit_conv) %>%
    group_by(scenario, region, year, var) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    complete(nesting(scenario, region, year),
      var = unique(var),
      fill = list(value = 0)
    ) %>%
    select(all_of(gcamreport::long_columns))
}


#' get_energy_trade_prod
#'
#' Get energy trade.
#' @keywords internal energy
#' @return energy_trade_prod global variable
#' @importFrom rgcam getQuery
#' @importFrom dplyr group_by ungroup summarise mutate filter
#' @importFrom magrittr %>%
#' @export
get_energy_trade_prod <- function() {
  Units <- resource <- scenario <- region <- year <- value <- NULL

  energy_trade_prod <<-
    getQuery(prj, "resource production") %>%
    filter(Units == "EJ") %>%
    filter(resource %in% c("coal", "natural gas", "crude oil", "unconventional oil")) %>%
    mutate(
      resource = sub("crude oil", "oil", resource),
      resource = sub("unconventional oil", "oil", resource)
    ) %>%
    group_by(scenario, resource, region, year) %>%
    summarise(production = sum(value)) %>%
    ungroup()
}


#' get_energy_trade_tmp
#'
#' Get energy trade supply. Query used to compute other variables.
#' @keywords internal energy tmp
#' @return energy_trade_supply global variable
#' @importFrom tidyr separate
#' @importFrom rgcam getQuery
#' @importFrom dplyr group_by ungroup summarise filter
#' @importFrom magrittr %>%
#' @export
get_energy_trade_tmp <- function() {
  market <- resource <- scenario <- region <- year <- value <- NULL

  energy_trade_supply <<- suppressWarnings(
    getQuery(prj, "supply of all markets") %>%
      filter(grepl("regional coal", market) | grepl("regional natural gas", market) | grepl("regional oil", market)) %>%
      separate(market, into = c("region", "resource"), sep = "regional ", fill = "right") %>%
      filter(resource != "oilpalm", resource != "oilcrop") %>%
      group_by(scenario, resource, region, year) %>%
      summarise(demand = sum(value)) %>%
      ungroup()
  )
}


#' get_energy_trade
#'
#' Get energy trade supply.
#' @keywords internal energy tmp
#' @return energy_trade_clean global variable
#' @importFrom dplyr left_join mutate select
#' @importFrom magrittr %>%
#' @export
get_energy_trade <- function() {
  production <- demand <- resource <- NULL

  energy_trade_clean <<-
    energy_trade_prod %>%
    left_join(energy_trade_supply, by = c("scenario", "resource", "region", "year")) %>%
    mutate(
      value = production - demand,
      resource = sub("coal", "Coal", resource),
      resource = sub("natural gas", "Gas", resource),
      resource = sub("oil", "Oil", resource),
      var = paste0("Trade|Primary Energy|", resource, "|Volume")
    ) %>%
    filter_variables(variable = "energy_trade_clean") %>%
    select(all_of(gcamreport::long_columns))
}

# Secondary Energy
# ==============================================================================================
#' get_elec_gen_tech
#'
#' Get electricity generation
#' @keywords internal electricity
#' @return elec_gen_tech_clean global variable
#' @importFrom tidyr complete nesting
#' @importFrom rgcam getQuery
#' @importFrom dplyr left_join mutate select filter ungroup group_by summarise
#' @importFrom magrittr %>%
#' @export
get_elec_gen_tech <- function() {
  var <- value <- unit_conv <- scenario <- region <- year <- NULL

  elec_gen_tech_clean <<-
    getQuery(prj, "elec gen by gen tech") %>%
    left_join(filter_variables(gcamreport::elec_gen_map, "elec_gen_tech_clean"), by = c("output", "subsector", "technology"), multiple = "all") %>%
    filter(!is.na(var)) %>%
    mutate(value = value * unit_conv) %>%
    group_by(scenario, region, year, var) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    complete(nesting(scenario, region, year),
      var = unique(var),
      fill = list(value = 0)
    ) %>%
    select(all_of(gcamreport::long_columns))
}


#' get_secondary_solids
#'
#' Get secondary solids
#' @keywords internal energy
#' @return secondary_solids global variable
#' @importFrom rgcam getQuery
#' @importFrom dplyr mutate filter ungroup group_by summarise bind_rows select
#' @importFrom magrittr %>%
#' @export
get_secondary_solids <- function() {
  input <- scenario <- region <- year <- value <- NULL

  secondary_solids <<-
    getQuery(prj, "inputs by sector") %>%
    filter(input %in% c("delivered biomass", "delivered coal")) %>%
    group_by(scenario, region, year, input) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    mutate(var = ifelse(input == "delivered biomass", "Secondary Energy|Solids|Biomass",
      "Secondary Energy|Solids|Coal"
    )) %>%
    bind_rows(getQuery(prj, "inputs by sector") %>%
      filter(input %in% c("delivered biomass", "delivered coal")) %>%
      group_by(scenario, region, year) %>%
      summarise(value = sum(value, na.rm = T)) %>%
      ungroup() %>%
      mutate(var = "Secondary Energy|Solids")) %>%
    filter_variables(variable = "secondary_solids") %>%
    select(all_of(gcamreport::long_columns))
}


#' get_se_gen_tech
#'
#' Get other secondary energy production
#' @keywords internal energy
#' @return se_gen_tech_clean global variable
#' @importFrom tidyr complete nesting
#' @importFrom rgcam getQuery
#' @importFrom dplyr mutate filter ungroup group_by summarise bind_rows select left_join
#' @importFrom magrittr %>%
#' @export
get_se_gen_tech <- function() {
  var <- value <- unit_conv <- scenario <- region <- year <- NULL

  se_gen_tech_clean <<-
    bind_rows(
      getQuery(prj, "gas production by tech"),
      getQuery(prj, "hydrogen production by tech"),
      getQuery(prj, "refined liquids production by tech")
    ) %>%
    left_join(filter_variables(gcamreport::se_gen_map, "se_gen_tech_clean"), by = c("sector", "subsector", "technology"), multiple = "all") %>%
    filter(!is.na(var)) %>%
    mutate(value = value * unit_conv) %>%
    group_by(scenario, region, year, var) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    complete(nesting(scenario, region, year),
      var = unique(var),
      fill = list(value = 0)
    ) %>%
    select(all_of(gcamreport::long_columns)) %>%
    bind_rows(secondary_solids)
}


# Final Energy
# ==============================================================================================
# demand by sector by technology

#' get_fe_sector_tmp
#'
#' Get final energy demand by sector
#' @keywords internal energy tmp
#' @return fe_sector global variable
#' @importFrom tidyr complete nesting
#' @importFrom rgcam getQuery
#' @importFrom dplyr mutate filter ungroup group_by summarise select left_join
#' @importFrom magrittr %>%
#' @export
get_fe_sector_tmp <- function() {
  var <- value <- unit_conv <- scenario <- region <- year <- NULL

  fe_sector <<-
    getQuery(prj, "final energy consumption by sector and fuel") %>%
    left_join(filter_variables(gcamreport::final_energy_map, "fe_sector"), by = c("sector", "input"), multiple = "all") %>%
    filter(!is.na(var)) %>%
    mutate(value = value * unit_conv) %>%
    group_by(scenario, region, year, var) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    complete(nesting(scenario, region, year),
      var = unique(var),
      fill = list(value = 0)
    ) %>%
    select(all_of(gcamreport::long_columns))
}


#' get_fe_transportation_tmp
#'
#' Get mode-specific transport final energy to break out rail, ship, and domestic air.
#' @keywords internal energy  tmp
#' @return fe_transportation global variable
#' @importFrom tidyr complete nesting
#' @importFrom rgcam getQuery
#' @importFrom dplyr mutate filter ungroup group_by summarise select left_join
#' @importFrom magrittr %>%
#' @export
get_fe_transportation_tmp <- function() {
  var <- value <- unit_conv <- scenario <- region <- year <- NULL

  fe_transportation <<-
    getQuery(prj, "transport final energy by mode and fuel") %>%
    left_join(filter_variables(gcamreport::transport_final_en_map, "fe_transportation"), by = c("sector", "input", "mode"), multiple = "all") %>%
    filter(!is.na(var)) %>%
    mutate(value = value * unit_conv) %>%
    group_by(scenario, region, year, var) %>%
    summarise(value = sum(value,
      na.rm = T
    )) %>%
    ungroup() %>%
    complete(nesting(scenario, region, year),
      var = unique(var),
      fill = list(value = 0)
    ) %>%
    select(all_of(gcamreport::long_columns))
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
#' @importFrom dplyr bind_rows ungroup group_by summarise
#' @importFrom magrittr %>%
#' @export
get_fe_sector <- function() {
  scenario <- region <- var <- year <- value <- NULL

  fe_sector_clean <<-
    bind_rows(fe_sector, fe_transportation) %>%
    group_by(scenario, region, var, year) %>%
    summarise(value = sum(value)) %>%
    ungroup()
}


# Energy Service ----------------------------------------------------------

#' get_energy_service_transportation
#'
#' Get transport.
#' @keywords internal energy
#' @return energy_service_transportation_clean global variable
#' @importFrom rgcam getQuery
#' @importFrom dplyr mutate filter ungroup group_by summarise select left_join
#' @importFrom magrittr %>%
#' @export
get_energy_service_transportation <- function() {
  var <- value <- unit_conv <- scenario <- region <- year <- NULL

  energy_service_transportation_clean <<-
    getQuery(prj, "transport service output by mode") %>%
    left_join(filter_variables(gcamreport::transport_en_service, "energy_service_transportation_clean"), by = c("sector", "mode"), multiple = "all") %>%
    filter(!is.na(var)) %>%
    mutate(value = value * unit_conv) %>%
    group_by(scenario, region, year, var) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    select(all_of(gcamreport::long_columns))
}


#' get_energy_service_buildings
#'
#' Get ES buildings.
#' @keywords internal energy
#' @return energy_service_buildings_clean global variable
#' @importFrom rgcam getQuery
#' @importFrom dplyr mutate filter ungroup group_by summarise select left_join
#' @importFrom magrittr %>%
#' @export
get_energy_service_buildings <- function() {
  var <- value <- unit_conv <- scenario <- region <- year <- NULL

  energy_service_buildings_clean <<-
    getQuery(prj, "building floorspace") %>%
    left_join(filter_variables(gcamreport::buildings_en_service, "energy_service_buildings_clean"), by = c("building"), multiple = "all") %>%
    filter(!is.na(var)) %>%
    mutate(value = value * unit_conv) %>%
    group_by(scenario, region, year, var) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    select(all_of(gcamreport::long_columns))
}



# industry production
# could add chemicals but they're in terms of EJ, need to also add cement

#' get_industry_production
#'
#' Get industry production.
#' @keywords internal energy
#' @return industry_production_clean global variable
#' @importFrom rgcam getQuery
#' @importFrom dplyr filter ungroup group_by summarise select left_join
#' @importFrom magrittr %>%
#' @export
get_industry_production <- function() {
  var <- scenario <- region <- year <- value <- NULL

  industry_production_clean <<-
    getQuery(prj, "industry primary output by sector") %>%
    left_join(filter_variables(gcamreport::production_map, "industry_production_clean"), by = c("sector")) %>%
    # filter variables that are in terms of Mt
    filter(var %in% c("Production|Cement", "Production|Steel", "Production|Non-ferrous metals")) %>%
    group_by(scenario, region, var, year) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    select(all_of(gcamreport::long_columns))
}

#' get_iron_steel_imports
#'
#' Get iron steel imports
#' @keywords internal industry tmp
#' @return iron_steel_imports global variable
#' @importFrom rgcam getQuery
#' @importFrom dplyr filter ungroup group_by summarise select left_join
#' @importFrom magrittr %>%
#' @export
get_iron_steel_imports <- function() {
  var <- scenario <- region <- year <- value <- NULL

  iron_steel_imports <<-
    getQuery(prj, "regional iron and steel sources") %>%
    filter(subsector == "domestic iron and steel") %>%
    left_join(filter_variables(gcamreport::iron_steel_trade_map, "iron_steel_clean"), by = c("sector")) %>%
    # filter variables that are in terms of Mt
    group_by(scenario, region, var, year) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    select(all_of(gcamreport::long_columns))
}

#' get_iron_steel_exports
#'
#' Get iron steel production.
#' @keywords internal industry tmp
#' @return iron_steel_exports global variable
#' @importFrom rgcam getQuery
#' @importFrom stringr str_replace_all
#' @importFrom dplyr filter ungroup group_by summarise select left_join
#' @importFrom magrittr %>%
#' @export
get_iron_steel_exports <- function() {
  var <- scenario <- region <- year <- value <- NULL

  iron_steel_exports <<-
    getQuery(prj, "traded iron and steel") %>%
    left_join(filter_variables(gcamreport::iron_steel_trade_map, "iron_steel_clean"), by = c("sector")) %>%
    # extract region
    mutate(region = str_replace_all(subsector, " traded iron and steel", "")) %>%
    filter(region %in% desired_regions) %>%
    # filter variables that are in terms of Mt
    group_by(scenario, region, var, year) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    select(all_of(gcamreport::long_columns))
}

#' get_iron_steel_clean
#'
#' Get iron steel imports & exports
#' @keywords internal industry
#' @return iron_steel_clean global variable
#' @importFrom dplyr filter ungroup group_by summarise select left_join
#' @importFrom magrittr %>%
#' @export
get_iron_steel_clean <- function() {
  iron_steel_clean <<- bind_rows(
    iron_steel_imports,
    iron_steel_exports
  )
}


# Prices
# ==============================================================================================
#' get_ag_prices_wld_tmp
#'
#' Get ag price index.
#' @keywords internal ag tmp
#' @return ag_prices_wld global variable
#' @importFrom rgcam getQuery
#' @importFrom dplyr mutate filter ungroup group_by summarise left_join
#' @importFrom magrittr %>%
#' @export
get_ag_prices_wld_tmp <- function() {
  var <- scenario <- sector <- year <- value <- NULL

  ag_prices_wld <<-
    getQuery(prj, "prices by sector") %>%
    left_join(filter_variables(gcamreport::ag_prices_map, "ag_prices_wld"), by = c("sector")) %>%
    filter(!is.na(var)) %>%
    group_by(scenario, sector, year) %>%
    summarise(value = mean(value, na.rm = T)) %>%
    ungroup() %>%
    mutate(region = "World")
}

#' get_ag_prices
#'
#' Calculate average mean for ag global index
#' @keywords internal ag
#' @return ag_prices_clean global variable
#' @importFrom rgcam getQuery
#' @importFrom dplyr mutate filter ungroup group_by bind_rows select left_join
#' @importFrom magrittr %>%
#' @export
get_ag_prices <- function() {
  var <- scenario <- region <- sector <- value <- unit_conv <- year <- NULL

  ag_prices_clean <<-
    getQuery(prj, "prices by sector") %>%
    bind_rows(ag_prices_wld) %>%
    left_join(filter_variables(gcamreport::ag_prices_map, "ag_prices_clean"),
      by = c("sector"), relationship = "many-to-many"
    ) %>%
    filter(!is.na(var)) %>%
    group_by(scenario, region, sector) %>%
    mutate(value = value * unit_conv / value[year == 2005]) %>%
    ungroup() %>%
    # do the mean by variable
    group_by(scenario, region, var, year, ) %>%
    summarise(value = mean(value)) %>%
    ungroup() %>%
    # rearrange dataset
    select(all_of(gcamreport::long_columns))
}


# carbon price
# sectoral CO2 prices are the same for all scenarios except for d_rap
# CO2 prices are global except for d_delfrag

# calculate co2 price for all scenarios except for d_rap and d_delfrag

#' get_price_var_tmp
#'
#' Get price variables to compute carbon price.
#' @keywords internal internal tmp process
#' @return price_var global variable
#' @importFrom magrittr %>%
#' @export
get_price_var_tmp <- function() {
  price_var <<-
    unique(filter_variables(gcamreport::co2_market_frag_map, "price_var")$var)
}



#' filter_data_regions
#'
#' Filter the desired regions of some data with "regions" column
#' @keywords internal internal tmp process
#' @return data containing only the desired regions
#' @importFrom dplyr filter
#' @importFrom magrittr %>%
#' @export
filter_data_regions <- function(data) {
  region <- NULL

  if (!(identical(desired_regions, "All"))) {
    data <- data %>%
      filter(region %in% desired_regions)
  }

  return(data)
}


#' get_regions_tmp
#'
#' Get regions to compute carbon price.
#' @keywords internal internal tmp process
#' @return regions global variable
#' @importFrom magrittr %>%
#' @export
get_regions_tmp <- function() {
  CO2_market_filteredReg <- filter_data_regions(gcamreport::CO2_market)
  regions.global <<-
    unique(CO2_market_filteredReg$region)
}


#' get_regions_weight_tmp
#'
#' Get regions weights to compute carbon price.
#' @keywords internal internal tmp process
#' @return region_weight global variable
#' @importFrom dplyr mutate filter ungroup group_by select
#' @importFrom magrittr %>%
#' @export
get_regions_weight_tmp <- function() {
  var <- scenario <- year <- value <- NULL

  # for scenarios w/ different regional carbon prices, weigh regional price by final energy to get global CO2 price
  region_weight <<-
    fe_sector_clean %>%
    filter(var == "Final Energy") %>%
    group_by(scenario, year) %>%
    mutate(weight = value / sum(value)) %>%
    ungroup() %>%
    select(-value, -var)
}


#' get_co2_price_global_tmp
#'
#' Get global co2 price.
#' @keywords internal co2 tmp
#' @return co2_price_global & regions global variable
#' @importFrom tidyr expand_grid
#' @importFrom tibble tibble as_tibble
#' @importFrom rgcam getQuery
#' @importFrom dplyr mutate filter select left_join
#' @importFrom magrittr %>%
#' @export
get_co2_price_global_tmp <- function() {
  market <- value <- co2_price_global_pre <- regions <- NULL

  co2_price_global_pre <-
    getQuery(prj, "CO2 prices") %>%
    filter(market == "globalCO2")

  if (nrow(co2_price_global_pre) > 1) {
    co2_price_global <<-
      as_tibble(co2_price_global_pre) %>%
      mutate(value = value / gcamreport::convert$conv_C_CO2 * gcamreport::convert$conv_90USD_10USD) %>%
      mutate(market = gsub("global", "", market)) %>%
      left_join(filter_variables(gcamreport::co2_market_frag_map, "co2_price_global"), by = "market", multiple = "all") %>%
      filter(value != 0) %>%
      expand_grid(tibble(region = regions.global)) %>%
      select(all_of(gcamreport::long_columns))
  } else {
    co2_price_global <<- NULL
  }
}


#' get_co2_price_share
#'
#' Get co2 price between CO2 and CO2_ETS. If only one CO2 type present, share = 1;
#' otherwise, each type has the share corresponding to the last historical year
#' @keywords internal co2 tmp
#' @return co2_price_share_byreg and co2_price_share_bysec global variables
#' @importFrom tidyr expand_grid pivot_wider
#' @importFrom rgcam getQuery
#' @importFrom tibble tibble
#' @importFrom dplyr mutate filter select right_join if_else distinct
#' @importFrom magrittr %>%
#' @export
get_co2_price_share <- function() {
  var <- year <- region <- value <- . <- sector <-
    CO2_ETS <- CO2 <- scenario <- share_CO2_ETS <- NULL

  co2_price_share_byreg <<- co2_clean %>%
    filter(
      var == "Emissions|CO2|Energy and Industrial Processes",
      year == gcamreport::last_historical_year
    ) %>%
    rbind(co2_ets_byreg %>%
      filter(
        var == "Emissions|CO2_ETS|Energy and Industrial Processes",
        year == gcamreport::last_historical_year
      )) %>%
    mutate(var = if_else(var == "Emissions|CO2|Energy and Industrial Processes", "CO2", "CO2_ETS")) %>%
    pivot_wider(names_from = "var", values_from = "value")

  # if CO2_ETS is not present, create a NA column
  if (!("CO2_ETS" %in% colnames(co2_price_share_byreg))) {
    co2_price_share_byreg <<- co2_price_share_byreg %>%
      mutate(CO2_ETS = NA)
  }

  if (nrow(co2_price_share_byreg) < 1) {
    co2_price_clean <<-
      expand_grid(tibble(scenario = unique(fe_sector_clean$scenario))) %>%
      expand_grid(tibble(year = unique(fe_sector_clean$year))) %>%
      expand_grid(tibble(region = c(unique(fe_sector_clean$region), "Global"))) %>%
      mutate(value = 0) %>%
      select(all_of(gcamreport::long_columns))
  }

  co2_price_share_byreg <<- co2_price_share_byreg %>%
    mutate(CO2_ETS = if_else(is.na(CO2_ETS), 0, CO2_ETS)) %>%
    mutate(share_CO2_ETS = CO2_ETS / CO2) %>%
    select(scenario, region, year, share_CO2_ETS)

  co2_price_share_bysec <<- co2_clean %>%
    filter(year == gcamreport::last_historical_year) %>%
    rbind(co2_ets_bysec %>%
      filter(year == gcamreport::last_historical_year)) %>%
    # select only reported sectors and do a right join, so that all sectors are present,
    # even if the value is NA
    right_join(expand.grid(
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
      region = unique(co2_clean$region),
      scenario = unique(co2_clean$scenario),
      year = 2015
    )) %>%
    mutate(value = if_else(is.na(value), 0, value)) %>%
    # compute the share
    mutate(
      sector = sub(".*\\|([^|]+)$", "\\1", var),
      ghg = sub("Emissions\\|([^|]+)\\|.*$", "\\1", var)
    ) %>%
    select(-var) %>%
    distinct(.) %>%
    pivot_wider(names_from = "ghg", values_from = "value") %>%
    mutate(CO2_ETS, if_else(is.na(CO2_ETS), 0, CO2_ETS)) %>%
    mutate(share_CO2_ETS = CO2_ETS / CO2) %>%
    # if the share is > 1, set it to 1 (seems that "biomass" is not accounted in the CO2 emissions query)
    mutate(share_CO2_ETS = if_else(share_CO2_ETS > 1, 1, share_CO2_ETS)) %>%
    select(scenario, region, year, sector, share_CO2_ETS)
}



#' get_co2_price_fragmented_tmp
#'
#' Get fragmented co2 price.
#' @keywords internal co2 tmp
#' @return co2_price_fragmented global variable
#' @importFrom tidyr complete nesting pivot_wider
#' @importFrom rgcam getQuery
#' @importFrom stats complete.cases
#' @importFrom dplyr mutate filter ungroup group_by select left_join if_else
#' @importFrom magrittr %>%
#' @export
get_co2_price_fragmented_tmp <- function() {
  market <- Units <- regions <- year <- value <- market_adj <- scenario <-
    region <- CO2 <- CO2_ETS <- share_CO2_ETS <- sector <- var <- NULL

  co2_price_fragmented_pre <<-
    getQuery(prj, "CO2 prices") %>%
    filter(!grepl("LUC", market)) %>%
    filter(market != "globalCO2") %>%
    filter(Units == "1990$/tC")

  if (nrow(co2_price_fragmented_pre) > 1) {
    CO2_market_filteredReg <- filter_data_regions(gcamreport::CO2_market)

    co2_price_fragmented <<-
      co2_price_fragmented_pre %>%
      left_join(CO2_market_filteredReg, by = c("market"), multiple = "all") %>%
      filter(complete.cases(.)) %>%
      mutate(value = value / gcamreport::convert$conv_C_CO2 * gcamreport::convert$conv_90USD_10USD) %>%
      mutate(
        market_adj = "CO2",
        market_adj = if_else(grepl("ETS", market), "CO2_ETS", market_adj),
        market_adj = if_else(grepl("CO2BLD", market), "CO2BLD", market_adj),
        market_adj = if_else(grepl("CO2IND", market), "CO2_ETS", market_adj),
        market_adj = if_else(grepl("CO2TRAN", market), "CO2TRAN", market_adj)
      ) %>%
      # consider the value sum of by market (sum CO2_ETS coming from ETS and CO2IND)
      group_by(Units, scenario, year, market, region) %>%
      mutate(value = sum(value)) %>%
      ungroup() %>%
      # apply the share between CO2 and CO2_ETS
      select(-market) %>%
      pivot_wider(names_from = "market_adj", values_from = "value") %>%
      mutate(across(5:length(colnames(.)), ~ ifelse(is.na(.), 0, .))) %>%
      left_join(
        co2_price_share_bysec %>%
          select(-year),
        by = c("scenario", "region")
      )

    if (!"CO2_ETS" %in% names(co2_price_fragmented)) {
      co2_price_fragmented <<- co2_price_fragmented %>%
        mutate(CO2_ETS = 0)
    }

    co2_price_fragmented <<- co2_price_fragmented %>%
      mutate(value = CO2 + CO2_ETS * share_CO2_ETS) %>%
      select(Units, scenario, year, region, value, CO2, CO2_ETS, share_CO2_ETS, sector) %>%
      left_join(filter_variables(gcamreport::co2_market_frag_map, "co2_price_fragmented"), by = "sector", multiple = "all") %>%
      filter(complete.cases(.)) %>%
      complete(nesting(scenario, var, year, market, Units), region = regions.global, fill = list(value = 0)) %>%
      select(all_of(gcamreport::long_columns))
  } else {
    co2_price_fragmented <<- NULL
  }
}


#' get_co2_price
#'
#' Get co2 price.
#' @keywords internal co2
#' @return co2_price_clean global variable
#' @importFrom tidyr complete nesting expand_grid
#' @importFrom tibble tibble
#' @importFrom dplyr mutate select bind_rows
#' @importFrom magrittr %>%
#' @export
get_co2_price <- function() {
  co2_price_clean_pre <- region <- var <- year <- NULL

  co2_price_clean_pre <-
    bind_rows(co2_price_global, co2_price_fragmented)

  if (nrow(co2_price_clean_pre) < 1) {
    co2_price_clean <<-
      tibble(var = unique(filter_variables(gcamreport::co2_market_frag_map, "co2_price_clean")$var)) %>%
      expand_grid(tibble(scenario = unique(fe_sector_clean$scenario))) %>%
      expand_grid(tibble(year = unique(fe_sector_clean$year))) %>%
      expand_grid(tibble(region = c(unique(fe_sector_clean$region), "Global"))) %>%
      mutate(value = 0) %>%
      select(all_of(gcamreport::long_columns))
  } else {
    co2_price_clean <<- co2_price_clean_pre %>%
      complete(nesting(region, var, year), scenario = unique(fe_sector_clean$scenario), fill = list(value = 0)) %>%
      select(all_of(gcamreport::long_columns))
  }
}


#' get_gov_revenue_sector
#'
#' Get overall carbon revenue.
#' @keywords internal revenue
#' @return gov_revenue_sector global variable
#' @importFrom dplyr mutate filter select left_join
#' @importFrom magrittr %>%
#' @export
get_gov_revenue_sector <- function() {
  var <- sector <- value <- emiss <- NULL

  gov_revenue_sector <<-
    co2_clean %>%
    mutate(
      sector = ifelse(var == "Emissions|CO2|Energy|Demand|Industry", "Carbon|Demand|Industry", NA),
      sector = ifelse(var == "Emissions|CO2|Energy|Demand|Residential and Commercial", "Carbon|Demand|Buildings", sector),
      sector = ifelse(var == "Emissions|CO2|Energy|Demand|Transportation", "Carbon|Demand|Transport", sector),
      sector = ifelse(var == "Emissions|CO2|Energy|Supply|Electricity", "Carbon|Supply", sector),
      emiss = value
    ) %>%
    select(-var, -value) %>%
    filter(!is.na(sector)) %>%
    left_join(
      co2_price_clean %>%
        mutate(
          sector = ifelse(var == "Price|Carbon|Energy|Demand|Industry", "Carbon|Demand|Industry", NA),
          sector = ifelse(var == "Price|Carbon|Energy|Demand|Residential and Commercial", "Carbon|Demand|Buildings", sector),
          sector = ifelse(var == "Price|Carbon|Energy|Demand|Transportation", "Carbon|Demand|Transport", sector),
          sector = ifelse(var == "Price|Carbon|Energy|Supply", "Carbon|Supply", sector)
        ) %>%
        select(-var),
      by = c("scenario", "region", "sector", "year")
    ) %>%
    mutate(
      value = value * emiss,
      var = paste0("Revenue|Government|Tax|", sector)
    )
}

#' get_gov_revenue
#'
#' Get overall carbon revenue.
#' @keywords internal revenue
#' @return gov_revenue_all global variable
#' @importFrom dplyr mutate ungroup group_by summarise select bind_rows
#' @importFrom magrittr %>%
#' @export
get_gov_revenue <- function() {
  scenario <- region <- year <- value <- NULL

  gov_revenue_clean <<-
    gov_revenue_sector %>%
    bind_rows(gov_revenue_sector %>%
      group_by(scenario, region, year) %>%
      summarise(value = sum(value, na.rm = T)) %>%
      ungroup() %>%
      mutate(var = "Revenue|Government|Tax|Carbon", )) %>%
    mutate(value = value / 1000) %>%
    select(all_of(gcamreport::long_columns))
}


#' get_prices_subsector
#'
#' Get energy prices - primary, secondary, and final
#' @keywords internal prices
#' @return prices_subsector global variable
#' @importFrom tidyr replace_na
#' @importFrom rgcam getQuery
#' @importFrom dplyr mutate filter select left_join bind_rows
#' @importFrom magrittr %>%
#' @export
get_prices_subsector <- function() {
  Units <- subsector <- var <- PrimaryFuelCO2Coef.name <- PrimaryFuelCO2Coef <- NULL

  prices_subsector <<-
    getQuery(prj, "prices by sector") %>%
    select(-Units) %>%
    left_join(filter_variables(gcamreport::energy_prices_map, "prices_subsector") %>%
      filter(is.na(subsector)) %>%
      unique(), by = c("sector"), multiple = "all") %>%
    bind_rows(getQuery(prj, "costs by subsector") %>%
      left_join(
        filter_variables(gcamreport::energy_prices_map, "prices_subsector") %>%
          unique(),
        by = c("sector", "subsector")
      )) %>%
    filter(!is.na(var)) %>%
    # read in carbon content in kg C per GJ -> convert to tC per GJ
    left_join(
      gcamreport::carbon_content %>%
        filter(grepl("biomass", PrimaryFuelCO2Coef.name)),
      by = c("region", "sector" = "PrimaryFuelCO2Coef.name")
    ) %>%
    mutate(PrimaryFuelCO2Coef = PrimaryFuelCO2Coef / 1000) %>%
    replace_na(list(PrimaryFuelCO2Coef = 0))
}


#' get_energy_price_fragmented
#'
#' Get energy prices fragmented, join by region since price is different.
#' @keywords internal prices process
#' @return energy_price_fragmented global variable
#' @importFrom tidyr replace_na
#' @importFrom rgcam getQuery
#' @importFrom dplyr mutate filter select left_join
#' @importFrom magrittr %>%
#' @export
get_energy_price_fragmented <- function() {
  var <- market <- scenario <- region <- year <-
    value <- PrimaryFuelCO2Coef <- price_C <- unit_conv <- NULL

  CO2_market_filteredReg <- filter_data_regions(gcamreport::CO2_market)

  energy_price_fragmented <<-
    prices_subsector %>%
    filter(!is.na(var)) %>%
    left_join(getQuery(prj, "CO2 prices") %>%
      filter(!grepl("LUC", market)) %>%
      left_join(CO2_market_filteredReg, by = c("market"), multiple = "all") %>%
      select(scenario, region, year, price_C = value), by = c("scenario", "region", "year")) %>%
    replace_na(list(price_C = 0)) %>%
    # remove carbon price (subsidy) 1990$/tC from biomass 1975$/GJ
    mutate(
      price_C = PrimaryFuelCO2Coef * price_C * gcamreport::convert$conv_90USD_10USD,
      value = value * unit_conv * gcamreport::convert$conv_75USD_10USD + price_C
    ) %>%
    select(all_of(gcamreport::long_columns))
}

#' get_total_revenue
#'
#' Compute total revenue: total production * global price.
#' @keywords internal revenue
#' @return total_revenue global variable
#' @importFrom rgcam getQuery
#' @importFrom dplyr mutate filter ungroup group_by summarise left_join rename
#' @importFrom magrittr %>%
#' @export
get_total_revenue <- function() {
  resource <- scenario <- year <- value <- sector <- resource_price <-
    total_production <- NULL

  total_revenue <<-
    getQuery(prj, "resource production") %>%
    filter(resource %in% c("coal", "crude oil", "natural gas")) %>%
    group_by(scenario, resource, year) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    rename(total_production = value) %>%
    left_join(
      getQuery(prj, "prices by sector") %>%
        filter(sector %in% c("regional coal", "regional oil", "regional natural gas")) %>%
        mutate(
          resource = ifelse(sector == "regional coal", "coal", NA),
          resource = ifelse(sector == "regional oil", "crude oil", resource),
          resource = ifelse(sector == "regional natural gas", "natural gas", resource)
        ) %>%
        rename(resource_price = value) %>%
        group_by(scenario, resource, year) %>%
        summarise(resource_price = mean(resource_price, na.rm = T)) %>%
        ungroup(),
      by = c("scenario", "year", "resource")
    ) %>%
    mutate(
      total_production = total_production * gcamreport::convert$GJ_to_EJ,
      total_revenue = total_production * resource_price * gcamreport::convert$conv_75USD_10USD
    )
}

#' get_regional_emission
#'
#' Compute regional nonCO2 emission: regional production * nonCO2 coef.
#' @keywords internal nonco2
#' @return regional_emission global variable
#' @importFrom tidyr spread
#' @importFrom rgcam getQuery
#' @importFrom dplyr mutate filter select left_join rename
#' @importFrom magrittr %>%
#' @export
get_regional_emission <- function() {
  year <- Non.CO2 <- emiss.coef <- CH4 <- N2O <- CH4.coef <- N2O.coef <-
    region <- resource <- value <- regional_production <- NULL

  regional_emission <<- suppressWarnings(
    gcamreport::nonCO2_content %>%
      filter(year == 2005) %>%
      spread(Non.CO2, emiss.coef) %>%
      rename(
        CH4.coef = CH4,
        N2O.coef = N2O
      ) %>%
      mutate(
        CH4.coef = CH4.coef / 1000000,
        N2O.coef = N2O.coef / 1000000
      ) %>%
      select(region, resource, CH4.coef, N2O.coef) %>%
      left_join(getQuery(prj, "resource production") %>%
        filter(resource %in% c("coal", "crude oil", "natural gas")) %>%
        rename(regional_production = value), by = c("region", "resource")) %>%
      mutate(
        regional_CH4emission = regional_production * CH4.coef * gcamreport::convert$GJ_to_EJ,
        regional_N2Oemission = regional_production * N2O.coef * gcamreport::convert$GJ_to_EJ
      )
  )
}


#' get_energy_price_tmp
#'
#' Bind regional oil, gas, coal prices with other energy prices
#' @keywords internal price tmp
#' @return energy_price global variable
#' @importFrom dplyr select
#' @importFrom magrittr %>%
#' @export
get_energy_price_tmp <- function() {
  energy_price <<-
    energy_price_fragmented %>%
    select(all_of(gcamreport::long_columns))
}


#' get_energy_price
#'
#' Compute final energy price
#' @keywords internal price process
#' @return energy_price_clean global variable
#' @importFrom dplyr mutate filter ungroup group_by summarise select bind_rows
#' @importFrom magrittr %>%
#' @export
get_energy_price <- function() {
  var <- scenario <- region <- value <- year <- NULL

  energy_price_clean <<-
    energy_price %>%
    filter(grepl("Residential\\|Electricity", var) |
      grepl("Residential\\|Gas", var) |
      grepl("Primary Energy\\|Coal", var) |
      grepl("Primary Energy\\|Biomass", var) |
      grepl("Primary Energy\\|Gas", var) |
      grepl("Primary Energy\\|Oil", var) |
      grepl("Secondary Energy\\|Electricity", var) |
      grepl("Secondary Energy\\|Gas", var) |
      grepl("Secondary Energy\\|Liquids", var) |
      grepl("Secondary Energy\\|Liquids\\|Biomass", var) |
      grepl("Secondary Energy\\|Liquids\\|Oil", var) |
      grepl("Secondary Energy\\|Solids", var) |
      grepl("Secondary Energy\\|Solids\\|Biomass", var) |
      grepl("Secondary Energy\\|Solids\\|Coal", var)) %>%
    mutate(var = paste(var, "Index", sep = "|")) %>%
    group_by(scenario, region, var) %>%
    mutate(value = value / value[year == 2020]) %>%
    ungroup() %>%
    select(all_of(gcamreport::long_columns)) %>%
    bind_rows(energy_price)

  # sum by energy price var
  energy_price_clean <<-
    energy_price_clean %>%
    group_by(scenario, region, var, year) %>%
    mutate(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    unique()

  # average mean for global primary energy price
  energy_price_clean <<-
    energy_price_clean %>%
    group_by(scenario, var, year) %>%
    summarise(value = mean(value, na.rm = T)) %>%
    ungroup() %>%
    mutate(region = "World") %>%
    bind_rows(energy_price_clean)
}

# Capacity and investment
# ==============================================================================================
# electricity capacity
## first check global existing capacity from IEA, calculate cf for existing capacity

#' get_cf_iea_tmp
#'
#' Calculate cf for existing capacity checking global existing capacity from IEA.
#' @keywords internal capacity process tmp
#' @return cf_iea global variable
#' @importFrom tidyr complete nesting
#' @importFrom dplyr mutate filter ungroup group_by summarise select left_join
#' @importFrom magrittr %>%
#' @export
get_cf_iea_tmp <- function() {
  year <- scenario <- var <- value <- period <- variable <- EJ <- cf <- technology <- NULL

  cf_rgn_filteredReg <- filter_data_regions(gcamreport::cf_rgn)

  cf_iea <<-
    elec_gen_tech_clean %>%
    filter(year == 2020, scenario == unique(elec_gen_tech_clean$scenario)[1]) %>%
    group_by(var) %>%
    summarise(EJ = sum(value, na.rm = T)) %>%
    ungroup() %>%
    left_join(
      gcamreport::iea_capacity %>%
        filter(period == 2020, scenario == "Current Policies Scenario") %>%
        mutate(
          variable = gsub("Capacity\\|Electricity\\|CSP", "Capacity\\|Electricity\\|Solar\\|CSP", variable),
          variable = gsub("Capacity\\|Electricity\\|Biomass", "Capacity\\|Electricity\\|Biomass\\|w/o CCS", variable),
          variable = gsub("Capacity\\|Electricity\\|Coal", "Capacity\\|Electricity\\|Coal\\|w/o CCS", variable),
          variable = gsub("Capacity\\|Electricity\\|Gas", "Capacity\\|Electricity\\|Gas\\|w/o CCS", variable),
          variable = gsub("Capacity\\|Electricity\\|Oil", "Capacity\\|Electricity\\|Oil\\|w/o CCS", variable),
          variable = gsub("Capacity", "Secondary Energy", variable)
        ),
      by = c("var" = "variable")
    ) %>%
    mutate(
      cf = EJ / (value * gcamreport::convert$hr_per_yr * gcamreport::convert$EJ_to_GWh),
      cf = replace(cf, cf > 1, 0.99)
    ) %>%
    filter(!is.na(cf), !var %in% c("Secondary Energy|Electricity", "Secondary Energy|Electricity|Non-Biomass Renewables")) %>%
    left_join(filter_variables(gcamreport::capacity_map, "cf_iea"), by = "var", multiple = "all") %>%
    select(technology, cf) %>%
    mutate(region = "USA", vintage = 2020) %>%
    complete(nesting(technology, cf),
      vintage = c(1990, seq(2005, 2020, by = 5)),
      region = unique(cf_rgn_filteredReg$region)
    )
}

#' get_elec_cf_tmp
#'
#' Calculate future capacity using GCAM
#' @keywords internal capacity process tmp
#' @return elec_cf global variable
#' @importFrom tidyr complete nesting
#' @importFrom dplyr mutate filter ungroup group_by select left_join bind_rows
#' @importFrom magrittr %>%
#' @export
get_elec_cf_tmp <- function() {
  technology <- X2100 <- cf <- region <- stub.technology <- year <-
    capacity.factor <- cf.rgn <- vintage <- NULL

  cf_rgn_filteredReg <- filter_data_regions(gcamreport::cf_rgn)
  cf_iea_filteredReg <- filter_data_regions(cf_iea)

  elec_cf <-
    gcamreport::cf_gcam %>%
    select(technology, cf = X2100) %>%
    mutate(region = "USA", vintage = 2025) %>%
    complete(nesting(technology, cf),
      vintage = seq(2025, 2100, by = 5),
      region = unique(cf_rgn_filteredReg$region)
    ) %>%
    # first, replace regional cf for wind and solar
    left_join(
      cf_rgn_filteredReg %>%
        select(region, technology = stub.technology, vintage = year, cf.rgn = capacity.factor),
      by = c("technology", "vintage", "region")
    ) %>%
    mutate(cf = replace(cf, !is.na(cf.rgn), cf.rgn[!is.na(cf.rgn)])) %>%
    # second, use iea capacity consistent cf for existing vintage
    bind_rows(cf_iea_filteredReg) %>%
    complete(nesting(technology, region), vintage = c(1990, seq(2005, 2100, by = 5))) %>%
    group_by(technology, region) %>%
    mutate(cf = approx_fun(vintage, cf, rule = 2)) %>%
    ungroup() %>%
    filter(!technology %in% c("existing coal", "add CCS retrofit"))

  elec_cf <<- filter_data_regions(elec_cf)
}

#' get_elec_capacity_tot
#'
#' Calculate electricity total capacity
#' @keywords internal capacity process
#' @return elec_capacity_tot_clean global variable
#' @importFrom tidyr complete nesting separate
#' @importFrom rgcam getQuery
#' @importFrom dplyr mutate filter ungroup group_by summarise select left_join bind_rows
#' @importFrom magrittr %>%
#' @export
get_elec_capacity_tot <- function() {
  output <- technology <- vintage <- var <- unit_conv <-
    scenario <- region <- year <- value <- gw <- NULL

  elec_capacity_tot_clean <<- suppressWarnings(
    getQuery(prj, "elec gen by gen tech and cooling tech and vintage") %>%
      filter(!output %in% c("electricity", "elect_td_bld")) %>%
      separate(technology, into = c("technology", "vintage"), sep = ",") %>%
      mutate(
        vintage = as.integer(sub("year=", "", vintage)),
        output = gsub("elec_", "", output)
      ) %>%
      group_by(scenario, region, technology = output, vintage, year) %>%
      summarise(value = sum(value, na.rm = T)) %>%
      ungroup() %>%
      bind_rows(getQuery(prj, "elec gen by gen tech and cooling tech and vintage") %>%
        filter(output %in% c("electricity", "elect_td_bld")) %>%
        separate(technology, into = c("technology", "vintage"), sep = ",") %>%
        mutate(vintage = as.integer(sub("year=", "", vintage))) %>%
        group_by(scenario, region, technology, vintage, year) %>%
        summarise(value = sum(value, na.rm = T)) %>%
        ungroup()) %>%
      left_join(elec_cf, by = c("region", "technology", "vintage")) %>%
      mutate(EJ = value) %>%
      conv_EJ_GW() %>%
      group_by(scenario, region, technology, year) %>%
      summarise(value = sum(gw, na.rm = T)) %>%
      ungroup() %>%
      left_join(filter_variables(gcamreport::capacity_map, "elec_capacity_tot_clean") %>% select(-output), by = c("technology"), multiple = "all") %>%
      filter(!is.na(var)) %>%
      mutate(
        value = value * unit_conv,
        var = sub("Secondary Energy", "Capacity", var)
      ) %>%
      group_by(scenario, region, var, year) %>%
      summarise(value = sum(value, na.rm = T)) %>%
      ungroup() %>%
      complete(nesting(scenario, region, year),
        var = unique(var),
        fill = list(value = 0)
      ) %>%
      select(all_of(gcamreport::long_columns))
  )
}

#' get_elec_capacity_add_tmp
#'
#' Calculate added total capacity
#' @keywords internal capacity process tmp
#' @return elec_capacity_add global variable
#' @importFrom tidyr separate
#' @importFrom rgcam getQuery
#' @importFrom dplyr mutate filter ungroup group_by summarise left_join bind_rows
#' @importFrom magrittr %>%
#' @export
get_elec_capacity_add_tmp <- function() {
  output <- technology <- vintage <- scenario <-
    region <- year <- value <- gw <- EJ <- NULL

  elec_capacity_add <<- suppressWarnings(
    getQuery(prj, "elec gen by gen tech and cooling tech and vintage") %>%
      filter(!output %in% c("electricity", "elect_td_bld")) %>%
      separate(technology, into = c("technology", "vintage"), sep = ",") %>%
      mutate(
        vintage = as.integer(sub("year=", "", vintage)),
        output = gsub("elec_", "", output)
      ) %>%
      group_by(scenario, region, technology = output, vintage, year) %>%
      summarise(value = sum(value, na.rm = T)) %>%
      ungroup() %>%
      bind_rows(getQuery(prj, "elec gen by gen tech and cooling tech and vintage") %>%
        filter(output %in% c("electricity", "elect_td_bld")) %>%
        separate(technology, into = c("technology", "vintage"), sep = ",") %>%
        mutate(vintage = as.integer(sub("year=", "", vintage))) %>%
        group_by(scenario, region, technology, vintage, year) %>%
        summarise(value = sum(value, na.rm = T)) %>%
        ungroup()) %>%
      filter(year == vintage, year > 2015) %>%
      group_by(scenario, region, technology, year) %>%
      summarise(value = sum(value, na.rm = T)) %>%
      ungroup() %>%
      # use GCAM cf for capacity additions
      left_join(elec_cf,
        by = c("region", "technology", "year" = "vintage")
      ) %>%
      # use average annual additions
      mutate(EJ = value / 5) %>%
      conv_EJ_GW() %>%
      group_by(scenario, region, technology, year) %>% #
      summarise(GW = sum(gw, na.rm = T), EJ = sum(EJ, na.rm = T)) %>%
      ungroup()
  )
}

#' get_elec_capacity_add
#'
#' Calculate added total capacity
#' @keywords internal capacity process
#' @return elec_capacity_add_clean global variable
#' @importFrom tidyr complete nesting
#' @importFrom dplyr mutate filter ungroup group_by summarise select left_join bind_rows
#' @importFrom magrittr %>%
#' @export
get_elec_capacity_add <- function() {
  output <- EJ <- value <- var <- GW <- unit_conv <- scenario <- vintage <-
    region <- year <- gw <- output <- technology <- scenario <- NULL

  # check calculations for this
  elec_capacity_add_clean <<-
    elec_capacity_add %>%
    left_join(gcamreport::capacity_map %>% select(-output), by = c("technology"), multiple = "all") %>%
    filter(!var %in% c(
      "Secondary Energy|Electricity|Hydro",
      "Secondary Energy|Electricity|Storage Capacity"
    )) %>%
    mutate(
      value = GW * unit_conv,
      var = sub("Secondary Energy", "Capacity Additions", var)
    ) %>%
    bind_rows(elec_capacity_add %>%
      left_join(gcamreport::capacity_map %>% select(-output), by = c("technology"), multiple = "all") %>%
      filter(var == "Secondary Energy|Electricity|Storage Capacity") %>%
      mutate(
        value = GW * 8760, # multiply by # of hours in a year
        var = sub("Secondary Energy", "Capacity Additions", var)
      )) %>%
    filter_variables(variable = "elec_capacity_add_clean") %>%
    group_by(scenario, region, var, year) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    complete(nesting(scenario, region, year),
      var = unique(var),
      fill = list(value = 0)
    ) %>%
    select(all_of(gcamreport::long_columns))
}


#' get_elec_capital
#'
#' Calculate electricity capital
#' @keywords internal capital process
#' @return elec_capital_clean global variable
#' @importFrom tidyr complete nesting
#' @importFrom dplyr mutate filter ungroup group_by summarise select left_join bind_rows
#' @importFrom magrittr %>%
#' @export
get_elec_capital <- function() {
  sector <- subsector <- technology <- year <- capital.overnight <- output <-
    var <- value <- unit_conv <- scenario <- region <- NULL

  cf_rgn_filteredReg <- filter_data_regions(gcamreport::cf_rgn)

  # Capital costs from GCAM in $1975/kw -> convert to $2010/kw
  elec_capital <-
    gcamreport::capital_gcam %>%
    mutate(scenario = scenarios.global[1]) %>%
    select(-sector) %>%
    # gw * 10e6 * $/kw / 10e9 = bill$
    mutate(value = capital.overnight * gcamreport::convert$conv_75USD_10USD) %>%
    left_join(filter_variables(gcamreport::elec_gen_map, "elec_capital") %>% select(-output),
      by = c("subsector", "technology"),
      relationship = "many-to-many"
    )

  elec_capital_clean <<-
    filter_data_regions(elec_capital) %>%
    filter(!is.na(var), var != "Secondary Energy|Electricity|Electricity Storage") %>%
    mutate(
      value = value * unit_conv,
      var = sub("Secondary Energy", "Capital Cost", var)
    ) %>%
    group_by(scenario, region, var, year) %>%
    summarise(value = mean(value, na.rm = T)) %>%
    ungroup() %>%
    select(all_of(gcamreport::long_columns))

  # average mean for global tech capital cost
  elec_capital_clean <<-
    elec_capital_clean %>%
    group_by(scenario, var, year) %>%
    summarise(value = mean(value, na.rm = T)) %>%
    ungroup() %>%
    mutate(region = "World") %>%
    bind_rows(elec_capital_clean)
}

#' get_elec_investment
#'
#' Calculate electricity investment = annual capacity additions * capital costs
#' @keywords internal capital process
#' @return elec_investment_clean global variable
#' @importFrom dplyr mutate filter ungroup group_by summarise select left_join
#' @importFrom magrittr %>%
#' @export
get_elec_investment <- function() {
  year <- region <- var <- capital.overnight <- technology <-
    GW <- scenario <- output <- value <- unit_conv <- NULL

  elec_investment_clean <<-
    # Electricity investment = annual capacity additions * capital costs
    elec_capacity_add %>%
    left_join(
      gcamreport::capital_gcam %>%
        mutate(
          capital.overnight = replace(capital.overnight, technology == "wind_storage", capital.overnight[technology == "wind"] * .484),
          capital.overnight = replace(capital.overnight, technology == "CSP_storage", 760 * gcamreport::convert$conv_19USD_75USD),
          capital.overnight = replace(capital.overnight, technology == "PV_storage", capital.overnight[technology == "PV"] * .518)
        ),
      by = c("technology", "year", "region")
    ) %>%
    # gw * 10e6 * $/kw / 10e9 = bill$
    mutate(value = GW * capital.overnight / 1000 * gcamreport::convert$conv_75USD_10USD) %>%
    left_join(filter_variables(gcamreport::elec_gen_map, "elec_investment_clean") %>% select(-output), by = c("technology"), relationship = "many-to-many") %>%
    filter(!is.na(var)) %>%
    mutate(
      value = value * unit_conv,
      var = sub("Secondary Energy", "Investment|Energy Supply", var)
    ) %>%
    group_by(scenario, region, var, year) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    select(all_of(gcamreport::long_columns))
}

#' get_transmission_invest
#'
#' Calculate investment electricity transmission and distribution
#' scale 2020 number - average of other model results from Mcollion et al. 2018
#' Need to convert 2015 to 2010 $
#' @keywords internal investment process
#' @return transmission_invest_clean global variable
#' @importFrom dplyr mutate filter group_by summarise select left_join
#' @importFrom magrittr %>%
#' @export
get_transmission_invest <- function() {
  Region <- Variable <- year <- value <- var <- scenario <- share <-
    region <- invest <- rate <- NULL

  transmission2020 <-
    gcamreport::investment %>%
    filter(Region == "World", Variable == "Energy Supply|Electricity|Transmission and Distribution", year == 2020) %>%
    mutate(value = value * gcamreport::convert$conv_15USD_10USD) %>%
    summarise(value = mean(value, na.rm = T)) %>%
    unlist()

  transmission_invest2020 <-
    elec_capacity_add_clean %>%
    filter(var == "Capacity Additions|Electricity", year == 2020) %>%
    group_by(scenario) %>%
    mutate(
      share = value / sum(value, na.rm = T),
      invest = share * transmission2020
    )

  transmission_invest_clean <<-
    elec_capacity_add_clean %>%
    filter(var == "Capacity Additions|Electricity") %>%
    group_by(scenario, region) %>%
    mutate(rate = value / value[year == 2020]) %>%
    left_join(
      transmission_invest2020 %>%
        select(scenario, region, invest),
      by = c("scenario", "region")
    ) %>%
    mutate(value = rate * invest, var = "Investment|Energy Supply|Electricity|Transmission and Distribution") %>%
    select(all_of(gcamreport::long_columns))
}


#' get_CCS_invest
#'
#' Calculate CSS investment
#' @keywords internal investment process
#' @return CCS_invest_clean global variable
#' @importFrom dplyr mutate filter group_by summarise select left_join
#' @importFrom magrittr %>%
#' @export
get_CCS_invest <- function() {
  Region <- Variable <- year <- value <- var <- scenario <- share <- region <-
    invest <- rate <- NULL

  # use last available year if 2040 is not present in the data
  yy <- ifelse(max(unique(co2_sequestration_clean$year)) >= 2040, 2040, max(unique(co2_sequestration_clean$year)))

  CCS2040 <-
    gcamreport::investment %>%
    filter(Region == "World", Variable == "CCS", year == yy) %>%
    mutate(value = value * gcamreport::convert$conv_15USD_10USD) %>%
    summarise(value = mean(value, na.rm = T)) %>%
    unlist()

  CCS_invest2040 <-
    co2_sequestration_clean %>%
    filter(var == "Carbon Sequestration|CCS", year == yy) %>%
    group_by(scenario) %>%
    mutate(
      share = value / sum(value, na.rm = T),
      invest = share * CCS2040
    )

  CCS_invest_clean <<-
    co2_sequestration_clean %>%
    filter(var == "Carbon Sequestration|CCS") %>%
    group_by(scenario, region) %>%
    mutate(rate = value / value[year == yy]) %>%
    left_join(
      CCS_invest2040 %>%
        select(scenario, region, invest),
      by = c("scenario", "region")
    ) %>%
    mutate(value = rate * invest, var = "Investment|Energy Supply|CO2 Transport and Storage") %>%
    select(all_of(gcamreport::long_columns))
}

#' get_resource_investment
#'
#' Calculate investment of resource production
#' @keywords internal investment process
#' @return resource_investment_clean global variable
#' @importFrom tidyr separate spread
#' @importFrom rgcam getQuery
#' @importFrom dplyr mutate filter ungroup group_by summarise select left_join bind_rows if_else
#' @importFrom magrittr %>%
#' @export
get_resource_investment <- function() {
  resource <- technology <- vintage <- year <- scenario <- region <- rate <-
    value <- Region <- Variable <- fuel <- production <- share <- invest <- NULL

  # Investment of resource production
  resource_addition <- suppressWarnings(
    getQuery(prj, "resource production by tech and vintage") %>%
      filter(resource %in% c("coal", "natural gas", "crude oil", "unconventional oil")) %>%
      separate(technology, into = c("technology", "vintage"), sep = ",") %>%
      mutate(vintage = as.integer(sub("year=", "", vintage))) %>%
      filter(year > 2010) %>%
      mutate(
        resource = sub("crude oil", "oil", resource),
        resource = sub("unconventional oil", "oil", resource)
      ) %>%
      group_by(scenario, resource, region, year) %>%
      summarise(production = sum(value, na.rm = T)) %>%
      ungroup()
  )

  # scale 2015 number - average of other model results from Mcollion et al. 2018
  extraction2015 <-
    gcamreport::investment %>%
    filter(Region == "World", Variable == "Extraction and Conversion - Fossil Fuels", year == 2015) %>%
    mutate(value = value * gcamreport::convert$conv_15USD_10USD) %>%
    summarise(value = mean(value, na.rm = T)) %>%
    unlist()

  extraction2020 <-
    gcamreport::investment %>%
    filter(Region == "World", Variable == "Extraction and Conversion - Fossil Fuels", year == 2020) %>%
    mutate(value = value * gcamreport::convert$conv_15USD_10USD) %>%
    summarise(value = mean(value, na.rm = T)) %>%
    unlist()

  resource_investment2015 <-
    resource_addition %>%
    filter(year == 2015) %>%
    left_join(
      getQuery(prj, "regional primary energy prices") %>%
        mutate(fuel = sub("regional ", "", fuel)),
      by = c("scenario", "region", "year", "resource" = "fuel")
    ) %>%
    mutate(value = production * value) %>%
    group_by(scenario, resource, region) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    group_by(scenario) %>%
    mutate(
      share = value / sum(value, na.rm = T),
      invest = share * extraction2015
    ) %>%
    ungroup()

  resource_investment2020 <-
    resource_addition %>%
    filter(year == 2020) %>%
    left_join(
      getQuery(prj, "regional primary energy prices") %>%
        mutate(fuel = sub("regional ", "", fuel)),
      by = c("scenario", "region", "year", "resource" = "fuel")
    ) %>%
    mutate(value = production * value) %>%
    group_by(scenario, resource, region) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    group_by(scenario) %>%
    mutate(
      share = value / sum(value, na.rm = T),
      invest = share * extraction2020
    ) %>%
    ungroup()

  reg <- if_else(desired_regions == "All" | "China" %in% desired_regions, "China", desired_regions[1])[1]

  resource_investment <-
    resource_addition %>%
    filter(year != 2020) %>%
    group_by(scenario, resource) %>%
    mutate(rate = production / production[year == 2015 & region == reg]) %>%
    ungroup() %>%
    left_join(
      resource_investment2015 %>%
        filter(region == reg) %>%
        select(scenario, resource, invest),
      by = c("scenario", "resource")
    ) %>%
    bind_rows(resource_addition %>%
      filter(year == 2020) %>%
      group_by(scenario, resource) %>%
      mutate(rate = production / production[region == reg]) %>%
      ungroup() %>%
      left_join(
        resource_investment2020 %>%
          filter(region == reg) %>%
          select(scenario, resource, invest),
        by = c("scenario", "resource")
      )) %>%
    mutate(
      value = invest * rate,
      resource = sub("coal", "Coal", resource),
      resource = sub("natural gas", "Gas", resource),
      resource = sub("oil", "Oil", resource),
      var = paste0("Investment|Energy Supply|Extraction|", resource)
    ) %>%
    select(all_of(gcamreport::long_columns))

  resource_investment_clean <<-
    resource_investment %>%
    group_by(scenario, region, year) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    mutate(var = "Investment|Energy Supply|Extraction|Fossil") %>%
    select(all_of(gcamreport::long_columns)) %>%
    bind_rows(resource_investment) # %>% group_by(scenario, var, year) %>% summarise(value = sum(value)) %>% spread(year, value)
}

#########################################################################
#                        BIND TO TEMPLATE FUNCTIONS                     #
#########################################################################
#' do_bind_results
#'
#' Bind and save results
#' @keywords internal process
#' @return Save results in an output file.
#' @importFrom tidyr complete nesting replace_na pivot_wider
#' @importFrom dplyr bind_rows mutate filter group_by summarise ungroup distinct inner_join left_join select rename
#' @importFrom magrittr %>%
#' @export
do_bind_results <- function() {
  region <- var <- scenario <- year <- value <- . <- na.omit <- Region <- Variable <- NULL

  vars <- variables.global[variables.global$required == TRUE, "name"]
  GCAM_DATA <-
    bind_rows(lapply(vars, function(x) get(x))) %>%
    mutate(
      region = gsub("Global", "World", region),
      region = gsub("global", "World", region)
    )

  # Calculate global total
  GCAM_DATA_WORLD <-
    GCAM_DATA %>%
    filter(
      region != "World", # excl. Temperature|Forcing|Concentration
      # excl. price and costs variables - already calculated global average
      !grepl("Price|Capital Cost", var)
    ) %>%
    group_by(scenario, year, var) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    mutate(region = "World")

  GCAM_DATA_wGLOBAL <-
    GCAM_DATA_WORLD %>%
    bind_rows(GCAM_DATA %>% filter(!(region == "World" & var %in% unique(GCAM_DATA_WORLD$var)))) %>%
    complete(nesting(scenario, region, var), year = gcamreport::reporting_years) %>%
    replace_na(list(value = 0)) %>%
    distinct(.)

  # filter to final_year.global
  GCAM_DATA_wGLOBAL <- GCAM_DATA_wGLOBAL %>% filter(year <= final_year.global)


  report <<-
    gcamreport::template %>%
    inner_join(
      GCAM_DATA_wGLOBAL %>%
        na.omit() %>%
        pivot_wider(names_from = "year", values_from = "value"),
      by = c("Variable" = "var"), multiple = "all"
    ) %>%
    #  left_join(reporting_scen %>% select(GCAM_scenario, Scenario),
    #            by = c("scenario" = "GCAM_scenario")) %>%
    rename(Region = region) %>%
    #  rename(Model = ?..Model) %>%
    rename(Scenario = scenario) %>%
    select(all_of(reporting_columns.global)) %>%
    filter(!is.na(Region)) # Drop variables we don't report

  if (!(length(desired_variables) == 1 && desired_variables == "All")) {
    report <<- report %>%
      filter(Variable %in% desired_variables)
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
#' @importFrom dplyr left_join group_by summarise ungroup mutate if_else rename filter
#' @importFrom magrittr %>%
#' @export
do_check_trade <- function() {
  scenario <- resource <- year <- production <- demand <- check <- NULL

  if (exists("energy_trade_prod")) {
    # check global total is zero
    trade <- energy_trade_prod %>%
      left_join(energy_trade_supply, by = c("scenario", "resource", "region", "year")) %>%
      group_by(scenario, resource, year) %>%
      summarise(
        production = sum(production, na.rm = T),
        demand = sum(demand, na.rm = T)
      ) %>%
      ungroup() %>%
      mutate(
        diff = (production - demand) / production,
        check = if_else(abs(diff) > 1, "ERROR", "OK")
      )

    check_trade_summary <- trade %>%
      rename("percentual_diff_between_production_and_demand" = "diff")

    if (nrow(trade %>% filter(check == "ERROR")) > 0) {
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
#' @return Verification message indicating if the process was successful.
#' @importFrom here here
#' @importFrom tidyr gather unnest
#' @importFrom dplyr rename filter mutate select left_join if_else group_by summarise ungroup
#' @import ggplot2
#' @importFrom magrittr %>%
#' @export
do_check_vetting <- function() {
  year <- value <- Model <- Variable <- Unit <- Scenario <- Region <-
    adj_var <- adj_var2 <- region <- Range <- variable <- value_vet <-
    unit_vet <- check <- type <- NULL

  # Check vetting results from SM
  final_data_long_check <- report %>%
    gather(year, value, -Model, -Variable, -Unit, -Scenario, -Region) %>%
    rename(
      region = Region,
      variable = Variable
    ) %>%
    filter(Scenario == scenarios.global[1]) %>%
    mutate(year = as.integer(year))

  check_vet <- gcamreport::global_vet_values %>%
    select(variable = adj_var, adj_var2, region, year, value, unit, range = Range) %>%
    rename(
      unit_vet = unit,
      value_vet = value
    ) %>%
    left_join(final_data_long_check, by = c("variable", "region", "year")) %>%
    unnest(value) %>%
    mutate(value = if_else(grepl("Traditional", variable), value * -1, value)) %>%
    select(Scenario, variable = adj_var2, region, year, value, unit = Unit, value_vet, unit_vet, range) %>%
    # Adjust for Solar&Wind and biomass
    group_by(Scenario, variable, region, year, unit, unit_vet, range) %>%
    summarise(
      value = sum(value),
      value_vet = mean(value_vet)
    ) %>%
    ungroup() %>%
    # mutate(unit_vet = as.character(unit_vet)) %>%
    mutate(
      value_vet = if_else(unit_vet == "bcm", value_vet * gcamreport::convert$bcm_to_EJ, value_vet),
      unit_vet = if_else(unit_vet == "bcm", "EJ/yr", unit_vet)
    ) %>%
    mutate(
      diff = (value / value_vet) - 1,
      check = if_else(abs(diff) > range, "ERROR", "OK")
    )

  check_vet_summary <- check_vet %>%
    rename(
      "computed_value" = "value",
      "expected_value (vetting)" = "value_vet",
      "confidance_range" = "range"
    )

  ## plot
  check_vet_plot <- check_vet %>%
    select(-year, -range, -diff, -check, -unit_vet) %>%
    gather(type, value, -variable, -Scenario, -unit, -region)

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
  if (!dir.exists(file.path(here(), "output"))) {
    dir.create(file.path(here(), "output"))
  }
  if (!dir.exists(file.path(here(), "output", "figure"))) {
    dir.create(file.path(here(), "output", "figure"))
  }
  ggsave(file.path(here(), "output", "figure", "vetting.tiff"), last_plot(), "tiff", dpi = 200)

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
#' @keywords internal template
#' @importFrom utils write.csv
#' @importFrom usethis use_data
#' @importFrom here here
#' @importFrom dplyr filter mutate select anti_join if_else
#' @return Updated template as .rda and as csv in the inst/extdata folder
update_template <- function() {
  as_output <- Internal_variable <- Variable <- NULL

  data <- merge(gcamreport::template,
    data.frame(Variable = unique(report$Variable)) %>%
      mutate("as_output" = TRUE),
    by = "Variable", all = TRUE
  ) %>%
    select(colnames(gcamreport::template), as_output) %>%
    # if the variable was not given as output, set NA as Internal_variable
    mutate(Internal_variable = if_else(is.na(as_output), NA, Internal_variable)) %>%
    # if there is a variable given as output but not recorded as so, print it
    mutate(
      Variables_outputed_but_not_recorded =
        if_else(is.na(Internal_variable) & !is.na(as_output), Variable, NA)
    )
  print(paste0("New variables that can be reported: ", unique(data$Variables_outputed_but_not_recorded)))
  print(paste0(
    "Old variables that are no longer reported: ",
    anti_join(
      gcamreport::template %>%
        filter(!is.na(Internal_variable) & Internal_variable != "") %>%
        select(Variable),
      data %>%
        filter(!is.na(Internal_variable) & Internal_variable != "") %>%
        select(Variable)
    )
  ))

  template <- data %>%
    select(colnames(gcamreport::template))

  write.csv(template,
    file = file.path(here(), "inst/extdata", "template/reporting_template.csv"),
    row.names = FALSE
  )
  use_data(template, overwrite = T)
}

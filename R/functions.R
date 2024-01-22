library(usethis)
library(magrittr)
options(dplyr.summarise.inform = FALSE)

#########################################################################
#                           ANCILLARY FUNCTIONS                         #
#########################################################################


#' start_with_pattern
#'
#' Return the vector elements starting with the specified parameters
#' @param vector: vector to check
#' @param pattern: pattern to consider
#' @return subvector of `vector` whose elements start with `pattern`
#' @export
start_with_pattern = function(vector, pattern) {
  matching_elements = vector[substr(vector, 1, nchar(pattern)) == pattern]
  return(matching_elements)
}


#' filter_loading_regions
#'
#' Filter the desired regions of a GCAM project
#' @param data: dataframe to be filtered
#' @param desired_regions: desired regions to consider. By default, 'All'. Otherwise, specify a vector with all the considered regions.
#' To know all possible regions, run `available_regions()`. ATTENTION: the considered regions will make up "World".
#' In case the project dataset needs to be created, it will be produced with only the specified regions.
#' @param variable: dataset variable information
#' @return filtered dataframe
#' @export
filter_loading_regions <- function (data, desired_regions = 'All', variable) {
  if (!(length(desired_regions) == 1 && desired_regions == 'All')) {
    # the variable CO2 prices does not contain "region", but "markets". Now we
    # filter for all market items that do not contain the desired regions
    if (variable %in% c('CO2 prices','supply of all markets')) {
      pattern <- paste(c("CO2", "airCO2", "nonCO2", "CO2_FUG", "CO2 removal",
                         "H2", "Exports"), collapse = "|")
      # desired_regions special case: if some "EU" region is present, consider the
      # "EU" region to compute CO2 price
      if (any(grepl("^EU", desired_regions))) {
        desired_regions_tmp = c(desired_regions,'EU')
      } else {
        desired_regions_tmp = desired_regions
      }

      data = data %>%
        dplyr::mutate(region = sapply(strsplit(as.character(market), pattern),
                                      function(x) x[1])) %>%
        dplyr::filter(region %in% desired_regions_tmp) %>%
        dplyr::select(-region)
    } else if (!(variable %in% c('CO2 concentrations','global mean temperature',
                          'total climate forcing'))) {
      data = data %>%
        dplyr::filter(region %in% desired_regions)
    }
  }

  return(data)
}


#' filter_variables
#'
#' Filter the desired regions of a GCAM project
#' @param data: dataframe to be filtered
#' @param variable: variable that requires this data
#' @return filtered dataframe
#' @export
filter_variables = function(data, variable) {

  if (variable %in% variables[variables$required == TRUE,]$name) {
    if (!(length(desired_variables) == 1 && desired_variables == 'All')) {
      if ('var' %in% colnames(data)) {
        data = data %>%
          dplyr::filter(var %in% desired_variables)
      }
    }
  }

  return(invisible(data))
}


#' conv_ghg_co2e
#'
#' Covert GHG to CO2e
#' @param data: dataset
#' @export
conv_ghg_co2e <- function (data) {

  # Ancillary function
  # GHG emission conversion
  F_GASES <- c("C2F6", "CF4", "HFC125", "HFC134a", "HFC245fa", "SF6", "HFC143a", "HFC152a", "HFC227ea", "HFC23", "HFC236fa", "HFC32", "HFC365mfc", "HFC43",
               "HFC245fa", "HFC43-10")
  GHG_gases <- c("CH4", "N2O", F_GASES, "CO2", "CO2LUC")

  data %>%
    tidyr::separate(ghg, into = c("variable", "sector"), sep = "_", fill = "right") %>%
    dplyr::filter(variable %in% GHG_gases) %>%
    dplyr::left_join(GWP_adjuster, by = c("variable" = "GHG_gases")) %>%
    dplyr::mutate(value = value * GWP, Units = "CO2e") %>%
    dplyr::select(-GWP) %>%
    return()
}


#' conv_EJ_GW
#'
#' Covert EJ to GW
#' @param data: dataset
#' @param cf: conversion factor
#' @param EJ: EJ amount
#' @export
conv_EJ_GW <- function (data, cf, EJ){
  # Elec related conversions
  hr_per_yr <- 8760
  EJ_to_GWh <- 0.0000036

  data %>%
    dplyr::mutate(gw = EJ / (cf * hr_per_yr * EJ_to_GWh))
}

#' approx_fun
#'
#' Interpolation function
#' @param year: year to consider
#' @param value: values to extrapolate from
#' @param rule: number of points to extrapolate
#' @export
approx_fun <- function(year, value, rule = 1) {
  if(rule == 1 | rule == 2) {
    tryCatch(stats::approx(as.vector(year), value, rule = rule, xout = year)$y,
             error = function(e) NA)

  } else {
    stop("Use fill_exp_decay_extrapolate!")
  }
}


#########################################################################
#                         LOAD QUERIES FUNCTIONS                        #
#########################################################################

# Scioeconomics
# ==============================================================================================
#' get_population
#'
#' Get population query and change units to [million].
#' @keywords population rgcam::getQuery
#' @return population_clean global variable
#' @export
get_population = function() {
  population_clean <<-
    rgcam::getQuery(prj, "population by region") %>%
    dplyr::mutate(value = value * conv_thousand_million,
                  var = "Population") %>%
    dplyr::select(all_of(long_columns))
}


#' get_gdp_ppp
#'
#' Get GDP (PPP) query, compute regional GDP, and change units to [10USD].
#' @keywords GDP rgcam::getQuery
#' @return GDP_PPP_clean global variable
#' @export
get_gdp_ppp = function() {
  GDP_PPP_clean <<-
    rgcam::getQuery(prj, "GDP per capita PPP by region") %>%
    dplyr::left_join(population_clean %>% dplyr::rename(pop_mill = value), by = c("scenario", "region", "year")) %>%
    dplyr::mutate(value = value * pop_mill * conv_90USD_10USD,
                  var = "GDP|PPP") %>%
    dplyr::select(all_of(long_columns))
}


#' get_gdp_mer
#'
#' Get GDP (MER) query and change units to [10USD].
#' @keywords GDP rgcam::getQuery
#' @return GDP_MER_clean global variable
#' @export
get_gdp_mer = function() {
  GDP_MER_clean <<-
    rgcam::getQuery(prj, "GDP MER by region") %>%
    dplyr::mutate(value = value * conv_million_billion * conv_90USD_10USD,
                  var = "GDP|MER") %>%
    dplyr::select(all_of(long_columns))
}

# Climate and emissions
# ==============================================================================================
#' get_forcing
#'
#' Get World's forcing query.
#' @keywords forcing rgcam::getQuery
#' @return forcing_clean global variable
#' @export
get_forcing = function() {
  forcing_clean <<-
    rgcam::getQuery(prj, "total climate forcing") %>%
    dplyr::filter(year %in% GCAM_years) %>%
    dplyr::mutate(var = "Forcing", region = "World") %>%
    dplyr::select(all_of(long_columns))
}


#' get_temperature
#'
#' Get World's mean temperature query.
#' @keywords temperature rgcam::getQuery
#' @return global_temp_clean global variable
#' @export
get_temperature = function() {
  global_temp_clean <<-
    rgcam::getQuery(prj, "global mean temperature") %>%
    dplyr::filter(year %in% GCAM_years) %>%
    dplyr::mutate(var = "Temperature|Global Mean", region = "World") %>%
    dplyr::select(all_of(long_columns))
}


#' get_co2_concentration
#'
#' Get World's CO2 concentration query.
#' @keywords co2 rgcam::getQuery
#' @return co2_concentration_clean global variable
#' @export
get_co2_concentration = function() {
  co2_concentration_clean <<-
    rgcam::getQuery(prj, "CO2 concentrations") %>%
    dplyr::filter(year %in% GCAM_years) %>%
    dplyr::mutate(var = "Concentration|CO2", region = "World") %>%
    dplyr::select(all_of(long_columns))
}


#' get_co2
#'
#' Get World's CO2 emissions query.
#' @keywords co2 rgcam::getQuery
#' @return co2_clean global variable
#' @export
get_co2 = function() {
  co2_clean <<-
    tibble::as_tibble(rgcam::getQuery(prj, "CO2 emissions by sector (no bio) (excluding resource production)")) %>%
    dplyr::left_join(filter_variables(co2_sector_map, 'co2_clean'), by = "sector", multiple = "all") %>%
    dplyr::mutate(value = value * unit_conv) %>%
    dplyr::group_by(scenario, region, year, var) %>% #
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(all_of(long_columns))
}


#' get_co2_ets
#'
#' Get World's CO2 ETS emissions query.
#' @keywords co2 rgcam::getQuery
#' @return co2_ets_by reg and co2_ets_bysec global variables
#' @export
get_co2_ets = function() {
  co2_ets_byreg <<-
    tibble::as_tibble(rgcam::getQuery(prj, "nonCO2 emissions by region")) %>%
    dplyr::filter(ghg == 'CO2_ETS') %>%
    # change units to CO2 equivalent and set the variable
    dplyr::mutate(value = value * CO2_equivalent,
                  var = 'Emissions|CO2_ETS|Energy and Industrial Processes') %>%
    dplyr::select(all_of(long_columns))
  co2_ets_bysec <<-
    tibble::as_tibble(rgcam::getQuery(prj, "nonCO2 emissions by sector (excluding resource production)")) %>%
    dplyr::filter(ghg == 'CO2_ETS') %>%
    # change units to CO2 equivalent and group by sector
    dplyr::left_join(filter_variables(co2_ets_sector_map, 'co2_ets_bysec'), by = "sector", multiple = "all") %>%
    dplyr::mutate(value = value * unit_conv) %>%
    dplyr::group_by(scenario, region, year, var) %>% #
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(all_of(long_columns))
}


# Get CO2 emissions by tech, to break out ships vs rail vs aviation
# and to get Emissions|CO2|Energy| Coal vs Gas vs Oil.
# Must create CO2 emissions by tech (no bio) output first to be consistent. There is no query for this

# Apply bio negative emissions by joining by sector and by sector (no bio) and finding share of non-bio emissions

#' get_nonbio_tmp
#'
#' Get no bio CO2 emissions query by sector.
#' @keywords co2 rgcam::getQuery
#' @return nonbio_share global variable
#' @export
get_nonbio_tmp = function() {
  nonbio_share <<-
    rgcam::getQuery(prj, "CO2 emissions by sector (excluding resource production)") %>%
    dplyr::left_join(rgcam::getQuery(prj, "CO2 emissions by sector (no bio) (excluding resource production)"), by = c("region", "scenario", "year", "sector", "Units")) %>%
    dplyr::mutate(value.y = dplyr::if_else(is.na(value.y), value.x, value.y),
                  percent = value.y/value.x) %>%
    dplyr::select(-value.x, -value.y)
}


#' get_co2_tech_nobio_tmp
#'
#' Get no bio CO2 emissions query by sector and techonolgy.
#' @keywords co2 rgcam::getQuery tmp
#' @return co2_tech_nobio global variable
#' @export
get_co2_tech_nobio_tmp = function() {
  co2_tech_nobio <<-
    rgcam::getQuery(prj, "CO2 emissions by tech (excluding resource production)") %>%
    dplyr::left_join(nonbio_share, by = c("region", "scenario", "year", "sector", "Units")) %>%
    dplyr::mutate(value = value*percent) %>%
    dplyr::select(-percent)
}


#' get_co2_tech_emissions_tmp
#'
#' Get no bio CO2 emissions query by sector, subsector, and techonolgy.
#' @keywords co2 rgcam::getQuery tmp
#' @return co2_tech_emissions global variable
#' @export
get_co2_tech_emissions_tmp = function() {
  co2_tech_emissions <<-
    co2_tech_nobio %>%
    dplyr::left_join(filter_variables(co2_tech_map, 'co2_tech_emissions'), by = c("sector", "subsector", "technology"), multiple = "all")  %>%
    dplyr::filter(!is.na(var)) %>%
    dplyr::mutate(value = value * unit_conv) %>%
    dplyr::group_by(scenario, region, year, var) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(all_of(long_columns))
}


# Iron and Steel Emissions: for Emissions|CO2|Coal, Gas, Oil
# Find which input has the greatest share for each IRONSTL tech (between coal, gas, oil)

#' get_iron_steel_map
#'
#' Get iron and steel emissions.
#' @keywords iron steel rgcam::getQuery
#' @return iron_steel_map global variable
#' @export
get_iron_steel_map = function() {
  iron_steel_map <<-
    rgcam::getQuery(prj, "industry final energy by tech and fuel") %>%
    dplyr::filter(sector == "iron and steel",
                  input %in% c("wholesale gas", "refined liquids industrial", "delivered coal")) %>%
    dplyr::mutate(max = max(value),
                  save = dplyr::if_else(value == max, 1, 0)) %>%
    dplyr::filter(save == 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(-save, -max, -Units, -scenario, -value)
}


#' get_co2_iron_steel
#'
#' Get iron and steel CO2 emissions.
#' @keywords iron steel co2 rgcam::getQuery
#' @return co2_tech_ironsteel global variable
#' @export
get_co2_iron_steel = function() {
  co2_tech_ironsteel <<-
    co2_tech_nobio %>% #Using redistributed bio version
    dplyr::filter(sector == "iron and steel") %>%
    dplyr::left_join(filter_variables(iron_steel_map, 'co2_tech_ironsteel'), by = c("sector", "subsector", "technology", "year", "region")) %>%
    dplyr::mutate(input = stringr::str_replace(input, "wholesale gas", "Emissions|CO2|Energy|Gas"),
                  input = stringr::str_replace(input, "refined liquids industrial", "Emissions|CO2|Energy|Oil"),
                  input = stringr::str_replace(input,	"delivered coal", "Emissions|CO2|Energy|Coal")) %>%
    dplyr::rename(var = input) %>%
    dplyr::mutate(value = value * conv_C_CO2) %>%
    dplyr::group_by(scenario, region, year, var) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    na.omit() %>%
    dplyr::select(all_of(long_columns))
}


#' get_lu_co2
#'
#' Get land use CO2 emissions.
#' @keywords lu co2 rgcam::getQuery
#' @return LU_carbon_clean global variable
#' @export
get_lu_co2 = function() {
  LU_carbon_clean <<-
    # Land use CO2
    rgcam::getQuery(prj, "LUC emissions by region") %>%
    # dplyr::filter(scenario == "Reference") %>%
    dplyr::filter(year %in% GCAM_years) %>%
    dplyr::group_by(scenario, region, year) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(value = value * conv_C_CO2,
                  var = "Emissions|CO2|AFOLU") %>%
    dplyr::select(all_of(long_columns)) %>%
    # dplyr::filter(scenario==scen1) %>%
    dplyr::group_by(scenario,var,year)
}


#' get_co2_emissions
#'
#' Combine CO2 emission queries.
#' @keywords co2 process
#' @return co2_emissions_clean global variable
#' @export
get_co2_emissions = function() {
  co2_emissions_clean <<-
    dplyr::bind_rows(co2_clean, LU_carbon_clean, co2_tech_emissions) %>%
    dplyr::group_by(scenario, region, year, var) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(all_of(long_columns))
}

#' get_total_co2_emissions
#'
#' Compute total CO2 emission.
#' @keywords co2 process
#' @return tot_co2_clean global variable
#' @export
get_total_co2_emissions = function() {
  tot_co2_clean <<-
    dplyr::bind_rows(co2_clean %>% dplyr::filter(var == "Emissions|CO2|Energy and Industrial Processes"), LU_carbon_clean) %>%
    dplyr::group_by(scenario, region, year) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(var = "Emissions|CO2") %>%
    dplyr::select(all_of(long_columns))
}


#' get_nonco2_emissions
#'
#' Get non CO2 emissions query.
#' @keywords nonco2 rgcam::getQuery
#' @return nonco2_clean global variable
#' @export
get_nonco2_emissions = function() {
  nonco2_clean <<-
    rgcam::getQuery(prj, "nonCO2 emissions by sector (excluding resource production)") %>%
    dplyr::left_join(filter_variables(nonco2_emis_sector_map, 'nonco2_clean'), by = c("ghg", "sector"), multiple = "all") %>%
    dplyr::bind_rows(rgcam::getQuery(prj, "nonCO2 emissions by resource production") %>%
                       dplyr::left_join(filter_variables(nonco2_emis_resource_map, 'nonco2_clean'), by = c("ghg", "resource"), multiple = "all")) %>%
    dplyr::mutate(value = value * unit_conv) %>%
    dplyr::group_by(scenario, region, year, var) %>% #
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(all_of(long_columns))
}

#' get_fgas
#'
#' Compute F-Gases emissions.
#' @keywords f-gases process
#' @return f_gas_clean global variable
#' @export
get_fgas = function() {
  f_gas_clean <<-
    rgcam::getQuery(prj, "nonCO2 emissions by region") %>%
    dplyr::filter(!grepl("CO2_ETS", ghg)) %>%
    conv_ghg_co2e() %>%
    dplyr::filter(variable %in% F_GASES) %>%
    dplyr::group_by(scenario, region, year) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(var = "Emissions|F-Gases") %>%
    dplyr::select(all_of(long_columns))
}


#' get_ghg
#'
#' Get total GHG emissions.
#' @keywords ghg rgcam::getQuery
#' @return ghg_clean global variable
#' @export
get_ghg = function() {
  ghg_all <<-
    rgcam::getQuery(prj, "nonCO2 emissions by region") %>%
    dplyr::filter(!grepl("CO2_ETS", ghg)) %>%
    conv_ghg_co2e() %>%
    dplyr::filter(variable %in% GHG_gases) %>%
    dplyr::bind_rows(LU_carbon_clean) %>%
    dplyr::group_by(scenario, region, year) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(var = "Emissions|Kyoto Gases") %>%
    dplyr::select(all_of(long_columns))
}


#' get_ghg_sector
#'
#' Get sectorial GHG emissions.
#' @keywords ghg rgcam::getQuery
#' @return ghg_sector_clean global variable
#' @export
get_ghg_sector = function() {
  ghg_sector_clean <<-
    rgcam::getQuery(prj, "nonCO2 emissions by sector (excluding resource production)")  %>%
    dplyr::filter(!grepl("CO2", ghg), !grepl("CO2_ETS", ghg)) %>%
    dplyr::bind_rows(rgcam::getQuery(prj, "nonCO2 emissions by resource production") %>%
                       dplyr::rename(sector = resource) %>%
                       dplyr::select(-subresource)) %>%
    dplyr::bind_rows(rgcam::getQuery(prj, "CO2 emissions by sector (no bio) (excluding resource production)") %>%
                       dplyr::mutate(ghg = "CO2")) %>%
    dplyr::mutate(subsector = sector) %>%
    conv_ghg_co2e() %>%
    dplyr::filter(variable %in% GHG_gases) %>%
    dplyr::rename(ghg = variable) %>%
    dplyr::left_join(filter_variables(kyoto_sector_map, 'ghg_sector_clean'), relationship = "many-to-many") %>%
    dplyr::select(all_of(long_columns)) %>%
    dplyr::bind_rows(LU_carbon_clean %>%
                       dplyr::mutate(var = "Emissions|Kyoto Gases"),
                     LU_carbon_clean %>%
                       dplyr::mutate(var = "Emissions|Kyoto Gases|AFOLU")) %>%
    dplyr::group_by(scenario, region, var, year) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup()
}


#' get_co2_sequestration
#'
#' Get carbon sequestration.
#' @keywords co2 rgcam::getQuery
#' @return co2_sequestration_clean global variable
#' @export
get_co2_sequestration = function() {
  co2_sequestration_clean <<-
    rgcam::getQuery(prj, "CO2 sequestration by tech") %>%
    dplyr::left_join(filter_variables(carbon_seq_tech_map, 'co2_sequestration_clean'), by = c("sector", "technology"), multiple = "all") %>%
    tidyr::complete(tidyr::nesting(scenario,region, year),
                    var = unique(var),
                    fill = list(value = 0)) %>%
    dplyr::filter(!is.na(var)) %>% #, var!= "Carbon Sequestration|Feedstocks",var != "Carbon Sequestration|Feedstocks|Liquids") %>%
    dplyr::mutate(value = value * unit_conv) %>%
    dplyr::group_by(scenario, region, year, var) %>% #
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>% #tidyr::spread(year, value) -> d
    dplyr::select(all_of(long_columns))
}


# Agriculture and land use
# ==============================================================================================
#' get_ag_demand
#'
#' Get agricultural demand.
#' @keywords ag rgcam::getQuery
#' @return ag_demand_clean global variable
#' @export
get_ag_demand = function() {
  ag_demand_clean <<-
    dplyr::bind_rows(rgcam::getQuery(prj, "demand balances by crop commodity"),
                     rgcam::getQuery(prj, "demand balances by meat and dairy commodity")) %>%
    # Adjust OtherMeat_Fish
    dplyr::mutate(sector = dplyr::if_else(sector == "FoodDemand_NonStaples" & input == "OtherMeat_Fish", "OtherMeat_Fish", sector)) %>%
    dplyr::left_join(filter_variables(ag_demand_map, 'ag_demand_clean'), by = c("sector"), multiple = "all") %>%
    dplyr::filter(!is.na(var)) %>%
    dplyr::mutate(value = value * unit_conv) %>%
    dplyr::group_by(scenario, region, year, var) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(all_of(long_columns))
}


#' get_ag_production
#'
#' Get agricultural production.
#' @keywords ag rgcam::getQuery
#' @return ag_production_clean global variable
#' @export
get_ag_production = function() {
  ag_production_clean <<-
    rgcam::getQuery(prj, "ag production by crop type") %>%
    dplyr::filter(Units == "Mt") %>%
    # Forests produce in units of billion m3 and biomass produces in EJ. We'll need to find a conversion factor to include it
    # 1 m3 = .001 tons
    dplyr::mutate(var = "Agricultural Production") %>%
    dplyr::group_by(scenario, region, year, var) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(all_of(long_columns))
}


#' get_land
#'
#' Get land use area.
#' @keywords ag rgcam::getQuery
#' @return land_clean global variable
#' @export
get_land = function() {
  land_clean <<-
    rgcam::getQuery(prj, "aggregated land allocation") %>%
    dplyr::left_join(filter_variables(land_use_map, 'land_clean'), by = c("landleaf"), multiple = "all") %>%
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
#' @keywords energy rgcam::getQuery
#' @return primary_energy_clean global variable
#' @export
get_primary_energy = function() {
  primary_energy_clean <<-
    rgcam::getQuery(prj, "primary energy consumption with CCS by region (direct equivalent)") %>%
    dplyr::filter(!grepl("water", fuel),
                  Units == "EJ") %>%
    dplyr::left_join(filter_variables(primary_energy_map, 'primary_energy_clean'), by = c("fuel"), multiple = "all") %>%
    dplyr::filter(!is.na(var)) %>%
    dplyr::mutate(value = value * unit_conv) %>%
    dplyr::group_by(scenario, region, year, var) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup()  %>%
    tidyr::complete(tidyr::nesting(scenario,region, year),
                    var = unique(var),
                    fill = list(value = 0)) %>%
    dplyr::select(all_of(long_columns))
}


#' get_energy_trade_prod
#'
#' Get energy trade.
#' @keywords energy rgcam::getQuery
#' @return energy_trade_prod global variable
#' @export
get_energy_trade_prod = function() {
  energy_trade_prod <<-
    rgcam::getQuery(prj, "resource production") %>%
    dplyr::filter(Units == "EJ") %>%
    dplyr::filter(resource %in% c("coal", "natural gas", "crude oil", "unconventional oil")) %>%
    dplyr::mutate(resource = sub("crude oil", "oil", resource),
                  resource = sub("unconventional oil", "oil", resource)) %>%
    dplyr::group_by(scenario, resource, region, year) %>%
    dplyr::summarise(production = sum(value)) %>%
    dplyr::ungroup()
}


#' get_energy_trade_tmp
#'
#' Get energy trade supply. Query used to compute other variables.
#' @keywords energy rgcam::getQuery tmp
#' @return energy_trade_supply global variable
#' @export
get_energy_trade_tmp = function() {
  energy_trade_supply <<-
    rgcam::getQuery(prj, "supply of all markets") %>%
    dplyr::filter(grepl("regional coal", market) | grepl("regional natural gas", market) | grepl("regional oil", market)) %>%
    tidyr::separate(market, into = c("region", "resource"), sep = "regional ", fill = "right") %>%
    dplyr::filter(resource != "oilpalm", resource != "oilcrop") %>%
    dplyr::group_by(scenario, resource, region, year) %>%
    dplyr::summarise(demand = sum(value)) %>%
    dplyr::ungroup()
}


#' get_energy_trade
#'
#' Get energy trade supply.
#' @keywords energy rgcam::getQuery tmp
#' @return energy_trade_clean global variable
#' @export
get_energy_trade = function() {
  energy_trade_clean <<-
    energy_trade_prod %>%
    dplyr::left_join(energy_trade_supply, by = c("scenario", "resource", "region", "year")) %>%
    dplyr::mutate(value = production - demand,
                  resource = sub("coal", "Coal", resource),
                  resource = sub("natural gas", "Gas", resource),
                  resource = sub("oil", "Oil", resource),
                  var = paste0("Trade|Primary Energy|", resource, "|Volume")) %>%
    filter_variables(variable = 'energy_trade_clean') %>%
    dplyr::select(all_of(long_columns))
}

# Secondary Energy
# ==============================================================================================
#' get_elec_gen_tech
#'
#' Get electricity generation
#' @keywords electricity rgcam::getQuery
#' @return elec_gen_tech_clean global variable
#' @export
get_elec_gen_tech = function() {
  elec_gen_tech_clean <<-
    rgcam::getQuery(prj, "elec gen by gen tech") %>%
    dplyr::left_join(filter_variables(elec_gen_map, 'elec_gen_tech_clean'), by = c("output", "subsector", "technology"), multiple = "all") %>%
    dplyr::filter(!is.na(var)) %>%
    dplyr::mutate(value = value * unit_conv) %>%
    dplyr::group_by(scenario, region, year, var) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    tidyr::complete(tidyr::nesting(scenario,region, year),
                    var = unique(var),
                    fill = list(value = 0)) %>%
    dplyr::select(all_of(long_columns))
}


#' get_secondary_solids
#'
#' Get secondary solids
#' @keywords energy rgcam::getQuery
#' @return secondary_solids global variable
#' @export
get_secondary_solids = function() {
  secondary_solids <<-
    rgcam::getQuery(prj,"inputs by sector") %>%
    dplyr::filter(input %in% c("delivered biomass", "delivered coal")) %>%
    dplyr::group_by(scenario, region, year, input) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(var = ifelse(input == "delivered biomass", "Secondary Energy|Solids|Biomass",
                               "Secondary Energy|Solids|Coal")) %>%
    dplyr::bind_rows(rgcam::getQuery(prj,"inputs by sector") %>%
                       dplyr::filter(input %in% c("delivered biomass", "delivered coal")) %>%
                       dplyr::group_by(scenario, region, year) %>%
                       dplyr::summarise(value = sum(value, na.rm = T)) %>%
                       dplyr::ungroup() %>%
                       dplyr::mutate(var = "Secondary Energy|Solids")) %>%
    filter_variables(variable = 'secondary_solids') %>%
    dplyr::select(all_of(long_columns))
}


#' get_se_gen_tech
#'
#' Get other secondary energy production
#' @keywords energy rgcam::getQuery
#' @return se_gen_tech_clean global variable
#' @export
get_se_gen_tech = function() {
  se_gen_tech_clean <<-
    dplyr::bind_rows(rgcam::getQuery(prj, "gas production by tech"),
                     rgcam::getQuery(prj, "hydrogen production by tech"),
                     rgcam::getQuery(prj, "refined liquids production by tech")) %>%
    dplyr::left_join(filter_variables(se_gen_map, 'se_gen_tech_clean'), by = c("sector", "subsector", "technology"), multiple = "all") %>%
    dplyr::filter(!is.na(var)) %>%
    dplyr::mutate(value = value * unit_conv) %>%
    dplyr::group_by(scenario, region, year, var) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    tidyr::complete(tidyr::nesting(scenario,region, year),
                    var = unique(var),
                    fill = list(value = 0)) %>%
    dplyr::select(all_of(long_columns)) %>%
    dplyr::bind_rows(secondary_solids)
}


# Final Energy
# ==============================================================================================
# demand by sector by technology

#' get_fe_sector_tmp
#'
#' Get final energy demand by sector
#' @keywords energy rgcam::getQuery tmp
#' @return fe_sector global variable
#' @export
get_fe_sector_tmp = function() {
  fe_sector <<-
    rgcam::getQuery(prj, "final energy consumption by sector and fuel") %>%
    dplyr::left_join(filter_variables(final_energy_map, 'fe_sector'), by = c("sector", "input"), multiple = "all") %>%
    dplyr::filter(!is.na(var)) %>%
    dplyr::mutate(value = value * unit_conv) %>%
    dplyr::group_by(scenario, region, year, var) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    tidyr::complete(tidyr::nesting(scenario,region, year),
                    var = unique(var),
                    fill = list(value = 0)) %>%
    dplyr::select(all_of(long_columns))
}


#' get_fe_transportation_tmp
#'
#' Get mode-specific transport final energy to break out rail, ship, and domestic air.
#' @keywords energy rgcam::getQuery tmp
#' @return fe_transportation global variable
#' @export
get_fe_transportation_tmp = function() {
  fe_transportation <<-
    rgcam::getQuery(prj, "transport final energy by mode and fuel") %>%
    dplyr::left_join(filter_variables(transport_final_en_map, 'fe_transportation'), by = c("sector", "input", "mode"), multiple = "all") %>%
    dplyr::filter(!is.na(var)) %>%
    dplyr::mutate(value = value * unit_conv) %>%
    dplyr::group_by(scenario, region, year, var) %>%
    dplyr::summarise(value = sum(value,
                                 na.rm = T)) %>%
    dplyr::ungroup() %>%
    tidyr::complete(tidyr::nesting(scenario,region, year),
                    var = unique(var),
                    fill = list(value = 0)) %>%
    dplyr::select(all_of(long_columns))
}


#' get_fe_sector
#'
#' Compute final energy.
#' GPK 1/6/2022 - because some reporting categories are mapped from both the sector-level and subsector-level queries,
#' we need a step of aggregation here. For example, international and domestic air are both assigned to
#' aviation; international is mapped from the sector-level query, and domestic is mapped from the subsector (mode).
#' Without this step there would be duplicate entries with different data for the same reporting categories.
#' @keywords energy process
#' @return fe_sector_clean global variable
#' @export
get_fe_sector = function() {
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
#' @keywords energy rgcam::getQuery
#' @return energy_service_transportation_clean global variable
#' @export
get_energy_service_transportation = function() {
  energy_service_transportation_clean <<-
    rgcam::getQuery(prj, "transport service output by mode") %>%
    dplyr::left_join(filter_variables(transport_en_service, 'energy_service_transportation_clean'), by = c("sector", "mode"), multiple = "all") %>%
    dplyr::filter(!is.na(var)) %>%
    dplyr::mutate(value = value * unit_conv) %>%
    dplyr::group_by(scenario, region, year, var) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(all_of(long_columns))
}


#' get_energy_service_buildings
#'
#' Get ES buildings.
#' @keywords energy rgcam::getQuery
#' @return energy_service_buildings_clean global variable
#' @export
get_energy_service_buildings = function() {
  energy_service_buildings_clean <<-
    rgcam::getQuery(prj, "building floorspace") %>%
    dplyr::left_join(filter_variables(buildings_en_service, 'energy_service_buildings_clean'), by = c("building"), multiple = "all") %>%
    dplyr::filter(!is.na(var)) %>%
    dplyr::mutate(value = value * unit_conv) %>%
    dplyr::group_by(scenario, region, year, var) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(all_of(long_columns))
}



# industry production
# could add chemicals but they're in terms of EJ, need to also add cement

#' get_industry_production
#'
#' Get industry production.
#' @keywords energy rgcam::getQuery
#' @return industry_production_clean global variable
#' @export
get_industry_production = function() {
  industry_production_clean <<-
    rgcam::getQuery(prj, "industry primary output by sector") %>%
    dplyr::left_join(filter_variables(production_map, 'industry_production_clean'), by = c("sector")) %>%
    # dplyr::filter variables that are in terms of Mt
    dplyr::filter(var %in% c("Production|Cement", "Production|Steel", "Production|Non-ferrous metals"))%>%
    dplyr::group_by(scenario, region, var, year) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(all_of(long_columns))
}


# Prices
# ==============================================================================================
#' get_ag_prices_wld_tmp
#'
#' Get ag price index.
#' @keywords ag rgcam::getQuery tmp
#' @return ag_prices_wld global variable
#' @export
get_ag_prices_wld_tmp = function() {
  ag_prices_wld <<-
    rgcam::getQuery(prj, "prices by sector") %>%
    dplyr::left_join(filter_variables(ag_prices_map, 'ag_prices_wld'), by = c("sector")) %>%
    dplyr::filter(!is.na(var)) %>%
    dplyr::group_by(scenario, sector, year) %>%
    dplyr::summarise(value = mean(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(region = "World")
}

#' get_ag_prices
#'
#' Calculate average mean for ag global index
#' @keywords ag rgcam::getQuery
#' @return ag_prices_clean global variable
#' @export
get_ag_prices = function() {
  ag_prices_clean <<-
    rgcam::getQuery(prj, "prices by sector") %>%
    dplyr::bind_rows(ag_prices_wld) %>%
    dplyr::left_join(filter_variables(ag_prices_map, 'ag_prices_clean'), by = c("sector")) %>%
    dplyr::filter(!is.na(var)) %>%
    dplyr::group_by(scenario, region, sector) %>%
    dplyr::mutate(value = value * unit_conv / value[year == 2005]) %>%
    dplyr::ungroup() %>%
    dplyr::select(all_of(long_columns))
}


# carbon price
# sectoral CO2 prices are the same for all scenarios except for d_rap
# CO2 prices are global except for d_delfrag

#calculate co2 price for all scenarios except for d_rap and d_delfrag

#' get_price_var_tmp
#'
#' Get price variables to compute carbon price.
#' @keywords internal tmp process
#' @return price_var global variable
#' @export
get_price_var_tmp = function() {
  price_var <<-
    unique(filter_variables(co2_market_frag_map, 'price_var')$var)
}



#' filter_data_regions
#'
#' Filter the desired regions of some data with "regions" column
#' @keywords internal tmp process
#' @return data containing only the desired regions
#' @export
filter_data_regions = function(data) {
  if (!(length(desired_regions) == 1 && desired_regions == 'All')) {
    data = data %>%
      dplyr::filter(region %in% desired_regions)
  }

  return(data)
}


#' get_regions_tmp
#'
#' Get regions to compute carbon price.
#' @keywords internal tmp process
#' @return regions global variable
#' @export
get_regions_tmp = function() {
  CO2_market_filteredReg = filter_data_regions(CO2_market)
  regions <<-
    unique(CO2_market_filteredReg$region)
}


#' get_regions_weight_tmp
#'
#' Get regions weights to compute carbon price.
#' @keywords internal tmp process
#' @return region_weight global variable
#' @export
get_regions_weight_tmp = function() {
  # for scenarios w/ different regional carbon prices, weigh regional price by final energy to get global CO2 price
  region_weight <<-
    fe_sector_clean %>%
    dplyr::filter(var == "Final Energy") %>%
    dplyr::group_by(scenario, year) %>%
    dplyr::mutate(weight = value/sum(value)) %>%
    dplyr::ungroup() %>%
    dplyr::select(-value, -var)
}


#' get_co2_price_global_tmp
#'
#' Get global co2 price.
#' @keywords co2 rgcam::getQuery tmp
#' @return co2_price_global & regions global variable
#' @export
get_co2_price_global_tmp = function() {

  co2_price_global_pre <<-
    rgcam::getQuery(prj, "CO2 prices") %>%
    dplyr::filter(market == "globalCO2")

  if(nrow(co2_price_global_pre) > 1) {

    co2_price_global <<-
      tibble::as_tibble(co2_price_global_pre) %>%
      dplyr::mutate(value = value / conv_C_CO2 * conv_90USD_10USD) %>%
      dplyr::mutate(market = gsub("global", "", market)) %>%
      dplyr::left_join(filter_variables(co2_market_frag_map, 'co2_price_global'), by = "market", multiple = "all") %>%
      dplyr::filter(value != 0) %>%
      tidyr::expand_grid(tibble::tibble(region = regions)) %>%
      dplyr::select(all_of(long_columns))

  } else {

    co2_price_global <<- NULL

  }

}


#' get_co2_price_share
#'
#' Get co2 price between CO2 and CO2_ETS. If only one CO2 type present, share = 1;
#' otherwise, each type has the share corresponding to the last historical year
#' @keywords co2 rgcam::getQuery tmp
#' @return co2_price_share_byreg and co2_price_share_bysec global variables
#' @export
get_co2_price_share = function() {
  co2_price_share_byreg <<- co2_clean %>%
    dplyr::filter(var == 'Emissions|CO2|Energy and Industrial Processes',
                  year == last_historical_year) %>%
    rbind(co2_ets_byreg %>%
            dplyr::filter(var == 'Emissions|CO2_ETS|Energy and Industrial Processes',
                          year == last_historical_year)) %>%
    dplyr::mutate(var = dplyr::if_else(var == "Emissions|CO2|Energy and Industrial Processes", 'CO2', 'CO2_ETS')) %>%
    tidyr::pivot_wider(names_from = 'var', values_from = 'value')

  # if CO2_ETS is not present, create a NA column
  if (!("CO2_ETS" %in% colnames(co2_price_share_byreg))) {
    co2_price_share_byreg <<- co2_price_share_byreg %>%
      dplyr::mutate(CO2_ETS = NA)
  }

  if (nrow(co2_price_share_byreg) < 1) {
    co2_price_clean <<-
      tidyr::expand_grid(tibble::tibble(scenario = unique(fe_sector_clean$scenario))) %>%
      tidyr::expand_grid(tibble::tibble(year = unique(fe_sector_clean$year))) %>%
      tidyr::expand_grid(tibble::tibble(region = c(unique(fe_sector_clean$region), "Global"))) %>%
      dplyr::mutate(value = 0) %>%
      dplyr::select(all_of(long_columns))

    # scenario region                         year share_CO2_ETS
  }

  co2_price_share_byreg <<- co2_price_share_byreg %>%
    dplyr::mutate(CO2_ETS = dplyr::if_else(is.na(CO2_ETS), 0, CO2_ETS)) %>%
    dplyr::mutate(share_CO2_ETS = CO2_ETS / CO2) %>%
    dplyr::select(scenario, region, year, share_CO2_ETS)

  co2_price_share_bysec <<- co2_clean %>%
    dplyr::filter(year == last_historical_year) %>%
    rbind(co2_ets_bysec %>%
            dplyr::filter(year == last_historical_year)) %>%
    # select only reported sectors and do a right join, so that all sectors are present,
    # even if the value is NA
    dplyr::right_join(expand.grid(var = c('Emissions|CO2|Energy and Industrial Processes',
                                   'Emissions|CO2|Energy|Demand|Industry',
                                   'Emissions|CO2|Energy|Demand|Transportation',
                                   'Emissions|CO2|Energy|Demand|Residential and Commercial',
                                   'Emissions|CO2|Energy|Supply',
                                   'Emissions|CO2_ETS|Energy and Industrial Processes',
                                   'Emissions|CO2_ETS|Energy|Demand|Industry',
                                   'Emissions|CO2_ETS|Energy|Demand|Transportation',
                                   'Emissions|CO2_ETS|Energy|Demand|Residential and Commercial',
                                   'Emissions|CO2_ETS|Energy|Supply'),
                           region = unique(co2_clean$region),
                           scenario = unique(co2_clean$scenario),
                           year = 2015)) %>%
    dplyr::mutate(value = dplyr::if_else(is.na(value), 0, value)) %>%
    # compute the share
    dplyr::mutate(sector = sub('.*\\|([^|]+)$', '\\1', var),
                  ghg = sub('Emissions\\|([^|]+)\\|.*$', '\\1', var)) %>%
    dplyr::select(-var) %>% dplyr::distinct(.) %>%
    tidyr::pivot_wider(names_from = 'ghg', values_from = 'value') %>%
    dplyr::mutate(CO2_ETS, dplyr::if_else(is.na(CO2_ETS), 0, CO2_ETS)) %>%
    dplyr::mutate(share_CO2_ETS = CO2_ETS/CO2) %>%
    # if the share is > 1, set it to 1 (seems that "biomass" is not accounted in the CO2 emissions query)
    dplyr::mutate(share_CO2_ETS = dplyr::if_else(share_CO2_ETS > 1, 1, share_CO2_ETS)) %>%
    dplyr::select(scenario, region, year, sector, share_CO2_ETS)
}



#' get_co2_price_fragmented_tmp
#'
#' Get fragmented co2 price.
#' @keywords co2 rgcam::getQuery tmp
#' @return co2_price_fragmented global variable
#' @export
get_co2_price_fragmented_tmp = function() {

  co2_price_fragmented_pre <<-
    rgcam::getQuery(prj, "CO2 prices") %>%
    dplyr::filter(!grepl("LUC", market)) %>%
    dplyr::filter(market != "globalCO2") %>%
    dplyr::filter(Units == '1990$/tC')

  if(nrow(co2_price_fragmented_pre) > 1) {

    CO2_market_filteredReg = filter_data_regions(CO2_market)

    co2_price_fragmented <<-
      co2_price_fragmented_pre %>%
      dplyr::left_join(CO2_market_filteredReg, by = c("market"), multiple = "all") %>%
      dplyr::filter(stats::complete.cases(.)) %>%
      dplyr::mutate(value = value / conv_C_CO2 * conv_90USD_10USD) %>%
      dplyr::mutate(market_adj = "CO2",
                    market_adj = dplyr::if_else(grepl("ETS", market), "CO2_ETS", market_adj),
                    market_adj = dplyr::if_else(grepl("CO2BLD", market), "CO2BLD", market_adj),
                    market_adj = dplyr::if_else(grepl("CO2IND", market), "CO2_ETS", market_adj),
                    market_adj = dplyr::if_else(grepl("CO2TRAN", market), "CO2TRAN", market_adj)) %>%
      # consider the value sum of by market (sum CO2_ETS coming from ETS and CO2IND)
      dplyr::group_by(Units, scenario, year, market, region) %>%
      dplyr::mutate(value = sum(value)) %>%
      dplyr::ungroup() %>%
      # apply the share between CO2 and CO2_ETS
      dplyr::select(-market) %>%
      tidyr::pivot_wider(names_from = 'market_adj', values_from = 'value') %>%
      dplyr::mutate(across(6:length(colnames(.)), ~ ifelse(is.na(.), 0, .))) %>%
      dplyr::left_join(co2_price_share_bysec %>%
                        dplyr::select(-year),
                      by = c('scenario','region')) %>%
      dplyr::mutate(value = CO2 + CO2_ETS * share_CO2_ETS) %>%
      dplyr::select(Units, scenario, year, region, value, CO2, CO2_ETS, share_CO2_ETS, sector) %>%
      dplyr::left_join(filter_variables(co2_market_frag_map, 'co2_price_fragmented'), by = "sector", multiple = "all") %>%
      dplyr::filter(stats::complete.cases(.)) %>%
      tidyr::complete(tidyr::nesting(scenario, var, year, market, Units), region = regions, fill = list(value = 0)) %>%
      dplyr::select(all_of(long_columns))
  } else {

    co2_price_fragmented <<- NULL

  }

}


#' get_co2_price
#'
#' Get co2 price.
#' @keywords co2 rgcam::getQuery
#' @return co2_price_clean global variable
#' @export
get_co2_price = function() {

  co2_price_clean_pre <<-
    dplyr::bind_rows(co2_price_global, co2_price_fragmented)

  if(nrow(co2_price_clean_pre) < 1) {

    co2_price_clean <<-
      tibble::tibble(var = unique(filter_variables(co2_market_frag_map, 'co2_price_clean')$var)) %>%
      tidyr::expand_grid(tibble::tibble(scenario = unique(fe_sector_clean$scenario))) %>%
      tidyr::expand_grid(tibble::tibble(year = unique(fe_sector_clean$year))) %>%
      tidyr::expand_grid(tibble::tibble(region = c(unique(fe_sector_clean$region), "Global"))) %>%
      dplyr::mutate(value = 0) %>%
      dplyr::select(all_of(long_columns))

  } else {

    co2_price_clean <<- co2_price_clean_pre %>%
      tidyr::complete(tidyr::nesting(region, var, year), scenario = unique(fe_sector_clean$scenario), fill = list(value = 0)) %>%
      dplyr::select(all_of(long_columns))
  }

}


#' get_gov_revenue_sector
#'
#' Get overall carbon revenue.
#' @keywords revenue rgcam::getQuery
#' @return gov_revenue_sector global variable
#' @export
get_gov_revenue_sector = function() {
  gov_revenue_sector <<-
    co2_clean %>%
    dplyr::mutate(sector  = ifelse(var == "Emissions|CO2|Energy|Demand|Industry", "Carbon|Demand|Industry", NA ),
                  sector = ifelse(var == "Emissions|CO2|Energy|Demand|Residential and Commercial", "Carbon|Demand|Buildings", sector ),
                  sector = ifelse(var == "Emissions|CO2|Energy|Demand|Transportation", "Carbon|Demand|Transport", sector ),
                  sector = ifelse(var == "Emissions|CO2|Energy|Supply|Electricity", "Carbon|Supply", sector ),
                  emiss = value) %>%
    dplyr::select(-var, -value) %>%
    dplyr::filter(!is.na(sector)) %>%
    dplyr::left_join(co2_price_clean %>%
                       dplyr::mutate(sector  = ifelse(var == "Price|Carbon|Demand|Industry", "Carbon|Demand|Industry", NA ),
                                     sector  = ifelse(var == "Price|Carbon|Demand|Residential and Commercial", "Carbon|Demand|Buildings", sector ),
                                     sector  = ifelse(var == "Price|Carbon|Demand|Transportation", "Carbon|Demand|Transport", sector ),
                                     sector  = ifelse(var == "Price|Carbon|Supply", "Carbon|Supply", sector )) %>%
                       dplyr::select(-var),
                     by = c("scenario", "region", "sector", "year")) %>%
    dplyr::mutate(value = value * emiss,
                  var = paste0("Revenue|Government|Tax|", sector))
}

#' get_gov_revenue
#'
#' Get overall carbon revenue.
#' @keywords revenue rgcam::getQuery
#' @return gov_revenue_all global variable
#' @export
get_gov_revenue = function() {
  gov_revenue_clean <<-
    gov_revenue_sector %>%
    dplyr::bind_rows(gov_revenue_sector %>%
                       dplyr::group_by(scenario, region, year) %>%
                       dplyr::summarise(value = sum(value, na.rm =T))%>%
                       dplyr::ungroup() %>%
                       dplyr::mutate(var = "Revenue|Government|Tax|Carbon",)) %>%
    dplyr::mutate(value = value/1000) %>%
    dplyr::select(all_of(long_columns))
}


#' get_prices_subsector
#'
#' Get energy prices - primary, secondary, and final
#' @keywords prices rgcam::getQuery
#' @return prices_subsector global variable
#' @export
get_prices_subsector = function() {
  prices_subsector <<-
    rgcam::getQuery(prj, "prices by sector") %>%
    dplyr::select(-Units) %>%
    dplyr::left_join(filter_variables(energy_prices_map, 'prices_subsector') %>%
                       dplyr::filter(is.na(subsector)) %>%
                       unique, by = c("sector"), multiple = "all") %>%
    dplyr::bind_rows(rgcam::getQuery(prj, "costs by subsector") %>%
                       dplyr::left_join(filter_variables(energy_prices_map, 'prices_subsector') %>%
                                          unique,
                                        by = c("sector", "subsector"))) %>%
    dplyr::filter(!is.na(var)) %>%
    # read in carbon content in kg C per GJ -> convert to tC per GJ
    dplyr::left_join(carbon_content %>%
                       dplyr::filter(grepl("biomass", PrimaryFuelCO2Coef.name)),
                     by = c("region", "sector" = "PrimaryFuelCO2Coef.name")) %>%
    dplyr::mutate(PrimaryFuelCO2Coef = PrimaryFuelCO2Coef / 1000) %>%
    tidyr::replace_na(list(PrimaryFuelCO2Coef = 0))
}


#' get_energy_price_fragmented
#'
#' Get energy prices fragmented, join by region since price is different.
#' @keywords prices process
#' @return energy_price_fragmented global variable
#' @export
get_energy_price_fragmented = function() {
  CO2_market_filteredReg = filter_data_regions(CO2_market)

  energy_price_fragmented <<-
    prices_subsector %>%
    dplyr::filter(!is.na(var)) %>%
    dplyr::left_join(rgcam::getQuery(prj, "CO2 prices") %>%
                       dplyr::filter(!grepl("LUC", market)) %>%
                       dplyr::left_join(CO2_market_filteredReg, by = c("market"), multiple = "all") %>%
                       dplyr::select(scenario, region, year, price_C = value), by = c("scenario", "region", "year")) %>%
    tidyr::replace_na(list(price_C = 0)) %>%
    # remove carbon price (subsidy) 1990$/tC from biomass 1975$/GJ
    dplyr::mutate(price_C = PrimaryFuelCO2Coef * price_C * conv_90USD_10USD,
                  value = value * unit_conv * conv_75USD_10USD + price_C) %>%
    dplyr::select(all_of(long_columns))
}

#' get_total_revenue
#'
#' Compute total revenue: total production * global price.
#' @keywords revenue rgcam::getQuery
#' @return total_revenue global variable
#' @export
get_total_revenue = function() {
  total_revenue <<-
    rgcam::getQuery(prj, "resource production") %>%
    dplyr::filter(resource %in% c("coal", "crude oil", "natural gas")) %>%
    dplyr::group_by(scenario, resource, year) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::rename(total_production = value) %>%
    dplyr::left_join(rgcam::getQuery(prj, "prices by sector") %>%
                       dplyr::filter(sector %in% c("regional coal", "regional oil", "regional natural gas")) %>%
                       dplyr::mutate(resource = ifelse(sector == "regional coal", "coal", NA),
                                     resource = ifelse(sector == "regional oil", "crude oil", resource),
                                     resource = ifelse(sector == "regional natural gas", "natural gas", resource)) %>%
                       dplyr::rename(resource_price = value) %>%
                       dplyr::group_by(scenario, resource, year) %>%
                       dplyr::summarise(resource_price = mean(resource_price, na.rm = T)) %>%
                       dplyr::ungroup(),
                     by = c("scenario", "year", "resource")) %>%
    dplyr::mutate(total_production = total_production * GJ_to_EJ,
                  total_revenue = total_production * resource_price * conv_75USD_10USD)
}

#' get_regional_emission
#'
#' Compute regional nonCO2 emission: regional production * nonCO2 coef.
#' @keywords nonco2 rgcam::getQuery
#' @return regional_emission global variable
#' @export
get_regional_emission = function() {
  regional_emission <<-
    nonCO2_content %>%
    dplyr::filter(year == 2005) %>%
    tidyr::spread(Non.CO2, emiss.coef) %>%
    dplyr::rename(CH4.coef = CH4,
                  N2O.coef = N2O) %>%
    dplyr::mutate(CH4.coef = CH4.coef/1000000,
                  N2O.coef =  N2O.coef/1000000) %>%
    dplyr::select(region, resource, CH4.coef, N2O.coef) %>%
    dplyr::left_join(rgcam::getQuery(prj, "resource production")%>%
                       dplyr::filter(resource %in% c("coal", "crude oil", "natural gas")) %>%
                       dplyr::rename(regional_production = value), by = c("region", "resource")) %>%
    dplyr::mutate(regional_CH4emission = regional_production *CH4.coef * GJ_to_EJ,
                  regional_N2Oemission = regional_production *N2O.coef * GJ_to_EJ)
}


#' get_energy_price_tmp
#'
#' Bind regional oil, gas, coal prices with other energy prices
#' @keywords price rgcam::getQuery tmp
#' @return energy_price global variable
#' @export
get_energy_price_tmp = function() {
  energy_price <<-
    energy_price_fragmented  %>%
    dplyr::select(all_of(long_columns))
}


#' get_energy_price
#'
#' Compute final energy price
#' @keywords price process
#' @return energy_price_clean global variable
#' @export
get_energy_price = function() {
  energy_price_clean <<-
    energy_price %>%
    dplyr::filter(grepl("Residential\\|Electricity", var)|
                    grepl("Residential\\|Gases", var)|
                    grepl("Primary Energy\\|Coal", var)|
                    grepl("Primary Energy\\|Biomass", var)|
                    grepl("Primary Energy\\|Gas", var)|
                    grepl("Primary Energy\\|Oil", var)|
                    grepl("Secondary Energy\\|Electricity", var)|
                    grepl("Secondary Energy\\|Gases", var)|
                    grepl("Secondary Energy\\|Liquids\\|Biomass", var)|
                    grepl("Secondary Energy\\|Liquids\\|Oil", var)|
                    grepl("Secondary Energy\\|Solids\\|Coal", var))%>%
    dplyr::mutate(var = paste(var, "Index", sep = "|")) %>%
    dplyr::group_by(scenario, region, var) %>%
    dplyr::mutate(value = value/ value[year == 2020]) %>%
    dplyr::ungroup() %>%
    dplyr::select(all_of(long_columns)) %>%
    dplyr::bind_rows(energy_price)

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
#' @keywords capacity process tmp
#' @return cf_iea global variable
#' @export
get_cf_iea_tmp = function() {
  cf_rgn_filteredReg = filter_data_regions(cf_rgn)

  cf_iea <<-
    elec_gen_tech_clean %>%
    dplyr::filter(year == 2020, scenario == unique(elec_gen_tech_clean$scenario)[1]) %>%
    dplyr::group_by(var) %>%
    dplyr::summarise(EJ = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(iea_capacity %>%
                       dplyr::filter(period == 2020, scenario == "Current Policies Scenario") %>%
                       dplyr::mutate(variable = gsub("Capacity\\|Electricity\\|CSP", "Capacity\\|Electricity\\|Solar\\|CSP", variable),
                                     variable = gsub("Capacity\\|Electricity\\|Biomass", "Capacity\\|Electricity\\|Biomass\\|w/o CCS", variable),
                                     variable = gsub("Capacity\\|Electricity\\|Coal", "Capacity\\|Electricity\\|Coal\\|w/o CCS", variable),
                                     variable = gsub("Capacity\\|Electricity\\|Gas", "Capacity\\|Electricity\\|Gas\\|w/o CCS", variable),
                                     variable = gsub("Capacity\\|Electricity\\|Oil", "Capacity\\|Electricity\\|Oil\\|w/o CCS", variable),
                                     variable = gsub("Capacity", "Secondary Energy", variable)),
                     by = c("var" = "variable")) %>%
    dplyr::mutate(cf = EJ / (value * hr_per_yr * EJ_to_GWh),
                  cf = replace(cf, cf > 1, 0.99)) %>%
    dplyr::filter(!is.na(cf), !var %in% c("Secondary Energy|Electricity", "Secondary Energy|Electricity|Non-Biomass Renewables")) %>%
    dplyr::left_join(filter_variables(capacity_map, 'cf_iea'), by = "var", multiple = "all") %>%
    dplyr::select(technology, cf) %>%
    dplyr::mutate(region = "USA", vintage = 2020) %>%
    tidyr::complete(tidyr::nesting(technology, cf),
                    vintage = c(1990, seq(2005, 2020, by = 5)),
                    region = unique(cf_rgn_filteredReg$region))
}

#' get_elec_cf_tmp
#'
#' Calculate future capacity using GCAM
#' @keywords capacity process tmp
#' @return elec_cf global variable
#' @export
get_elec_cf_tmp = function() {
  cf_rgn_filteredReg = filter_data_regions(cf_rgn)
  cf_iea_filteredReg = filter_data_regions(cf_iea)

  elec_cf <-
    cf_gcam %>%
    dplyr::select(technology, cf = X2100) %>%
    dplyr::mutate(region = "USA", vintage = 2025) %>%
    tidyr::complete(tidyr::nesting(technology, cf),
                    vintage = seq(2025, 2100, by = 5),
                    region = unique(cf_rgn_filteredReg$region)) %>%
    # first, replace regional cf for wind and solar
    dplyr::left_join(cf_rgn_filteredReg %>%
                       dplyr::select(region, technology = stub.technology, vintage = year, cf.rgn = capacity.factor),
                     by = c("technology", "vintage", "region")) %>%
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
#' @keywords capacity process
#' @return elec_capacity_tot_clean global variable
#' @export
get_elec_capacity_tot = function() {
  elec_capacity_tot_clean <<-
    rgcam::getQuery(prj, "elec gen by gen tech and cooling tech and vintage") %>%
    dplyr::filter(!output %in% c("electricity", "elect_td_bld" )) %>%
    tidyr::separate(technology, into = c("technology", "vintage"), sep = ",") %>%
    dplyr::mutate(vintage = as.integer(sub("year=", "", vintage)),
                  output = gsub("elec_", "", output)) %>%
    dplyr::group_by(scenario, region, technology = output, vintage, year) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::bind_rows(rgcam::getQuery(prj, "elec gen by gen tech and cooling tech and vintage") %>%
                       dplyr::filter(output %in% c("electricity", "elect_td_bld" )) %>%
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
    dplyr::left_join(filter_variables(capacity_map, 'elec_capacity_tot_clean') %>% dplyr::select(-output), by = c("technology"), multiple = "all") %>%
    dplyr::filter(!is.na(var)) %>%
    dplyr::mutate(value = value * unit_conv,
                  var = sub("Secondary Energy", "Capacity", var)) %>%
    dplyr::group_by(scenario, region, var, year) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    tidyr::complete(tidyr::nesting(scenario,region, year),
                    var = unique(var),
                    fill = list(value = 0)) %>%
    dplyr::select(all_of(long_columns))
}

#' get_elec_capacity_add_tmp
#'
#' Calculate added total capacity
#' @keywords capacity process tmp
#' @return elec_capacity_add global variable
#' @export
get_elec_capacity_add_tmp = function() {
  elec_capacity_add <<-
    rgcam::getQuery(prj, "elec gen by gen tech and cooling tech and vintage") %>%
    dplyr::filter(!output %in% c("electricity", "elect_td_bld" )) %>%
    tidyr::separate(technology, into = c("technology", "vintage"), sep = ",") %>%
    dplyr::mutate(vintage = as.integer(sub("year=", "", vintage)),
                  output = gsub("elec_", "", output)) %>%
    dplyr::group_by(scenario, region, technology = output, vintage, year) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::bind_rows(rgcam::getQuery(prj, "elec gen by gen tech and cooling tech and vintage") %>%
                       dplyr::filter(output %in% c("electricity", "elect_td_bld" )) %>%
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
    dplyr::left_join(elec_cf ,
                     by = c("region", "technology", "year" = "vintage")) %>%
    # use average annual additions
    dplyr::mutate(EJ = value / 5) %>%
    conv_EJ_GW() %>%
    dplyr::group_by(scenario, region, technology, year) %>% #
    dplyr::summarise(GW = sum(gw, na.rm = T), EJ = sum(EJ, na.rm = T)) %>%
    dplyr::ungroup()
}

#' get_elec_capacity_add
#'
#' Calculate added total capacity
#' @keywords capacity process
#' @return elec_capacity_add_clean global variable
#' @export
get_elec_capacity_add = function() {
  # check calculations for this
  elec_capacity_add_clean <<-
    elec_capacity_add %>%
    dplyr::left_join(capacity_map %>% dplyr::select(-output), by = c("technology"), multiple = "all") %>%
    dplyr::filter(!var %in% c("Secondary Energy|Electricity|Hydro",
                              "Secondary Energy|Electricity|Storage Capacity")) %>%
    dplyr::mutate(value = GW * unit_conv,
                  var = sub("Secondary Energy", "Capacity Additions", var)) %>%
    dplyr::bind_rows(elec_capacity_add %>%
                       dplyr::left_join(capacity_map %>% dplyr::select(-output), by = c("technology"), multiple = "all") %>%
                       dplyr::filter(var == "Secondary Energy|Electricity|Storage Capacity") %>%
                       dplyr::mutate(value = GW * 8760, # multiply by # of hours in a year
                                     var = sub("Secondary Energy", "Capacity Additions", var))) %>%
    filter_variables(variable = 'elec_capacity_add_clean') %>%
    dplyr::group_by(scenario, region, var, year) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    tidyr::complete(tidyr::nesting(scenario,region, year),
                    var = unique(var),
                    fill = list(value = 0)) %>%
    dplyr::select(all_of(long_columns))
}


#' get_elec_capital
#'
#' Calculate electricity capital
#' @keywords capital process
#' @return elec_capital_clean global variable
#' @export
get_elec_capital = function() {
  cf_rgn_filteredReg = filter_data_regions(cf_rgn)

  # Capital costs from GCAM in $1975/kw -> convert to $2010/kw
  elec_capital <-
    capital_gcam %>%
    dplyr::mutate(scenario = Scenarios[1]) %>%
    dplyr::select(-sector) %>%
    # gw * 10e6 * $/kw / 10e9 = bill$
    dplyr::mutate(value = capital.overnight * conv_75USD_10USD) %>%
    dplyr::left_join(filter_variables(elec_gen_map) %>% dplyr::select(-output),
                     by = c("subsector", "technology"),
                     relationship = "many-to-many")

  elec_capital_clean <<-
    filter_data_regions(elec_capital) %>%
    dplyr::filter(!is.na(var), var != "Secondary Energy|Electricity|Electricity Storage") %>%
    dplyr::mutate(value = value * unit_conv,
                  var = sub("Secondary Energy", "Capital Cost", var)) %>%
    dplyr::group_by(scenario, region, var, year) %>%
    dplyr::summarise(value = mean(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(all_of(long_columns))

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
#' @keywords capital process
#' @return elec_investment_clean global variable
#' @export
get_elec_investment = function() {
  elec_investment_clean <<-
    # Electricity investment = annual capacity additions * capital costs
    elec_capacity_add %>%
    dplyr::left_join(capital_gcam %>%
                       dplyr::mutate(capital.overnight = replace(capital.overnight, technology=="wind_storage", capital.overnight[technology == "wind"]*.484),
                                     capital.overnight = replace(capital.overnight, technology=="CSP_storage", 760 * conv_19USD_75USD),
                                     capital.overnight = replace(capital.overnight, technology=="PV_storage", capital.overnight[technology == "PV"]*.518)),
                     by = c("technology", "year", "region")) %>%
    # gw * 10e6 * $/kw / 10e9 = bill$
    dplyr::mutate(value = GW * capital.overnight / 1000 * conv_75USD_10USD) %>%
    dplyr::left_join(filter_variables(elec_gen_map) %>% dplyr::select(-output), by = c("technology"), relationship = "many-to-many") %>%
    dplyr::filter(!is.na(var)) %>%
    dplyr::mutate(value = value * unit_conv,
                  var = sub("Secondary Energy", "Investment|Energy Supply", var)) %>%
    dplyr::group_by(scenario, region, var, year) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::select(all_of(long_columns))
}

#' get_transmission_invest
#'
#' Calculate investment electricity transmission and distribution
#' scale 2020 number - average of other model results from Mcollion et al. 2018
#' Need to convert 2015 to 2010 $
#' @keywords investment process
#' @return transmission_invest_clean global variable
#' @export
get_transmission_invest = function() {
  transmission2020 <-
    investment %>%
    dplyr::filter(Region == "World", Variable == "Energy Supply|Electricity|Transmission and Distribution", year == 2020) %>%
    dplyr::mutate(value = value * conv_15USD_10USD) %>%
    dplyr::summarise(value = mean(value, na.rm = T)) %>% unlist

  transmission_invest2020 <-
    elec_capacity_add_clean %>%
    dplyr::filter(var == "Capacity Additions|Electricity", year == 2020) %>%
    dplyr::group_by(scenario) %>%
    dplyr::mutate(share = value / sum(value, na.rm = T),
                  invest = share * transmission2020)

  transmission_invest_clean <<-
    elec_capacity_add_clean %>%
    dplyr::filter(var == "Capacity Additions|Electricity") %>%
    dplyr::group_by(scenario, region) %>%
    dplyr::mutate(rate = value / value[year == 2020]) %>%
    dplyr::left_join(transmission_invest2020 %>%
                       dplyr::select(scenario, region, invest),
                     by = c("scenario", "region")) %>%
    dplyr::mutate(value = rate * invest, var = "Investment|Energy Supply|Electricity|Transmission and Distribution") %>%
    dplyr::select(all_of(long_columns)) #%>%  dplyr::group_by(scenario, year) %>% dplyr::summarise(value = sum(value)) %>% tidyr::spread(year, value)
}


#' get_CCS_invest
#'
#' Calculate CSS investment
#' @keywords investment process
#' @return CCS_invest_clean global variable
#' @export
get_CCS_invest = function() {
  # use last available year if 2040 is not present in the data
  yy = ifelse(max(unique(co2_sequestration_clean$year)) >= 2040, 2040, max(unique(co2_sequestration_clean$year)))

  CCS2040 <-
    investment %>%
    dplyr::filter(Region == "World", Variable == "CCS", year == yy) %>%
    dplyr::mutate(value = value * conv_15USD_10USD) %>%
    dplyr::summarise(value = mean(value, na.rm = T)) %>% unlist()

  CCS_invest2040 <-
    co2_sequestration_clean %>%
    dplyr::filter(var == "Carbon Sequestration|CCS", year == yy) %>%
    dplyr::group_by(scenario) %>%
    dplyr::mutate(share = value / sum(value, na.rm = T),
                  invest = share * CCS2040 )

  CCS_invest_clean <<-
    co2_sequestration_clean %>%
    dplyr::filter(var == "Carbon Sequestration|CCS") %>%
    dplyr::group_by(scenario, region) %>%
    dplyr::mutate(rate = value / value[year == yy]) %>%
    dplyr::left_join(CCS_invest2040 %>%
                       dplyr::select(scenario, region, invest),
                     by = c("scenario", "region")) %>%
    dplyr::mutate(value = rate * invest, var = "Investment|Energy Supply|CO2 Transport and Storage") %>%
    dplyr::select(all_of(long_columns))
}

#' get_resource_investment
#'
#' Calculate investment of resource production
#' @keywords investment process
#' @return resource_investment_clean global variable
#' @export
get_resource_investment = function() {
  # Investment of resource production
  resource_addition <-
    rgcam::getQuery(prj, "resource production by tech and vintage") %>%
    dplyr::filter(resource %in% c("coal", "natural gas", "crude oil", "unconventional oil")) %>%
    tidyr::separate(technology, into = c("technology", "vintage"), sep = ",") %>%
    dplyr::mutate(vintage = as.integer(sub("year=", "", vintage))) %>%
    dplyr::filter(year > 2010) %>%
    dplyr::mutate(resource = sub("crude oil", "oil", resource),
                  resource = sub("unconventional oil", "oil", resource)) %>%
    dplyr::group_by(scenario, resource, region, year) %>%
    dplyr::summarise(production = sum(value, na.rm = T)) %>%
    dplyr::ungroup()

  # scale 2015 number - average of other model results from Mcollion et al. 2018
  extraction2015 <-
    investment %>%
    dplyr::filter(Region == "World", Variable == "Extraction and Conversion - Fossil Fuels", year == 2015) %>%
    dplyr::mutate(value = value * conv_15USD_10USD) %>%
    dplyr::summarise(value = mean(value, na.rm = T)) %>% unlist

  extraction2020 <-
    investment %>%
    dplyr::filter(Region == "World", Variable == "Extraction and Conversion - Fossil Fuels", year == 2020) %>%
    dplyr::mutate(value = value * conv_15USD_10USD) %>%
    dplyr::summarise(value = mean(value, na.rm = T)) %>% unlist

  resource_investment2015 <-
    resource_addition %>%
    dplyr::filter(year == 2015) %>%
    dplyr::left_join(rgcam::getQuery(prj, "regional primary energy prices") %>%
                       dplyr::mutate(fuel = sub("regional ", "", fuel)),
                     by = c("scenario", "region", "year", "resource" = "fuel")) %>%
    dplyr::mutate(value = production * value) %>%
    dplyr::group_by(scenario, resource, region) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(scenario) %>%
    dplyr::mutate(share = value / sum(value, na.rm = T),
                  invest = share * extraction2015) %>%
    dplyr::ungroup()

  resource_investment2020 <-
    resource_addition %>%
    dplyr::filter(year == 2020) %>%
    dplyr::left_join(rgcam::getQuery(prj, "regional primary energy prices") %>%
                       dplyr::mutate(fuel = sub("regional ", "", fuel)),
                     by = c("scenario", "region", "year", "resource" = "fuel")) %>%
    dplyr::mutate(value = production * value) %>%
    dplyr::group_by(scenario, resource, region) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(scenario) %>%
    dplyr::mutate(share = value / sum(value, na.rm = T),
                  invest = share * extraction2020) %>%
    dplyr::ungroup()

  reg = dplyr::if_else(desired_regions == 'All' | 'China' %in% desired_regions, 'China', desired_regions[1])[1]

  resource_investment <-
    resource_addition %>%
    dplyr::filter(year!= 2020) %>%
    dplyr::group_by(scenario, resource) %>%
    dplyr::mutate(rate = production / production[year == 2015 & region == reg]) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(resource_investment2015 %>%
                       dplyr::filter(region == reg) %>%
                       dplyr::select(scenario, resource, invest),
                     by = c('scenario', 'resource')) %>%
    dplyr::bind_rows(resource_addition %>%
                       dplyr::filter(year == 2020) %>%
                       dplyr::group_by(scenario, resource) %>%
                       dplyr::mutate(rate = production / production[region == reg]) %>%
                       dplyr::ungroup() %>%
                       dplyr::left_join(resource_investment2020 %>%
                                          dplyr::filter(region == reg) %>%
                                          dplyr::select(scenario, resource, invest),
                                        by =c('scenario', 'resource'))) %>%
    dplyr::mutate(value = invest * rate,
                  resource = sub("coal", "Coal", resource),
                  resource = sub("natural gas", "Gas", resource),
                  resource = sub("oil", "Oil", resource),
                  var = paste0('Investment|Energy Supply|Extraction|', resource)) %>%
    dplyr::select(all_of(long_columns))

  resource_investment_clean <<-
    resource_investment %>%
    dplyr::group_by(scenario, region, year) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(var = 'Investment|Energy Supply|Extraction|Fossil') %>%
    dplyr::select(all_of(long_columns)) %>%
    dplyr::bind_rows(resource_investment) #%>% dplyr::group_by(scenario, var, year) %>% dplyr::summarise(value = sum(value)) %>% tidyr::spread(year, value)
}

#########################################################################
#                        BIND TO TEMPLATE FUNCTIONS                     #
#########################################################################
#' do_bind_results
#'
#' Bind and save results
#' @keywords process
#' @return Save results in an output file.
#' @export
do_bind_results = function() {
  vars = variables[variables$required == TRUE, 'name']
  GCAM_DATA <-
    dplyr::bind_rows(lapply(vars, function(x) get(x))) %>%
    dplyr::mutate(region = gsub("Global", "World", region),
                  region = gsub("global", "World", region))

  # Calculate global total
  GCAM_DATA_WORLD <-
    GCAM_DATA %>%
    dplyr::filter(region != "World", # excl. Temperature|Forcing|Concentration
           # excl. price and costs variables - already calculated global average
           !grepl("Price|Capital Cost", var)) %>%
    dplyr::group_by(scenario, year, var) %>%
    dplyr::summarise(value = sum(value, na.rm = T)) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(region = "World")

  GCAM_DATA_wGLOBAL <-
    GCAM_DATA_WORLD %>%
    dplyr::bind_rows(GCAM_DATA %>% dplyr::filter(!(region == "World" & var %in% unique(GCAM_DATA_WORLD$var)))) %>%
    tidyr::complete(tidyr::nesting(scenario, region, var), year = reporting_years) %>%
    tidyr::replace_na(list(value = 0)) %>%
    dplyr::distinct(.)

  # dplyr::filter to final_db_year
  GCAM_DATA_wGLOBAL <- GCAM_DATA_wGLOBAL %>% dplyr::filter(year <= final_db_year)


  final_data <<-
    template %>%
    dplyr::inner_join(GCAM_DATA_wGLOBAL %>%
                        na.omit %>%
                        tidyr::pivot_wider(names_from = 'year', values_from = 'value'),
                      by = c("Variable" = "var"), multiple = "all") %>%
    #  dplyr::left_join(reporting_scen %>% dplyr::select(GCAM_scenario, Scenario),
    #            by = c("scenario" = "GCAM_scenario")) %>%
    dplyr::rename(Region = region) %>%
    #  dplyr::rename(Model = ?..Model) %>%
    dplyr::rename(Scenario = scenario) %>%
    dplyr::select(all_of(reporting_columns_fin)) %>%
    dplyr::filter(!is.na(Region)) %>% # Drop variables we don't report
    dplyr::filter(Variable %in% desired_variables)
}

#########################################################################
#                       CHECKS AND VETTING FUNCTIONS                    #
#########################################################################
#' do_check_trade
#'
#' Check global trade is zero.
#' @keywords check
#' @return Verification message indicating if the process was successful.
#' @export
do_check_trade = function() {

  if (exists('energy_trade_prod')) {
    # check global total is zero
    trade <- energy_trade_prod %>%
      dplyr::left_join(energy_trade_supply, by = c("scenario", "resource", "region", "year")) %>%
      dplyr::group_by(scenario, resource, year) %>%
      dplyr::summarise(production = sum(production, na.rm = T),
                       demand = sum(demand, na.rm = T)) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(diff = (production - demand) / production,
                    check = dplyr::if_else( abs(diff) > 1, "ERROR", "OK" ))

    check_trade_summary <- trade %>%
      dplyr::rename('percentual_diff_between_production_and_demand' = 'diff')

    if (nrow(trade %>% dplyr::filter( check == "ERROR")) > 0) {
      res = list(message = 'Trade flows: ERROR',
                 summary = as.data.frame(check_trade_summary))
    } else {
      res = list(message = 'Trade flows: OK',
                 summary = check_trade_summary)
    }
  } else {
    res = list(message = 'Trade flows: Vetting not performed',
               summary = 'To check the trade flows, consider introducing `Trade*` as desired variable.')
  }
  return(res)
}


#' do_check_vetting
#'
#' Verify vetting and produce plot.
#' @keywords check
#' @return Verification message indicating if the process was successful.
#' @export
do_check_vetting = function() {
  # Check vetting results from SM
  final_data_long_check <- final_data %>%
    tidyr::gather(year, value, -Model, -Variable, -Unit, -Scenario, -Region) %>%
    dplyr::rename(region = Region,
                  variable = Variable) %>%
    dplyr::filter(Scenario == Scenarios[1]) %>%
    dplyr::mutate(year = as.integer(year))

  check_vet <- global_vet_values %>%
    dplyr::select(variable = adj_var, adj_var2, region, year, value, unit, range = Range) %>%
    dplyr::rename(unit_vet = unit,
                  value_vet = value) %>%
    dplyr::left_join(final_data_long_check, by = c("variable", "region", "year")) %>%
    tidyr::unnest(value) %>%
    dplyr::mutate(value = dplyr::if_else(grepl("Traditional", variable), value * -1, value)) %>%
    dplyr::select(Scenario, variable = adj_var2, region, year, value, unit = Unit, value_vet, unit_vet, range) %>%
    # Adjust for Solar&Wind and biomass
    dplyr::group_by(Scenario, variable, region, year, unit, unit_vet, range) %>%
    dplyr::summarise(value = sum(value),
                     value_vet = mean(value_vet)) %>%
    dplyr::ungroup() %>%
    # dplyr::mutate(unit_vet = as.character(unit_vet)) %>%
    dplyr::mutate(value_vet = dplyr::if_else(unit_vet == "bcm", value_vet * bcm_to_EJ, value_vet),
                  unit_vet = dplyr::if_else(unit_vet == "bcm", "EJ/yr", unit_vet)) %>%
    dplyr::mutate(diff = (value / value_vet) - 1,
                  check = dplyr::if_else(abs(diff) > range, "ERROR", "OK"))

  check_vet_summary <- check_vet %>%
    dplyr::rename('computed_value' = 'value',
                  'expected_value (vetting)' = 'value_vet',
                  'confidance_range' = 'range')

  ## plot
  check_vet_plot <- check_vet %>%
    dplyr::select(-year, -range, -diff, -check, -unit_vet) %>%
    tidyr::gather(type, value, -variable, -Scenario, -unit, -region)

  ggplot2::ggplot(check_vet_plot, ggplot2::aes(x = variable, y = value, fill = type)) +
    ggplot2::geom_bar(stat = "identity", position = "dodge") +
    ggplot2::facet_wrap(~variable, scales = "free") +
    ggplot2::labs(x = "", y = "value") +
    ggplot2::theme_classic()+
    ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                   legend.position = "bottom",
                   strip.text = ggplot2::element_text(size = 5),
                   legend.title = ggplot2::element_blank())
  if (!dir.exists(paste0(here::here(), "/output/"))){
    dir.create(paste0(here::here(), "/output/"))
  }
  if (!dir.exists(paste0(here::here(), "/output/figure/"))){
    dir.create(paste0(here::here(), "/output/figure/"))
  }
  ggplot2::ggsave(paste0(here::here(), "/output/figure", "/vetting.tiff"), ggplot2::last_plot(), "tiff", dpi = 200)

  # output
  if (nrow(check_vet_summary[check_vet_summary$check == 'ERROR',]) == 0) {
    res = list(message = 'Vetting variables: OK',
               summary = check_vet_summary)
  } else if (nrow(check_vet_summary[is.na(check_vet_summary$check),]) > 0) {
    res = list(message = 'Vetting variables: Vetting only performed on some variables',
               summary = check_vet_summary)
  } else {
    res = list(message = 'Vetting variables: ERROR',
               summary = as.data.frame(check_vet_summary))
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
#' @keywords internal
#' @return Updated template as .rda and as csv in the inst/extdata folder
update_template = function() {
  data = merge(template,
               data.frame(Variable = unique(final_data$Variable)) %>%
                 dplyr::mutate('as_output' = TRUE),
               by = 'Variable', all = TRUE) %>%
    dplyr::select(colnames(template), as_output) %>%
    # if the variable was not given as output, set NA as Internal_variable
    dplyr::mutate(Internal_variable = dplyr::if_else(is.na(as_output), NA, Internal_variable)) %>%
    # if there is a variable given as output but not recorded as so, print it
    dplyr::mutate(Variables_outputed_but_not_recorded =
                    dplyr::if_else(is.na(Internal_variable) & !is.na(as_output), Variable, NA))
  print(paste0('New variables that can be reported: ', unique(data$Variables_outputed_but_not_recorded)))
  print(paste0('Old variables that are no longer reported: ',
               dplyr::anti_join(template %>%
                                  dplyr::filter(!is.na(Internal_variable) & Internal_variable != '') %>%
                                  dplyr::select(Variable),
                                data %>%
                                  dplyr::filter(!is.na(Internal_variable) & Internal_variable != '') %>%
                                  dplyr::select(Variable)
               )
  ))

  template = data %>%
    dplyr::select(colnames(template))

  write.csv(template, paste0(getwd(), "/inst/extdata", "/template/reporting_template.csv"), row.names = FALSE)
  use_data(template, overwrite=T)
}

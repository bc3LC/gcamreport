#########################################################################
#                           ANCILLARY FUNCTIONS                         #
#########################################################################

#' conv_ghg_co2e
#'
#' Covert GHG to CO2e
#' @export
conv_ghg_co2e <- function (data) {

  # Ancillary function
  # GHG emission conversion
  F_GASES <- c("C2F6", "CF4", "HFC125", "HFC134a", "HFC245fa", "SF6", "HFC143a", "HFC152a", "HFC227ea", "HFC23", "HFC236fa", "HFC32", "HFC365mfc", "HFC43",
               "HFC245fa", "HFC43-10")
  GHG_gases <- c("CH4", "N2O", F_GASES, "CO2", "CO2LUC")

  GWP_adjuster <- read.csv(paste0(map_dir, "/ghg_GWP.csv"), skip = 1, na = "")

  data %>%
    dplyr::separate(ghg, into = c("variable", "sector"), sep = "_", fill = "right") %>%
    dplyr::filter(variable %in% GHG_gases) %>%
    dplyr::left_join(GWP_adjuster, by = c("variable" = "GHG_gases")) %>%
    dplyr::mutate(value = value * GWP, Units = "CO2e") %>%
    dplyr::select(-GWP) %>%
    return()
}

#' conv_EJ_TWh
#'
#' Covert capacity to generation
#' @export
conv_EJ_TWh <- function (data, EJ){
  data %>%
    dplyr::mutate(TWh = EJ / EJ_to_GWh / 1000)
}

#' conv_TWh_EJ
#'
#' Covert TWh to EJ
#' @export
conv_TWh_EJ <- function (data, TWh){
  data %>%
    dplyr::mutate(EJ = TWh * EJ_to_GWh * 1000)
}

#' conv_GW_EJ
#'
#' Covert GW to EJ
#' @export
conv_GW_EJ <- function (data, cf, GW){
  # Elec related conversions
  hr_per_yr <- 8760
  EJ_to_GWh <- 0.0000036

  data %>%
    dplyr::mutate(EJ = GW * (cf * hr_per_yr * EJ_to_GWh))
}

#' conv_EJ_GW
#'
#' Covert EJ to GW
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
#' @export
approx_fun <- function(year, value, rule = 1) {
  if(rule == 1 | rule == 2) {
    tryCatch(stats::approx(as.vector(year), value, rule = rule, xout = year)$y,
             error = function(e) NA)

  } else {
    stop("Use fill_exp_decay_extrapolate!")
  }
}

#' firstup
#'
#' Capitalizes the first letter of every variable
#' @export
firstup <- function(x) {
  x <- tolower(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

#' gather_map
#'
#' Formats all maps into a long table
#' @export
gather_map <- function(df){
  untouched_cols <- names(df) %>% .[!grepl("var", names(df))]
  df %>%
    gather(identifier, var, -untouched_cols) %>%
    dplyr::select(-identifier) %>%
    dplyr::filter(!is.na(var), var != "") %>%
    return()
}

#' standard_tech_group
#'
#' technologies categorization
#' @export
standard_tech_group <- function(data, technology) {
  data %>%
    dplyr::mutate(tech = technology,
             tech = replace(tech, grepl("wind", technology), "wind"),
             tech = replace(tech, grepl("biomass", technology), "biomass"),
             tech = replace(tech, grepl("PV", technology), "solar"),
             tech = replace(tech, grepl("pv", technology), "solar"),
             tech = replace(tech, grepl("CSP", technology), "solar"),
             tech = replace(tech, grepl("geothermal", technology), "geothermal"),
             tech = replace(tech, grepl("hydro", technology), "hydro"),
             tech = replace(tech, grepl("Gen", technology), "nuclear"),
             tech = replace(tech, grepl("coal", technology), "coal"),
             tech = replace(tech, grepl("gas", technology), "gas"),
             tech = replace(tech, grepl("refined liquids", technology), "oil"),
             tech = replace(tech, grepl("oil", technology), "oil"),
             tech = replace(tech, grepl("CCS", technology), paste(tech[grepl("CCS", technology)], "ccs", sep = " w/ "))) %>%
    dplyr::mutate(tech = factor(tech, levels = tech.list))
}

#########################################################################
#                         LOAD QUERIES FUNCTIONS                        #
#########################################################################

# Scioeconomics
# ==============================================================================================
#' get_population
#'
#' Get population query and change units to [million].
#' @keywords population getQuery
#' @return population_clean global variable
#' @export
get_population = function() {
  population_clean <<-
    getQuery(prj, "population by region") %>%
    mutate(value = value * conv_thousand_million,
           var = "Population") %>%
    select(all_of(long_columns))
}


#' get_gdp_ppp
#'
#' Get GDP (PPP) query, compute regional GDP, and change units to [10USD].
#' @keywords GDP getQuery
#' @return GDP_PPP_clean global variable
#' @export
get_gdp_ppp = function() {
  GDP_PPP_clean <<-
    getQuery(prj, "GDP per capita PPP by region") %>%
    left_join(population_clean %>% rename(pop_mill = value), by = c("scenario", "region", "year")) %>%
    mutate(value = value * pop_mill * conv_90USD_10USD,
           var = "GDP|PPP") %>%
    select(all_of(long_columns))
}


#' get_gdp_mer
#'
#' Get GDP (MER) query and change units to [10USD].
#' @keywords GDP getQuery
#' @return GDP_MER_clean global variable
#' @export
get_gdp_mer = function() {
  GDP_MER_clean <<-
    getQuery(prj, "GDP MER by region") %>%
    mutate(value = value * conv_million_billion * conv_90USD_10USD,
           var = "GDP|MER") %>%
    select(all_of(long_columns))
}

# Climate and emissions
# ==============================================================================================
#' get_forcing
#'
#' Get World's forcing query.
#' @keywords forcing getQuery
#' @return forcing_clean global variable
#' @export
get_forcing = function() {
  forcing_clean <<-
    getQuery(prj, "total climate forcing") %>%
    filter(year %in% GCAM_years) %>%
    mutate(var = "Forcing", region = "World") %>%
    select(all_of(long_columns))
}


#' get_temperature
#'
#' Get World's mean temperature query.
#' @keywords temperature getQuery
#' @return global_temp_clean global variable
#' @export
get_temperature = function() {
  global_temp_clean <<-
    getQuery(prj, "global mean temperature") %>%
    filter(year %in% GCAM_years) %>%
    mutate(var = "Temperature|Global Mean", region = "World") %>%
    select(all_of(long_columns))
}


#' get_co2_concentration
#'
#' Get World's CO2 concentration query.
#' @keywords co2 getQuery
#' @return co2_concentration_clean global variable
#' @export
get_co2_concentration = function() {
  co2_concentration_clean <<-
    getQuery(prj, "CO2 concentrations") %>%
    filter(year %in% GCAM_years) %>%
    mutate(var = "Concentration|CO2", region = "World") %>%
    select(all_of(long_columns))
}


#' get_co2
#'
#' Get World's CO2 emissions query.
#' @keywords co2 getQuery
#' @return co2_clean global variable
#' @export
get_co2 = function() {
  co2_clean <<-
    tibble::as_tibble(getQuery(prj, "CO2 emissions by sector (no bio)")) %>%
    left_join(co2_sector_map, by = "sector") %>%
    mutate(value = value * unit_conv) %>%
    group_by(scenario, region, year, var) %>% #
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    select(all_of(long_columns))
}


# Get CO2 emissions by tech, to break out ships vs rail vs aviation
# and to get Emissions|CO2|Energy| Coal vs Gas vs Oil.
# Must create CO2 emissions by tech (no bio) output first to be consistent. There is no query for this

# Apply bio negative emissions by joining by sector and by sector (no bio) and finding share of non-bio emissions

#' get_nonbio_tmp
#'
#' Get no bio CO2 emissions query by sector.
#' @keywords co2 getQuery
#' @return nonbio_share global variable
#' @export
get_nonbio_tmp = function() {
  nonbio_share <<-
    getQuery(prj, "CO2 emissions by sector") %>%
    left_join(getQuery(prj, "CO2 emissions by sector (no bio)"), by = c("region", "scenario", "year", "sector", "Units")) %>%
    mutate(value.y = if_else(is.na(value.y), value.x, value.y),
           percent = value.y/value.x) %>%
    select(-value.x, -value.y)
}


#' get_co2_tech_nobio_tmp
#'
#' Get no bio CO2 emissions query by sector and techonolgy.
#' @keywords co2 getQuery tmp
#' @return co2_tech_nobio global variable
#' @export
get_co2_tech_nobio_tmp = function() {
  co2_tech_nobio <<-
    getQuery(prj, "CO2 emissions by tech") %>%
    left_join(nonbio_share, by = c("region", "scenario", "year", "sector", "Units")) %>%
    mutate(value = value*percent) %>%
    select(-percent)
}


#' get_co2_tech_emissions_tmp
#'
#' Get no bio CO2 emissions query by sector, subsector, and techonolgy.
#' @keywords co2 getQuery tmp
#' @return co2_tech_emissions global variable
#' @export
get_co2_tech_emissions_tmp = function() {
  co2_tech_emissions <<-
    co2_tech_nobio %>%
    left_join(co2_tech_map, by = c("sector", "subsector", "technology"))  %>%
    filter(!is.na(var)) %>%
    mutate(value = value * unit_conv) %>%
    group_by(scenario, region, year, var) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    select(all_of(long_columns))
}


# Iron and Steel Emissions: for Emissions|CO2|Coal, Gas, Oil
# Find which input has the greatest share for each IRONSTL tech (between coal, gas, oil)

#' get_iron_steel_map
#'
#' Get iron and steel emissions.
#' @keywords iron steel getQuery
#' @return iron_steel_map global variable
#' @export
get_iron_steel_map = function() {
  iron_steel_map <<-
    getQuery(prj, "industry final energy by tech and fuel") %>%
    filter(sector == "iron and steel",
           input %in% c("wholesale gas", "refined liquids industrial", "delivered coal")) %>%
    mutate(max = max(value),
           save = if_else(value == max, 1, 0)) %>%
    filter(save == 1) %>%
    ungroup %>%
    select(-save, -max, -Units, -scenario, -value)
}


#' get_co2_iron_steel
#'
#' Get iron and steel CO2 emissions.
#' @keywords iron steel co2 getQuery
#' @return co2_tech_ironsteel global variable
#' @export
get_co2_iron_steel = function() {
  co2_tech_ironsteel <<-
    co2_tech_nobio %>% #Using redistributed bio version
    filter(sector == "iron and steel") %>%
    left_join(iron_steel_map, by = c("sector", "subsector", "technology", "year", "region")) %>%
    mutate(input = str_replace(input, "wholesale gas", "Emissions|CO2|Energy|Gas"),
           input = str_replace(input, "refined liquids industrial", "Emissions|CO2|Energy|Oil"),
           input = str_replace(input,	"delivered coal", "Emissions|CO2|Energy|Coal")) %>%
    rename(var = input) %>%
    mutate(value = value * conv_C_CO2) %>%
    group_by(scenario, region, year, var) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    na.omit() %>%
    select(all_of(long_columns))
}


#' get_lu_co2
#'
#' Get land use CO2 emissions.
#' @keywords lu co2 getQuery
#' @return LU_carbon_clean global variable
#' @export
get_lu_co2 = function() {
  LU_carbon_clean <<-
    # Land use CO2
    getQuery(prj, "LUC emissions by region") %>%
    # filter(scenario == "Reference") %>%
    filter(year %in% GCAM_years) %>%
    group_by(scenario, region, year) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    mutate(value = value * conv_C_CO2,
           var = "Emissions|CO2|AFOLU") %>%
    select(all_of(long_columns)) %>%
    # filter(scenario==scen1) %>%
    group_by(scenario,var,year)
}


#' get_co2_emissions
#'
#' Combine CO2 emission queries.
#' @keywords co2 process
#' @return co2_emissions_clean global variable
#' @export
get_co2_emissions = function() {
  co2_emissions_clean <<-
    bind_rows(co2_clean, LU_carbon_clean, co2_tech_emissions) %>%
    group_by(scenario, region, year, var) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    select(all_of(long_columns))
}

#' get_total_co2_emissions
#'
#' Compute total CO2 emission.
#' @keywords co2 process
#' @return tot_co2_clean global variable
#' @export
get_total_co2_emissions = function() {
  tot_co2_clean <<-
    bind_rows(co2_clean %>% filter(var == "Emissions|CO2|Energy and Industrial Processes"), LU_carbon_clean) %>%
    group_by(scenario, region, year) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    mutate(var = "Emissions|CO2") %>%
    select(all_of(long_columns))
}

#' get_nonco2_emissions
#'
#' Get non CO2 emissions query.
#' @keywords nonco2 getQuery
#' @return nonco2_clean global variable
#' @export
get_nonco2_emissions = function() {
  nonco2_clean <<-
    getQuery(prj, "nonCO2 emissions by sector") %>%
    left_join(nonco2_emis_sector_map, by = c("ghg", "sector")) %>%
    bind_rows(getQuery(prj, "nonCO2 emissions by resource production") %>%
                left_join(nonco2_emis_resource_map, by = c("ghg", "resource"))) %>%
    mutate(value = value * unit_conv) %>%
    group_by(scenario, region, year, var) %>% #
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup()%>%
    select(all_of(long_columns))
}

#' get_fgas
#'
#' Compute F-Gases emissions.
#' @keywords f-gases process
#' @return f_gas_clean global variable
#' @export
get_fgas = function() {
  f_gas_clean <<-
    getQuery(prj, "nonCO2 emissions by region") %>%
    conv_ghg_co2e() %>%
    filter(variable %in% F_GASES) %>%
    group_by(scenario, region, year) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    mutate(var = "Emissions|F-Gases") %>%
    select(all_of(long_columns))
}


#' get_ghg
#'
#' Get total GHG emissions.
#' @keywords ghg getQuery
#' @return ghg_clean global variable
#' @export
get_ghg = function() {
  ghg_clean <<-
    getQuery(prj, "nonCO2 emissions by region") %>%
    conv_ghg_co2e() %>%
    filter(variable %in% GHG_gases) %>%
    bind_rows(LU_carbon_clean) %>%
    group_by(scenario, region, year) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    mutate(var = "Emissions|Kyoto Gases") %>%
    select(all_of(long_columns))
}


#' get_ghg_sector
#'
#' Get sectorial GHG emissions.
#' @keywords ghg getQuery
#' @return ghg_sector_clean global variable
#' @export
get_ghg_sector = function() {
  ghg_sector_clean <<-
    getQuery(prj, "nonCO2 emissions by sector")  %>%
    filter(!grepl("CO2", ghg)) %>%
    bind_rows(getQuery(prj, "nonCO2 emissions by resource production") %>%
                rename(sector = resource) %>%
                select(-subresource)) %>%
    bind_rows(getQuery(prj, "CO2 emissions by sector (no bio)") %>%
                mutate(ghg = "CO2")) %>%
    mutate(subsector = sector) %>%
    conv_ghg_co2e() %>%
    filter(variable %in% GHG_gases) %>%
    rename(ghg = variable) %>%
    left_join(kyoto_sector_map) %>%
    select(all_of(long_columns)) %>%
    bind_rows(LU_carbon_clean %>%
                mutate(var = "Emissions|Kyoto Gases"),
              LU_carbon_clean %>%
                mutate(var = "Emissions|Kyoto Gases|AFOLU")) %>%
    group_by(scenario, region, var, year) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup()
}


#' get_co2_sequestration
#'
#' Get carbon sequestration.
#' @keywords co2 getQuery
#' @return co2_sequestration_clean global variable
#' @export
get_co2_sequestration = function() {
  co2_sequestration_clean <<-
    getQuery(prj, "CO2 sequestration by tech") %>%
    left_join(carbon_seq_tech_map, by = c("sector", "technology")) %>%
    complete(nesting(scenario,region, year),
             var = unique(var),
             fill = list(value =0)) %>%
    filter(!is.na(var)) %>% #, var!= "Carbon Sequestration|Feedstocks",var != "Carbon Sequestration|Feedstocks|Liquids") %>%
    mutate(value = value * unit_conv) %>%
    group_by(scenario, region, year, var) %>% #
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>% #spread(year, value) -> d
    select(all_of(long_columns))
}


# Agriculture and land use
# ==============================================================================================
#' get_ag_demand
#'
#' Get agricultural demand.
#' @keywords ag getQuery
#' @return ag_demand_clean global variable
#' @export
get_ag_demand = function() {
  ag_demand_clean <<-
    bind_rows(getQuery(prj, "demand balances by crop commodity"),
              getQuery(prj, "demand balances by meat and dairy commodity")) %>%
    left_join(ag_demand_map, by = c("sector")) %>%
    filter(!is.na(var)) %>%
    mutate(value = value * unit_conv) %>%
    group_by(scenario, region, year, var) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    select(all_of(long_columns))
}


#' get_ag_production
#'
#' Get agricultural production.
#' @keywords ag getQuery
#' @return ag_production_clean global variable
#' @export
get_ag_production = function() {
  ag_production_clean <<-
    getQuery(prj, "ag production by crop type") %>%
    filter(Units == "Mt") %>%
    # Forests produce in units of billion m3 and biomass produces in EJ. We'll need to find a conversion factor to include it
    # 1 m3 = .001 tons
    mutate(var = "Agricultural Production") %>%
    group_by(scenario, region, year, var) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    select(all_of(long_columns))
}


#' get_land
#'
#' Get land use area.
#' @keywords ag getQuery
#' @return land_clean global variable
#' @export
get_land = function() {
  land_clean <<-
    getQuery(prj, "aggregated land allocation") %>%
    left_join(land_use_map, by = c("landleaf")) %>%
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
#' @keywords energy getQuery
#' @return primary_energy_clean global variable
#' @export
get_primary_energy = function() {
  primary_energy_clean <<-
    getQuery(prj, "primary energy consumption with CCS by region (direct equivalent)") %>%
    filter(!grepl("water", fuel),
           Units == "EJ") %>%
    left_join(primary_energy_map, by = c("fuel")) %>%
    filter(!is.na(var)) %>%
    mutate(value = value * unit_conv) %>%
    group_by(scenario, region, year, var) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup()  %>%
    complete(nesting(scenario,region, year),
             var = unique(var),
             fill = list(value =0)) %>%
    select(all_of(long_columns))
}


#' get_energy_trade_prod
#'
#' Get energy trade.
#' @keywords energy getQuery
#' @return energy_trade_prod global variable
#' @export
get_energy_trade_prod = function() {
  energy_trade_prod <<-
    getQuery(prj, "resource production") %>%
    filter(Units == "EJ") %>%
    filter(resource %in% c("coal", "natural gas", "crude oil", "unconventional oil")) %>%
    mutate(resource = sub("crude oil", "oil", resource),
           resource = sub("unconventional oil", "oil", resource)) %>%
    group_by(scenario, resource, region, year) %>%
    summarise(production = sum(value)) %>%
    ungroup()
}


#' get_energy_trade_tmp
#'
#' Get energy trade supply. Query used to compute other variables.
#' @keywords energy getQuery tmp
#' @return energy_trade_supply global variable
#' @export
get_energy_trade_tmp = function() {
  energy_trade_supply <<-
    getQuery(prj, "supply of all markets") %>%
    filter(grepl("regional coal", market) | grepl("regional natural gas", market) | grepl("regional oil", market)) %>%
    separate(market, into = c("region", "resource"), sep = "regional ", fill = "right") %>%
    filter(resource != "oilpalm", resource != "oilcrop") %>%
    group_by(scenario, resource, region, year) %>%
    summarise(demand = sum(value)) %>%
    ungroup()
}


#' get_energy_trade
#'
#' Get energy trade supply.
#' @keywords energy getQuery tmp
#' @return energy_trade_clean global variable
#' @export
get_energy_trade = function() {
  energy_trade_clean <<-
    energy_trade_prod %>%
    left_join(energy_trade_supply, by = c("scenario", "resource", "region", "year")) %>%
    mutate(value = production - demand,
           resource = sub("coal", "Coal", resource),
           resource = sub("natural gas", "Gas", resource),
           resource = sub("oil", "Oil", resource),
           var = paste0("Trade|Primary Energy|", resource, "|Volume")) %>%
    select(all_of(long_columns))
}

# Secondary Energy
# ==============================================================================================
#' get_elec_gen_tech
#'
#' Get electricity generation
#' @keywords electricity getQuery
#' @return elec_gen_tech_clean global variable
#' @export
get_elec_gen_tech = function() {
  elec_gen_tech_clean <<-
    getQuery(prj, "elec gen by gen tech") %>%
    left_join(elec_gen_map, by = c("output", "subsector", "technology")) %>%
    filter(!is.na(var)) %>%
    mutate(value = value * unit_conv) %>%
    group_by(scenario, region, year, var) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    complete(nesting(scenario,region, year),
             var = unique(var),
             fill = list(value =0)) %>%
    select(all_of(long_columns))
}


#' get_secondary_solids
#'
#' Get secondary solids
#' @keywords energy getQuery
#' @return secondary_solids global variable
#' @export
get_secondary_solids = function() {
  secondary_solids <<-
    getQuery(prj,"inputs by tech") %>%
    filter(input %in% c("delivered biomass", "delivered coal")) %>%
    group_by(scenario, region, year, input) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    mutate(var = ifelse(input == "delivered biomass", "Secondary Energy|Solids|Biomass",
                        "Secondary Energy|Solids|Coal")) %>%
    bind_rows(getQuery(prj,"inputs by tech") %>%
                filter(input %in% c("delivered biomass", "delivered coal")) %>%
                group_by(scenario, region, year) %>%
                summarise(value = sum(value, na.rm = T)) %>%
                ungroup() %>%
                mutate(var = "Secondary Energy|Solids")) %>%
    select(all_of(long_columns))
}


#' get_se_gen_tech
#'
#' Get other secondary energy production
#' @keywords energy getQuery
#' @return se_gen_tech_clean global variable
#' @export
get_se_gen_tech = function() {
  se_gen_tech_clean <<-
    bind_rows(getQuery(prj, "gas production by tech"),
              getQuery(prj, "hydrogen production by tech"),
              getQuery(prj, "refined liquids production by tech")) %>%
    left_join(se_gen_map, by = c("sector", "subsector", "technology")) %>%
    filter(!is.na(var)) %>%
    mutate(value = value * unit_conv) %>%
    group_by(scenario, region, year, var) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    complete(nesting(scenario,region, year),
             var = unique(var),
             fill = list(value =0)) %>%
    select(all_of(long_columns)) %>%
    bind_rows(secondary_solids)
}


# Final Energy
# ==============================================================================================
# demand by sector by technology

#' get_fe_sector_tmp
#'
#' Get final energy demand by sector
#' @keywords energy getQuery tmp
#' @return fe_sector global variable
#' @export
get_fe_sector_tmp = function() {
  fe_sector <<-
    getQuery(prj, "final energy consumption by sector and fuel") %>%
    left_join(final_energy_map, by = c("sector", "input")) %>%
    filter(!is.na(var)) %>%
    mutate(value = value * unit_conv) %>%
    group_by(scenario, region, year, var) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    complete(nesting(scenario,region, year),
             var = unique(var),
             fill = list(value =0)) %>%
    select(all_of(long_columns))
}


#' get_fe_transportation_tmp
#'
#' Get mode-specific transport final energy to break out rail, ship, and domestic air.
#' @keywords energy getQuery tmp
#' @return fe_transportation global variable
#' @export
get_fe_transportation_tmp = function() {
  fe_transportation <<-
    getQuery(prj, "transport final energy by mode and fuel") %>%
    left_join(transport_final_en_map, by = c("sector", "input", "mode")) %>%
    filter(!is.na(var)) %>%
    mutate(value = value * unit_conv) %>%
    group_by(scenario, region, year, var) %>%
    summarise(value = sum(value,
                          na.rm = T)) %>%
    ungroup() %>%
    complete(nesting(scenario,region, year),
             var = unique(var),
             fill = list(value =0)) %>%
    select(all_of(long_columns))
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
    bind_rows(fe_sector, fe_transportation) %>%
    group_by(scenario, region, var, year) %>%
    summarise(value = sum(value)) %>%
    ungroup()
}


# Energy Service ----------------------------------------------------------

#' get_energy_service_transportation
#'
#' Get transport.
#' @keywords energy getQuery
#' @return energy_service_transportation_clean global variable
#' @export
get_energy_service_transportation = function() {
  energy_service_transportation_clean <<-
    getQuery(prj, "transport service output by mode") %>%
    left_join(transport_en_service, by = c("sector", "mode")) %>%
    filter(!is.na(var)) %>%
    mutate(value = value * unit_conv) %>%
    group_by(scenario, region, year, var) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    select(all_of(long_columns))
}


#' get_energy_service_buildings
#'
#' Get ES buildings.
#' @keywords energy getQuery
#' @return energy_service_buildings_clean global variable
#' @export
get_energy_service_buildings = function() {
  energy_service_buildings_clean <<-
    getQuery(prj, "building floorspace") %>%
    left_join(buildings_en_service, by = c("building")) %>%
    filter(!is.na(var)) %>%
    mutate(value = value * unit_conv) %>%
    group_by(scenario, region, year, var) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    select(all_of(long_columns))
}



# industry production
# could add chemicals but they're in terms of EJ, need to also add cement

#' get_industry_production
#'
#' Get industry production.
#' @keywords energy getQuery
#' @return industry_production_clean global variable
#' @export
get_industry_production = function() {
  industry_production_clean <<-
    getQuery(prj, "industry primary output by sector") %>%
    left_join(production_map, by = c("sector")) %>%
    # filter variables that are in terms of Mt
    filter(var %in% c("Production|Cement", "Production|Steel", "Production|Non-ferrous metals"))%>%
    group_by(scenario, region, var, year) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    select(all_of(long_columns))
}


# Prices
# ==============================================================================================
#' get_ag_prices_wld_tmp
#'
#' Get ag price index.
#' @keywords ag getQuery tmp
#' @return ag_prices_wld global variable
#' @export
get_ag_prices_wld_tmp = function() {
  ag_prices_wld <<-
    getQuery(prj, "prices by sector") %>%
    left_join(ag_prices_map, by = c("sector")) %>%
    filter(!is.na(var)) %>%
    group_by(scenario, sector, year) %>%
    summarise(value = mean(value, na.rm = T)) %>%
    ungroup() %>%
    mutate(region = "World")
}

#' get_ag_prices
#'
#' Calculate average mean for ag global index
#' @keywords ag getQuery
#' @return ag_prices_clean global variable
#' @export
get_ag_prices = function() {
  ag_prices_clean <<-
    getQuery(prj, "prices by sector") %>%
    bind_rows(ag_prices_wld) %>%
    left_join(ag_prices_map, by = c("sector")) %>%
    filter(!is.na(var)) %>%
    group_by(scenario, region, sector) %>%
    mutate(value = value * unit_conv / value[year == 2005]) %>%
    ungroup() %>%
    select(all_of(long_columns))
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
    c("Price|Carbon",
      "Price|Carbon|Supply",
      "Price|Carbon|Demand|Residential and Commercial",
      "Price|Carbon|Demand|Transportation",
      "Price|Carbon|Demand|Industry")
}


#' get_regions_tmp
#'
#' Get regions to compute carbon price.
#' @keywords internal tmp process
#' @return regions global variable
#' @export
get_regions_tmp = function() {
  regions <<-
    CO2_market$region
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
    filter(var == "Final Energy") %>%
    group_by(scenario, year) %>%
    mutate(weight = value/sum(value)) %>%
    ungroup() %>%
    select(-value, -var)
}


#' get_co2_price_global_tmp
#'
#' Get global co2 price.
#' @keywords co2 getQuery tmp
#' @return co2_price_global & regions global variable
#' @export
get_co2_price_global_tmp = function() {
  co2_price_global <<-
    getQuery(prj, "CO2 prices") %>%
    #calculate for all scenarios except for d_delfrag, d_rap
    filter(market == "GlobalCO2") %>%
    mutate(value = value / conv_C_CO2 * conv_90USD_10USD,
           var = "Price|Carbon|Supply") %>%
    complete(nesting(scenario, value, year),
             region = regions,
             var = unique(price_var)) %>%
    select(all_of(long_columns))
}


#' get_co2_price_fragmented_tmp
#'
#' Get fragmented co2 price.
#' @keywords co2 getQuery tmp
#' @return co2_price_fragmented global variable
#' @export
get_co2_price_fragmented_tmp = function() {
  co2_price_fragmented <<-
    getQuery(prj, "CO2 prices") %>%
    filter(!grepl("LUC", market)) %>%
    left_join(CO2_market, by = c("market")) %>%
    filter(complete.cases(.)) %>%
    # get global carbon price by weighing regional price by 2020 GHG emisisons
    bind_rows(getQuery(prj, "CO2 prices") %>%
                filter(!grepl("LUC", market)) %>%
                left_join(CO2_market, by = c("market")) %>%
                left_join(region_weight, by = c("scenario", "region", "year")) %>%
                mutate(value = weight * value) %>%
                group_by(scenario, year) %>%
                summarise(value = sum(value, na.rm=T)) %>%
                ungroup() %>%
                mutate(region = "Global"))  %>%
    mutate(value = value / conv_C_CO2 * conv_90USD_10USD,
           var = "Price|Carbon") %>%
    # apply to carbon price for all energy sectors
    complete(nesting(scenario,value, year, region),
             var = unique(price_var)) %>%
    select(all_of(long_columns))
}


#' get_co2_price
#'
#' Get co2 price.
#' @keywords co2 getQuery
#' @return co2_price_clean global variable
#' @export
get_co2_price = function() {
  co2_price_clean <<-
    bind_rows(co2_price_fragmented)
}


#' get_gov_revenue_sector
#'
#' Get overall carbon revenue.
#' @keywords revenue getQuery
#' @return gov_revenue_sector global variable
#' @export
get_gov_revenue_sector = function() {
  gov_revenue_sector <<-
    co2_clean %>%
    mutate(sector  = ifelse(var == "Emissions|CO2|Energy|Demand|Industry", "Carbon|Demand|Industry", NA ),
           sector  = ifelse(var == "Emissions|CO2|Energy|Demand|Residential and Commercial", "Carbon|Demand|Buildings", sector ),
           sector  = ifelse(var == "Emissions|CO2|Energy|Demand|Transportation", "Carbon|Demand|Transport", sector ),
           sector  = ifelse(var == "Emissions|CO2|Energy|Supply|Electricity", "Carbon|Supply", sector ),
           emiss = value) %>%
    select(-var, -value) %>%
    filter(!is.na(sector)) %>%
    left_join(co2_price_clean %>%
                mutate(sector  = ifelse(var == "Price|Carbon|Demand|Industry", "Carbon|Demand|Industry", NA ),
                       sector  = ifelse(var == "Price|Carbon|Demand|Residential and Commercial", "Carbon|Demand|Buildings", sector ),
                       sector  = ifelse(var == "Price|Carbon|Demand|Transportation", "Carbon|Demand|Transport", sector ),
                       sector  = ifelse(var == "Price|Carbon|Supply", "Carbon|Supply", sector )) %>%
                select(-var),
              by = c("scenario", "region", "sector", "year")) %>%
    mutate(value = value * emiss,
           var = paste0("Revenue|Government|Tax|", sector))
}

#' get_gov_revenue_all
#'
#' Get overall carbon revenue.
#' @keywords revenue getQuery
#' @return gov_revenue_all global variable
#' @export
get_gov_revenue_all = function() {
  gov_revenue_all <<-
    gov_revenue_sector %>%
    bind_rows(gov_revenue_sector %>%
                group_by(scenario, region, year) %>%
                summarise(value = sum(value, na.rm =T))%>%
                ungroup() %>%
                mutate(var = "Revenue|Government|Tax|Carbon",)) %>%
    mutate(value = value/1000) %>%
    select(all_of(long_columns))
}


#' get_prices_subsector
#'
#' Get energy prices - primary, secondary, and final
#' @keywords prices getQuery
#' @return prices_subsector global variable
#' @export
get_prices_subsector = function() {
  prices_subsector <<-
    getQuery(prj, "prices by sector") %>%
    left_join(energy_prices_map %>%
                filter(is.na(subsector)) %>%
                unique, by = c("sector")) %>%
    bind_rows(getQuery(prj, "costs by subsector") %>%
                left_join(energy_prices_map %>%
                            unique,
                          by = c("sector", "subsector"))) %>%
    filter(!is.na(var)) %>%
    # read in carbon content in kg C per GJ -> convert to tC per GJ
    left_join(carbon_content %>%
                filter(grepl("biomass", PrimaryFuelCO2Coef.name)),
              by = c("region", "sector" = "PrimaryFuelCO2Coef.name")) %>%
    mutate(PrimaryFuelCO2Coef = PrimaryFuelCO2Coef / 1000) %>%
    replace_na(list(PrimaryFuelCO2Coef = 0))
}


#' get_energy_price_fragmented
#'
#' Get energy prices fragmented, join by region since price is different.
#' @keywords prices process
#' @return energy_price_fragmented global variable
#' @export
get_energy_price_fragmented = function() {
  energy_price_fragmented <<-
    prices_subsector %>%
    filter(!is.na(var)) %>%
    left_join(getQuery(prj, "CO2 prices") %>%
                filter(!grepl("LUC", market)) %>%
                left_join(CO2_market, by = c("market")) %>%
                select(scenario, region, year, price_C = value), by = c("scenario", "region", "year")) %>%
    replace_na(list(price_C = 0)) %>%
    # remove carbon price (subsidy) 1990$/tC from biomass 1975$/GJ
    mutate(price_C = PrimaryFuelCO2Coef * price_C * conv_90USD_10USD,
           value = value * unit_conv * conv_75USD_10USD + price_C) %>%
    select(all_of(long_columns))
}

#' get_total_revenue
#'
#' Compute total revenue: total production * global price.
#' @keywords revenue getQuery
#' @return total_revenue global variable
#' @export
get_total_revenue = function() {
  total_revenue <<-
    getQuery(prj, "resource production") %>%
    filter(resource %in% c("coal", "crude oil", "natural gas")) %>%
    group_by(scenario, resource, year) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    rename(total_production = value) %>%
    left_join(getQuery(prj, "prices by sector") %>%
                filter(sector %in% c("regional coal", "regional oil", "regional natural gas")) %>%
                mutate(resource = ifelse(sector == "regional coal", "coal", NA),
                       resource = ifelse(sector == "regional oil", "crude oil", resource),
                       resource = ifelse(sector == "regional natural gas", "natural gas", resource)) %>%
                rename(resource_price = value) %>%
                group_by(scenario, resource, year) %>%
                summarise(resource_price = mean(resource_price, na.rm = T)) %>%
                ungroup(),
              by = c("scenario", "year", "resource")) %>%
    mutate(total_production = total_production * GJ_to_EJ,
           total_revenue = total_production * resource_price * conv_75USD_10USD)
}

#' get_regional_emission
#'
#' Compute regional nonCO2 emission: regional production * nonCO2 coef.
#' @keywords nonco2 getQuery
#' @return regional_emission global variable
#' @export
get_regional_emission = function() {
  regional_emission <<-
    nonCO2_content %>%
    filter(year == 2005) %>%
    spread(Non.CO2, emiss.coef) %>%
    rename(CH4.coef =  CH4,
           N2O.coef = N2O) %>%
    mutate(CH4.coef =  CH4.coef/1000000,
           N2O.coef =  N2O.coef/1000000) %>%
    select(region, resource, CH4.coef, N2O.coef) %>%
    left_join(getQuery(prj, "resource production")%>%
                filter(resource %in% c("coal", "crude oil", "natural gas")) %>%
                rename(regional_production = value), by = c("region", "resource")) %>%
    mutate(regional_CH4emission = regional_production *CH4.coef * GJ_to_EJ,
           regional_N2Oemission = regional_production *N2O.coef * GJ_to_EJ)
}


#' get_energy_price_tmp
#'
#' Bind regional oil, gas, coal prices with other energy prices
#' @keywords price getQuery tmp
#' @return energy_price global variable
#' @export
get_energy_price_tmp = function() {
  energy_price <<-
    energy_price_fragmented  %>%
    select(all_of(long_columns))
}
#energy_price_global-> energy_price

#' get_energy_price
#'
#' Compute final energy price
#' @keywords price process
#' @return energy_price_clean global variable
#' @export
get_energy_price = function() {
  energy_price_clean <<-
    energy_price %>%
    filter(grepl("Residential\\|Electricity", var)|
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
    mutate(var = paste(var, "Index", sep = "|")) %>%
    group_by(scenario, region, var) %>%
    mutate(value = value/ value[year == 2020]) %>%
    ungroup() %>%
    select(all_of(long_columns)) %>%
    bind_rows(energy_price)

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
#' @keywords capacity process tmp
#' @return cf_iea global variable
#' @export
get_cf_iea_tmp = function() {
  cf_iea <<-
    elec_gen_tech_clean %>%
    filter(year == 2020, scenario == unique(elec_gen_tech_clean$scenario)[1]) %>%
    group_by(var) %>%
    summarise(EJ = sum(value, na.rm = T)) %>%
    ungroup() %>%
    left_join(iea_capacity %>%
                filter(period == 2020, scenario == "Current Policies Scenario") %>%
                mutate(variable = gsub("Capacity\\|Electricity\\|CSP", "Capacity\\|Electricity\\|Solar\\|CSP", variable),
                       variable = gsub("Capacity\\|Electricity\\|Biomass", "Capacity\\|Electricity\\|Biomass\\|w/o CCS", variable),
                       variable = gsub("Capacity\\|Electricity\\|Coal", "Capacity\\|Electricity\\|Coal\\|w/o CCS", variable),
                       variable = gsub("Capacity\\|Electricity\\|Gas", "Capacity\\|Electricity\\|Gas\\|w/o CCS", variable),
                       variable = gsub("Capacity\\|Electricity\\|Oil", "Capacity\\|Electricity\\|Oil\\|w/o CCS", variable),
                       variable = gsub("Capacity", "Secondary Energy", variable)),
              by = c("var" = "variable")) %>%
    mutate(cf = EJ / (value * hr_per_yr * EJ_to_GWh),
           cf = replace(cf, cf > 1, 0.99)) %>%
    filter(!is.na(cf), !var %in% c("Secondary Energy|Electricity", "Secondary Energy|Electricity|Non-Biomass Renewables")) %>%
    left_join(capacity_map, by = "var") %>%
    select(technology, cf) %>%
    mutate(region = "USA", vintage = 2020) %>%
    complete(nesting(technology, cf),
             vintage = c(1990, seq(2005, 2020, by = 5)),
             region = unique(cf_rgn$region))
}

#' get_elec_cf_tmp
#'
#' Calculate future capacity using GCAM
#' @keywords capacity process tmp
#' @return elec_cf global variable
#' @export
get_elec_cf_tmp = function() {
  elec_cf <<-
    cf_gcam %>%
    select(technology, cf = X2100) %>%
    mutate(region = "USA", vintage = 2025) %>%
    complete(nesting(technology, cf),
             vintage = seq(2025, 2100, by = 5),
             region = unique(cf_rgn$region)) %>%
    # first, replace regional cf for wind and solar
    left_join(cf_rgn  %>%
                select(region, technology = stub.technology, vintage = year, cf.rgn = capacity.factor),
              by = c("technology", "vintage", "region")) %>%
    mutate(cf = replace(cf, !is.na(cf.rgn), cf.rgn[!is.na(cf.rgn)])) %>%
    # second, use iea capacity consistent cf for existing vintage
    bind_rows(cf_iea) %>%
    complete(nesting(technology, region), vintage = c(1990, seq(2005, 2100, by = 5))) %>%
    group_by(technology, region) %>%
    mutate(cf = approx_fun(vintage, cf, rule = 2)) %>%
    ungroup() %>%
    filter(!technology %in% c("existing coal", "add CCS retrofit"))
}

#' get_elec_capacity_tot
#'
#' Calculate electricity total capacity
#' @keywords capacity process
#' @return elec_capacity_tot_clean global variable
#' @export
get_elec_capacity_tot = function() {
  elec_capacity_tot_clean <<-
    getQuery(prj, "elec gen by gen tech and cooling tech and vintage") %>%
    filter(!output %in% c("electricity", "elect_td_bld" )) %>%
    separate(technology, into = c("technology", "vintage"), sep = ",") %>%
    mutate(vintage = as.integer(sub("year=", "", vintage)),
           output = gsub("elec_", "", output)) %>%
    group_by(scenario, region, technology = output, vintage, year) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    bind_rows(getQuery(prj, "elec gen by gen tech and cooling tech and vintage") %>%
                filter(output %in% c("electricity", "elect_td_bld" )) %>%
                separate(technology, into = c("technology", "vintage"), sep = ",") %>%
                mutate(vintage = as.integer(sub("year=", "", vintage))) %>%
                group_by(scenario, region, technology, vintage, year) %>%
                summarise(value = sum(value, na.rm = T)) %>%
                ungroup) %>%
    left_join(elec_cf, by = c("region", "technology", "vintage")) %>%
    mutate(EJ = value) %>%
    conv_EJ_GW() %>%
    group_by(scenario, region, technology, year) %>%
    summarise(value = sum(gw, na.rm = T)) %>%
    ungroup %>%
    left_join(capacity_map %>% select(-output), by = c("technology")) %>%
    filter(!is.na(var)) %>%
    mutate(value = value * unit_conv,
           var = sub("Secondary Energy", "Capacity", var)) %>%
    group_by(scenario, region, var, year) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup %>%
    complete(nesting(scenario,region, year),
             var = unique(var),
             fill = list(value =0)) %>%
    select(all_of(long_columns))
}

#' get_elec_capacity_add_tmp
#'
#' Calculate added total capacity
#' @keywords capacity process tmp
#' @return elec_capacity_add global variable
#' @export
get_elec_capacity_add_tmp = function() {
  elec_capacity_add <<-
    getQuery(prj, "elec gen by gen tech and cooling tech and vintage") %>%
    filter(!output %in% c("electricity", "elect_td_bld" )) %>%
    separate(technology, into = c("technology", "vintage"), sep = ",") %>%
    mutate(vintage = as.integer(sub("year=", "", vintage)),
           output = gsub("elec_", "", output)) %>%
    group_by(scenario, region, technology = output, vintage, year) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup() %>%
    bind_rows(getQuery(prj, "elec gen by gen tech and cooling tech and vintage") %>%
                filter(output %in% c("electricity", "elect_td_bld" )) %>%
                separate(technology, into = c("technology", "vintage"), sep = ",") %>%
                mutate(vintage = as.integer(sub("year=", "", vintage))) %>%
                group_by(scenario, region, technology, vintage, year) %>%
                summarise(value = sum(value, na.rm = T)) %>%
                ungroup) %>%
    filter(year == vintage, year > 2015) %>%
    group_by(scenario, region, technology, year) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup %>%
    # use GCAM cf for capacity additions
    left_join(elec_cf %>%
                filter(!technology %in% c("coal (conv pul)")) %>%
                group_by(region, technology) %>%
                mutate(cf = replace(cf, vintage < 2025, cf[vintage == 2025])) %>%
                ungroup %>%
                bind_rows(elec_cf %>%
                            filter(technology %in% c("coal (conv pul)" ))),
              by = c("region", "technology", "year" = "vintage")) %>%
    # use average annual additions
    mutate(EJ = value / 5) %>%
    conv_EJ_GW() %>%
    group_by(scenario, region, technology, year) %>% #
    summarise(GW = sum(gw, na.rm = T), EJ = sum(EJ, na.rm = T)) %>%
    ungroup
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
    left_join(capacity_map %>% select(-output), by = c("technology")) %>%
    filter(!var %in% c("Secondary Energy|Electricity|Hydro",
                       "Secondary Energy|Electricity|Storage Capacity")) %>%
    mutate(value = GW * unit_conv,
           var = sub("Secondary Energy", "Capacity Additions", var)) %>%
    bind_rows(elec_capacity_add%>%
                left_join(capacity_map %>% select(-output), by = c("technology")) %>%
                filter(var == "Secondary Energy|Electricity|Storage Capacity") %>%
                mutate(value = GW * 8760, # multiply by # of hours in a year
                       var = sub("Secondary Energy", "Capacity Additions", var))) %>%
    group_by(scenario, region, var, year) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup %>%
    complete(nesting(scenario,region, year),
             var = unique(var),
             fill = list(value =0)) %>%
    select(all_of(long_columns))
}


#' get_elec_capital
#'
#' Calculate electricity capital
#' @keywords capital process
#' @return elec_capital_clean global variable
#' @export
get_elec_capital = function() {
  # Capital costs from GCAM in $1975/kw -> convert to $2010/kw
  elec_capital <-
    capital_gcam %>%
    mutate(region = "USA", scenario = Scenarios[1]) %>%
    select(-sector) %>%
    complete(nesting(subsector, technology, year, capital.overnight),
             region = unique(cf_rgn$region), scenario = Scenarios) %>%
    # gw * 10e6 * $/kw / 10e9 = bill$
    mutate(value = capital.overnight * conv_75USD_10USD) %>%
    left_join(elec_gen_map %>% select(-output), by = c("subsector", "technology"))

  elec_capital_clean <<-
    elec_capital %>%
    filter(!is.na(var), var != "Secondary Energy|Electricity|Electricity Storage") %>%
    mutate(value = value * unit_conv,
           var = sub("Secondary Energy", "Capital Cost", var)) %>%
    group_by(scenario, region, var, year) %>%
    summarise(value = mean(value, na.rm = T)) %>%
    ungroup %>%
    select(all_of(long_columns))

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
#' @keywords capital process
#' @return elec_investment_clean global variable
#' @export
get_elec_investment = function() {
  elec_investment_clean <<-
    # Electricity investment = annual capacity additions * capital costs
    elec_capacity_add %>%
    left_join(capital_gcam %>%
                mutate(capital.overnight = replace(capital.overnight, technology=="wind_storage", capital.overnight[technology == "wind"]*.484),
                       capital.overnight = replace(capital.overnight, technology=="CSP_storage", 760 * conv_19USD_75USD),
                       capital.overnight = replace(capital.overnight, technology=="PV_storage", capital.overnight[technology == "PV"]*.518)),
              by = c("technology", "year")) %>%
    # gw * 10e6 * $/kw / 10e9 = bill$
    mutate(value = GW * capital.overnight / 1000 * conv_75USD_10USD) %>%
    left_join(elec_gen_map %>% select(-output), by = c("technology")) %>%
    filter(!is.na(var)) %>%
    mutate(value = value * unit_conv,
           var = sub("Secondary Energy", "Investment|Energy Supply", var)) %>%
    group_by(scenario, region, var, year) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup %>%
    select(all_of(long_columns))
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
    filter(Region == "World", Variable == "Energy Supply|Electricity|Transmission and Distribution", year == 2020) %>%
    mutate(value = value * conv_15USD_10USD) %>%
    summarise(value = mean(value, na.rm = T)) %>% unlist

  transmission_invest2020 <-
    elec_capacity_add_clean %>%
    filter(var == "Capacity Additions|Electricity", year == 2020) %>%
    group_by(scenario) %>%
    mutate(share = value / sum(value, na.rm = T),
           invest = share * transmission2020)

  transmission_invest_clean <<-
    elec_capacity_add_clean %>%
    filter(var == "Capacity Additions|Electricity") %>%
    group_by(scenario, region) %>%
    mutate(rate = value / value[year == 2020]) %>%
    left_join(transmission_invest2020 %>%
                select(scenario, region, invest),
              by = c("scenario", "region")) %>%
    mutate(value = rate * invest, var = "Investment|Energy Supply|Electricity|Transmission and Distribution") %>%
    select(all_of(long_columns)) #%>%  group_by(scenario, year) %>% summarise(value = sum(value)) %>% spread(year, value)
}


#' get_CCS_invest
#'
#' Calculate CSS investment
#' @keywords investment process
#' @return CCS_invest_clean global variable
#' @export
get_CCS_invest = function() {
  CCS2040 <-
    investment %>%
    filter(Region == "World", Variable == "CCS", year == 2040) %>%
    mutate(value = value * conv_15USD_10USD) %>%
    summarise(value = mean(value, na.rm = T)) %>% unlist

  CCS_invest2040 <-
    co2_sequestration_clean %>%
    filter(var == "Carbon Sequestration|CCS", year == 2040) %>%
    group_by(scenario) %>%
    mutate(share = value / sum(value, na.rm = T),
           invest = share * CCS2040 )

  CCS_invest_clean <<-
    co2_sequestration_clean %>%
    filter(var == "Carbon Sequestration|CCS") %>%
    group_by(scenario, region) %>%
    mutate(rate = value / value[year == 2040]) %>%
    left_join(CCS_invest2040 %>%
                select(scenario, region, invest),
              by = c("scenario", "region")) %>%
    mutate(value = rate * invest, var = "Investment|Energy Supply|CO2 Transport and Storage") %>%
    select(all_of(long_columns))
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
    getQuery(prj, "resource production by tech and vintage") %>%
    filter(resource %in% c("coal", "natural gas", "crude oil", "unconventional oil")) %>%
    separate(technology, into = c("technology", "vintage"), sep = ",") %>%
    mutate(vintage = as.integer(sub("year=", "", vintage))) %>%
    filter(year > 2010) %>%
    mutate(resource = sub("crude oil", "oil", resource),
           resource = sub("unconventional oil", "oil", resource)) %>%
    group_by(scenario, resource, region, year) %>%
    summarise(production = sum(value, na.rm = T)) %>%
    ungroup

  # scale 2015 number - average of other model results from Mcollion et al. 2018
  extraction2015 <-
    investment %>%
    filter(Region == "World", Variable == "Extraction and Conversion - Fossil Fuels", year == 2015) %>%
    mutate(value = value * conv_15USD_10USD) %>%
    summarise(value = mean(value, na.rm = T)) %>% unlist

  extraction2020 <-
    investment %>%
    filter(Region == "World", Variable == "Extraction and Conversion - Fossil Fuels", year == 2020) %>%
    mutate(value = value * conv_15USD_10USD) %>%
    summarise(value = mean(value, na.rm = T)) %>% unlist

  resource_investment2015 <-
    resource_addition %>%
    filter(year == 2015) %>%
    left_join(getQuery(prj, "regional primary energy prices") %>%
                mutate(fuel = sub("regional ", "", fuel)),
              by = c("scenario", "region", "year", "resource" = "fuel")) %>%
    mutate(value = production * value) %>%
    group_by(scenario, resource, region) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup %>%
    group_by(scenario) %>%
    mutate(share = value / sum(value, na.rm = T),
           invest = share * extraction2015) %>%
    ungroup

  resource_investment2020 <-
    resource_addition %>%
    filter(year == 2020) %>%
    left_join(getQuery(prj, "regional primary energy prices") %>%
                mutate(fuel = sub("regional ", "", fuel)),
              by = c("scenario", "region", "year", "resource" = "fuel")) %>%
    mutate(value = production * value) %>%
    group_by(scenario, resource, region) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup %>%
    group_by(scenario) %>%
    mutate(share = value / sum(value, na.rm = T),
           invest = share * extraction2020) %>%
    ungroup

  resource_investment <-
    resource_addition %>%
    filter(year!= 2020) %>%
    group_by(scenario, resource) %>%
    mutate(rate = production / production[year == 2015 & region == "China"]) %>%
    ungroup %>%
    left_join(resource_investment2015 %>%
                filter(region == "China") %>%
                select(scenario, resource, invest),
              by = c('scenario', 'resource')) %>%
    bind_rows(resource_addition %>%
                filter(year == 2020) %>%
                group_by(scenario, resource) %>%
                mutate(rate = production / production[region == "China"]) %>%
                ungroup %>%
                left_join(resource_investment2020 %>%
                            filter(region == "China") %>%
                            select(scenario, resource, invest),
                          by =c('scenario', 'resource'))) %>%
    mutate(value = invest * rate,
           resource = sub("coal", "Coal", resource),
           resource = sub("natural gas", "Gas", resource),
           resource = sub("oil", "Oil", resource),
           var = paste0('Investment|Energy Supply|Extraction|', resource)) %>%
    select(all_of(long_columns))

  resource_investment_clean <<-
    resource_investment %>%
    group_by(scenario, region, year) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup %>%
    mutate(var = 'Investment|Energy Supply|Extraction|Fossil') %>%
    select(all_of(long_columns)) %>%
    bind_rows(resource_investment) #%>% group_by(scenario, var, year) %>% summarise(value = sum(value)) %>% spread(year, value)
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
    bind_rows(lapply(vars, function(x) get(x))) %>%
    mutate(region = gsub("Global", "World", region),
           region = gsub("global", "World", region))

  # Calculate global total
  GCAM_DATA_WORLD <-
    GCAM_DATA %>%
    filter(region != "World", # excl. Temperature|Forcing|Concentration
           # excl. price and costs variables - already calculated global average
           !grepl("Price|Capital Cost", var)) %>%
    group_by(scenario, year, var) %>%
    summarise(value = sum(value, na.rm = T)) %>%
    ungroup %>%
    mutate(region = "World")

  GCAM_DATA_wGLOBAL <-
    GCAM_DATA_WORLD %>%
    bind_rows(GCAM_DATA %>% filter(!(region == "World" & var %in% unique(GCAM_DATA_WORLD$var)))) %>%
    complete(nesting(scenario, region, var), year = reporting_years) %>%
    replace_na(list(value = 0))

  # Filter to final_db_year
  GCAM_DATA_wGLOBAL <- GCAM_DATA_wGLOBAL %>% filter(year <= final_db_year)


  final_data <<-
    template %>%
    inner_join(GCAM_DATA_wGLOBAL %>%
                 na.omit %>%
                 spread(year, value), by = c("Variable" = "var")) %>%
    #  left_join(reporting_scen %>% select(GCAM_scenario, Scenario),
    #            by = c("scenario" = "GCAM_scenario")) %>%
    rename(Region = region) %>%
    #  rename(Model = ?..Model) %>%
    rename(Scenario = scenario) %>%
    select(reporting_columns_fin) %>%
    filter(!is.na(Region)) # Drop variables we don't report

  write.csv(final_data, file.path(out_dir, "final_data.csv"), row.names = FALSE)
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
  # check global total is zero
  trade <- energy_trade_prod %>%
    left_join(energy_trade_supply, by = c("scenario", "resource", "region", "year")) %>%
    group_by(scenario, resource, year) %>%
    summarise(production = sum(production, na.rm = T),
              demand = sum(demand, na.rm = T)) %>%
    ungroup() %>%
    mutate(diff = (production - demand) / production,
           check = if_else( abs(diff) > 1, "Error", "OK" ))

  if (nrow(trade %>% filter( check == "Error") > 0)) {
    return('Trade flows: ERROR')
  }
  return('Trade flows: OK')

}


#' do_check_vetting
#'
#' Verify vetting and produce plot.
#' @keywords check
#' @return Verification message indicating if the process was successful.
#' @export
do_check_vetting = function() {
  # Check vetting results from SM
  global_vet_values <- read.csv(paste0(vet_dir, "/global_vet_values.csv"))

  final_data_long_check <- final_data %>%
    gather(year, value, -Model, -Variable, -Unit, -Scenario, -Region) %>%
    rename(region = Region,
           variable = Variable) %>%
    filter(Scenario == Scenarios[1]) %>%
    mutate(year = as.integer(year))

  check_vet <- global_vet_values %>%
    select(variable = adj_var, adj_var2, region, year, value, unit, range = Range) %>%
    rename(unit_vet = unit,
           value_vet = value) %>%
    left_join(final_data_long_check, by = c("variable", "region", "year")) %>%
    mutate(value = if_else(grepl("Traditional", variable), value * -1, value)) %>%
    select(Scenario, variable = adj_var2, region, year, value, unit = Unit, value_vet, unit_vet, range) %>%
    # Adjust for Solar&Wind and biomass
    group_by(Scenario, variable, region, year, unit, unit_vet, range) %>%
    summarise(value = sum(value),
              value_vet = mean(value_vet)) %>%
    ungroup() %>%
    mutate(value_vet = if_else(unit_vet == "bcm", value_vet * bcm_to_EJ, value_vet),
           unit_vet = if_else(unit_vet == "bcm", "EJ/yr", unit_vet)) %>%
    mutate(diff = (value / value_vet) - 1,
           check = if_else(abs(diff) > range, "Check", "OK"))

  check_vet_summary <- check_vet %>% filter(check == "Check")

  ## plot
  check_vet_plot <- check_vet %>%
    select(-year, -range, -diff, -check, -unit_vet) %>%
    gather(type, value, -variable, -Scenario, -unit, -region)

  ggplot(check_vet_plot, aes(x = variable, y = value, fill = type)) +
    geom_bar(stat = "identity", position = "dodge") +
    facet_wrap(~variable, scales = "free") +
    labs(x = "", y = "value") +
    theme_classic()+
    theme(axis.text.x = element_blank(),
          legend.position = "bottom",
          strip.text = element_text(size = 5),
          legend.title = element_blank())
  ggsave(paste0(fig_dir, "/vetting.tiff"), last_plot(), "tiff", dpi = 200)

  if(nrow(check_vet_summary > 0)){
    return('Vetting variables: ERROR')
  }
  return('Vetting variables: OK')

}

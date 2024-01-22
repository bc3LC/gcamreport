# Converting raw data into package data
library(usethis)
library(magrittr)

### paths
rawDataFolder = here::here()

# emissions considered nonCO2
emissions_list = c('BC','BC_AWB','C2F6','CF4','CH4','CH4_AGR','CH4_AWB','CO','CO2','CO_AWB','CO2_ETS',
                   'H2','H2_AWB','HFC125','HFC134a','HFC143a','HFC152a','HFC227ea','HFC23','HFC236fa',
                   'HFC245fa','HFC32','HFC365mfc','HFC43','N2O','N2O_AGR','N2O_AWB','NH3','NH3_AGR',
                   'NH3_AWB','NMVOC','NMVOC_AGR','NMVOC_AWB','NOx','NOx_AGR','NOx_AWB','OC','OC_AWB',
                   'PM10','PM2.5','SF6','SO2_1','SO2_1_AWB','SO2_2','SO2_2_AWB','SO2_3','SO2_3_AWB',
                   'SO2_4','SO2_4_AWB')
use_data(emissions_list, overwrite=T)

# regions_continents_map
reg_cont <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "regions_continents_map.csv"), skip = 1,
                     stringsAsFactors = FALSE)
use_data(reg_cont, overwrite=T)

# variables_functions_mapping
var_fun_map <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "variables_functions_mapping.csv"),
                       sep=';',header=T, na.strings=c("","NA"), stringsAsFactors = FALSE)

var_fun_map$dependencies <- as.list(strsplit(var_fun_map$dependencies, ","))
var_fun_map$checks <- as.list(strsplit(var_fun_map$checks, ","))
var_fun_map$queries <- as.list(strsplit(var_fun_map$queries, ","))
use_data(var_fun_map, overwrite=T)

# ghg adjuster
GWP_adjuster <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "ghg_GWP.csv"), skip = 1, na = "",
                         stringsAsFactors = FALSE)
use_data(GWP_adjuster, overwrite=T)

# vetting test
global_vet_values <- read.csv(file.path(rawDataFolder, "inst/extdata/vetting", "global_vet_values.csv"),
                              stringsAsFactors = FALSE)
use_data(global_vet_values, overwrite=T)

# Read in template
template <- read.csv(file.path(rawDataFolder, "inst/extdata", "template/reporting_template.csv"),
                     fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE)
use_data(template, overwrite=T)

# emissions maps
co2_sector_map <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "CO2_sector_map.csv"), skip = 1, na = "",
                           stringsAsFactors = FALSE) %>% gather_map()
use_data(co2_sector_map, overwrite=T)

co2_ets_sector_map <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "CO2_ETS_sector_map.csv"), skip = 1, na = "",
                           stringsAsFactors = FALSE) %>% gather_map()
use_data(co2_ets_sector_map, overwrite=T)

co2_tech_map <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "CO2_tech_map.csv"), skip = 1, na = "",
                         stringsAsFactors = FALSE) %>% gather_map()
use_data(co2_tech_map, overwrite=T)

kyoto_sector_map <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "Kyotogas_sector.csv"), skip = 1, na = "",
                             stringsAsFactors = FALSE) %>% gather_map()
use_data(kyoto_sector_map, overwrite=T)

nonco2_emis_sector_map <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "nonCO2_emissions_sector_map.csv"),
                                          skip = 1, na = "", stringsAsFactors = FALSE) %>% gather_map()
use_data(nonco2_emis_sector_map, overwrite=T)

nonco2_emis_resource_map <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "nonCO2_emissions_resource_map.csv"),
                                     skip = 1, na = "", stringsAsFactors = FALSE) %>% gather_map()
use_data(nonco2_emis_resource_map, overwrite=T)

carbon_seq_tech_map <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "carbon_seq_tech_map.csv"), skip = 1, na = "",
                                stringsAsFactors = FALSE) %>% gather_map()
use_data(carbon_seq_tech_map, overwrite=T)


# ag maps
ag_demand_map <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "ag_demand_map.csv"), skip = 1,
                          stringsAsFactors = FALSE) %>% gather_map()
use_data(ag_demand_map, overwrite=T)

ag_prices_map <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "ag_prices_map.csv"), skip = 1,
                          stringsAsFactors = FALSE) %>% gather_map()
use_data(ag_prices_map, overwrite=T)

land_use_map <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "land_use_map.csv"), skip = 1,
                         stringsAsFactors = FALSE) %>% gather_map()
use_data(land_use_map, overwrite=T)


# primary, secondary, final energy maps
primary_energy_map <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "en_primary_map.csv"), skip = 1,
                               stringsAsFactors = FALSE) %>% gather_map()
use_data(primary_energy_map, overwrite=T)

production_map <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings","production_map.csv"), skip = 1,
                           stringsAsFactors = FALSE) %>% gather_map()
use_data(production_map, overwrite=T)

elec_gen_map <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "elec_gen_map_core.csv"), skip = 1,
                         stringsAsFactors = FALSE) %>%
  dplyr::filter(!grepl("cogen", technology)) %>%
  gather_map()
use_data(elec_gen_map, overwrite=T)

capacity_map <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "capacity_map.csv"), skip = 1,
                         stringsAsFactors = FALSE) %>%
  dplyr::filter(!grepl("cogen", technology)) %>%
  gather_map()
use_data(capacity_map, overwrite=T)

cf_gcam <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "A23.globaltech_capacity_factor.csv"), skip = 9, na = "",
                    stringsAsFactors = FALSE)
use_data(cf_gcam, overwrite=T)

cf_rgn <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "L223.StubTechCapFactor_elec.csv"), skip = 1, na = "",
                   stringsAsFactors = FALSE)
use_data(cf_rgn, overwrite=T)

se_gen_map <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "secondary_energy_gen_map.csv"), skip = 1,
                       stringsAsFactors = FALSE) %>% gather_map()
use_data(se_gen_map, overwrite=T)

final_energy_map <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "final_energy_map.csv"), skip = 1,
                             stringsAsFactors = FALSE) %>% gather_map()
use_data(final_energy_map, overwrite=T)

transport_final_en_map <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "transport_final_en_map.csv"), skip = 1, na = "",
                                   stringsAsFactors = FALSE) %>% gather_map()
use_data(transport_final_en_map, overwrite=T)

energy_prices_map <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "energy_prices_map.csv"), skip = 1, na = "",
                              stringsAsFactors = FALSE) %>% gather_map()
use_data(energy_prices_map, overwrite=T)


#Energy Service maps
transport_en_service <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "energy_service_transportation.csv"), skip = 1,
                                 stringsAsFactors = FALSE) %>% gather_map()
use_data(transport_en_service, overwrite=T)

buildings_en_service <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "energy_service_buildings.csv"), skip = 1,
                                 stringsAsFactors = FALSE) %>% gather_map()
use_data(buildings_en_service, overwrite=T)


# capital updates
capital_gcam <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "L223.GlobalIntTechCapital_elec.csv"), skip = 2, na = "",
                         fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE) %>%
  dplyr::rename(technology = intermittent.technology) %>%
  dplyr::bind_rows(read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "L223.GlobalTechCapital_elec.csv"), skip = 2, na = "",
                            stringsAsFactors = FALSE))%>%
  dplyr::select(sector = sector.name, subsector = subsector.name, technology, year, capital.overnight)
use_data(capital_gcam, overwrite=T)

investment <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "investment.csv"), na = "", stringsAsFactors = FALSE) %>%
  tidyr::gather(year, value, X2015:X2100) %>%
  dplyr::mutate(year = as.integer(sub("X", "", year))) %>%
  dplyr::mutate(value = gsub("%", "", value)) %>%
  dplyr::mutate(value = as.numeric(value))
use_data(investment, overwrite=T)


carbon_content <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "L202.CarbonCoef.csv"), skip = 2, na = "",
                           stringsAsFactors = FALSE)
use_data(carbon_content, overwrite=T)

nonCO2_content <-read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "L201.ghg_res.csv"), skip = 2, na = "",
                          stringsAsFactors = FALSE)
use_data(nonCO2_content, overwrite=T)

iea_capacity <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "IEAWEO2019_Capacity.csv"), stringsAsFactors = FALSE)
use_data(iea_capacity, overwrite=T)

CO2_market <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "CO2market_new.csv"), skip = 1, stringsAsFactors = FALSE)
use_data(CO2_market, overwrite=T)

co2_market_frag_map <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "CO2market_frag_map.csv"), skip = 1,
                                stringsAsFactors = FALSE)
use_data(co2_market_frag_map, overwrite=T)

# List of Constants

CUMULATIVE_EMISSIONS_BUDGET_GOALS <- c(300, 400, 500, 600, 700, 800, 900, 950, 1000, 1200, 1400, 1600, 1800, 2000, 2500, 3000)
use_data(CUMULATIVE_EMISSIONS_BUDGET_GOALS, overwrite=T)

# Basic format conv_[from]_[to]
conv_thousand_million <- 1/1000
use_data(conv_thousand_million, overwrite=T)

conv_million_billion <- 1/1000
use_data(conv_million_billion, overwrite=T)

# NOTE: These values are only used for queries that don't have an associated mapping file
# for queries such as primary_fuel_prices this conversion is specified in the mapping file
# These values are taken from GDP inflator in the GCAM R package
conv_90USD_10USD <- 1.515897
use_data(conv_90USD_10USD, overwrite=T)

conv_75USD_10USD <- 3.227608
use_data(conv_75USD_10USD, overwrite=T)

conv_15USD_10USD <- .91863
use_data(conv_15USD_10USD, overwrite=T)

conv_19USD_75USD <- .2658798
use_data(conv_19USD_75USD, overwrite=T)

conv_C_CO2 <- 44/12
use_data(conv_C_CO2, overwrite=T)


# Elec related conversions
hr_per_yr <- 8760
use_data(hr_per_yr, overwrite=T)

EJ_to_GWh <- 0.0000036
use_data(EJ_to_GWh, overwrite=T)

bcm_to_EJ <- 0.03600
use_data(bcm_to_EJ, overwrite=T)

GJ_to_EJ <- 1.0E9
use_data(GJ_to_EJ, overwrite=T)


# GHG emission conversion
F_GASES <- c("C2F6", "CF4", "HFC125", "HFC134a", "HFC245fa", "SF6", "HFC143a", "HFC152a", "HFC227ea", "HFC23", "HFC236fa", "HFC32", "HFC365mfc")
use_data(F_GASES, overwrite=T)

GHG_gases <- c("CH4", "N2O", F_GASES, "CO2", "CO2LUC")
use_data(GHG_gases, overwrite=T)

CO2_equivalent <- 3.666667 #ghg * CO2_equivalent gives CO2 units
use_data(CO2_equivalent, overwrite=T)


# Reporting years
GCAM_years <- c(1990, seq(2005, 2100, 5))
use_data(GCAM_years, overwrite=T)

reporting_years <- seq(2005, 2100, 5)
use_data(reporting_years, overwrite=T)

last_historical_year = 2015
use_data(last_historical_year, overwrite=T)


# REPLACE with reporting years

# Reporting columns
long_columns <- c("scenario", "region", "var", "year", "value")
use_data(long_columns, overwrite=T)

reporting_columns <- c("Model", "Scenario", "Region", "Variable", "Unit", reporting_years)
use_data(reporting_columns, overwrite=T)


# state names
state_names <- c("AL", "AK", "AZ", "AR", "CA", "CO", "CT", "DE", "DC", "FL", "GA", "HI",
                 "ID", "IL", "IN", "IA", "KS", "KY", "LA", "ME", "MD", "MA", "MI",
                 "MN", "MS", "MO", "MT", "NE", "NV", "NH", "NJ", "NM", "NY", "NC",
                 "ND", "OH", "OK", "OR", "PA", "RI", "SC", "SD", "TN", "TX", "UT",
                 "VT", "VA", "WA", "WV", "WI", "WY")
use_data(state_names, overwrite=T)


################################################################################

# color by tech
tech.color <- c( "biomass w/o ccs" = "darkgreen", "biomass w/ ccs" = "olivedrab",
                 "traditional biomass" = "olivedrab3", "biomass traditional" = "olivedrab3",
                 "biomass 1st generation" = "olivedrab1",
                 "solar" = "goldenrod1",
                 "wind" = "skyblue",
                 "geothermal" = "olivedrab1",
                 "hydro" = "mediumpurple4",
                 "coal w/o ccs" = "darkred", "coal w/ ccs" = "firebrick3", "coal.total" = "firebrick4",
                 "nuclear" = "darkorange1",
                 "gas w/o ccs" = "mediumorchid3", "gas w/ ccs" = "plum",
                 "natural gas" = "mediumorchid3", "natural gas_ccs" = "plum",
                 "oil w/o ccs" = "hotpink", "oil w/ ccs" = "lightpink1",
                 # "other" = "firebrick3")
                 "other" = "plum")
use_data(tech.color, overwrite=T)


colScaleTech <- ggplot2::scale_colour_manual(name = "technology",
                                            values = tech.color,
                                            na.translate = FALSE,
                                            guide = ggplot2::guide_legend(reverse = F, ncol = 1))
use_data(colScaleTech, overwrite=T)

fillScaleTech <- ggplot2::scale_fill_manual(name = "technology",
                                            values = tech.color,
                                            na.translate = FALSE,
                                            guide = ggplot2::guide_legend(reverse = F, ncol = 1))
use_data(fillScaleTech, overwrite=T)


# categorize fuel
fuel.list <- c("electricity","biomass", "biomass|modern", "biomass|traditional", "gas", "coal", "liquids", "hydrogen")
use_data(fuel.list, overwrite=T)


# color by fuel
fuel.color <- c( "biomass" = "darkgreen",
                 "biomass|modern" = "darkgreen",
                 "biomass|traditional" = "limegreen",
                 "electricity" = "goldenrod1",
                 "hydrogen" = "mediumpurple4",
                 "coal" = "firebrick4",
                 "gas" = "mediumorchid3",
                 "liquids" = "hotpink",
                 "heat" = "darkorange1")
use_data(fuel.color, overwrite=T)


colScaleFuel <- ggplot2::scale_colour_manual(name = "fuel",
                                    values = fuel.color,
                                    na.translate = FALSE,
                                    guide = ggplot2::guide_legend(reverse = F, ncol = 1))
use_data(colScaleFuel, overwrite=T)

fillScaleFuel <- ggplot2::scale_fill_manual(name = "fuel",
                                   values = fuel.color,
                                   na.translate = FALSE,
                                   guide = ggplot2::guide_legend(reverse = F, ncol = 1))
use_data(fillScaleFuel, overwrite=T)


# categorize sector
sector.list <- c("Electricity", "Residential and commercial", "Other energy supply", "Refining",
                 "Transportation", "Industrial processes", "AFOLU CO2", "NonCO2")
use_data(sector.list, overwrite=T)


# color by fuel
sector.color <- c(
  "Electricity" = "goldenrod1",
  "Refining" = "purple",
  "Industry" = "gray",
  "Other energy supply" = "navy",
  "Residential and commercial" = "deepskyblue2",
  "Transportation" = "darkgreen",
  "AFOLU CO2" = "lime green",
  "NonCO2" = "brown")
use_data(sector.color, overwrite=T)


fillScaleSector <- ggplot2::scale_fill_manual(name = "Sector",
                                     values = sector.color,
                                     na.translate = FALSE,
                                     guide = ggplot2::guide_legend(reverse = F, ncol = 1))
use_data(fillScaleSector, overwrite=T)


tech.list <- c("other", "geothermal", "solar", "wind", "biomass w/ ccs", "biomass w/o ccs",
               "hydro", "nuclear",
               "gas w/ ccs", "gas w/o ccs", "natural gas_ccs", "natural gas",
               "oil w/ ccs", "oil w/o ccs", "coal w/ ccs","coal w/o ccs", "coal.total",
               "biomass 1st generation", "traditional biomass", "biomass traditional")
use_data(tech.list, overwrite=T)

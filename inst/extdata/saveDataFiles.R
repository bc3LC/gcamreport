# Converting raw data into package data
library(usethis)
library(magrittr)
library(here)

### paths
rawDataFolder = here::here()


# variables_functions_mapping
var_fun_map <- read.csv(paste0(here::here(), "/inst/extdata", "/variables_functions_mapping.csv"),
                       sep=';',header=T, na.strings=c("","NA"))

var_fun_map$dependencies <- as.list(strsplit(var_fun_map$dependencies, ","))
var_fun_map$checks <- as.list(strsplit(var_fun_map$checks, ","))
use_data(var_fun_map, overwrite=T)

# Read in template
template <- read.csv(paste0(here::here(), "/inst/extdata", "/template/NGFS_reporting_template.csv"),
                     fileEncoding = "UTF-8-BOM")
use_data(template, overwrite=T)

# emissions maps
co2_sector_map <- read.csv(paste0(here::here(), "/inst/extdata/mappings", "/CO2_sector_map.csv"), skip = 1, na = "") %>% gather_map()
use_data(co2_sector_map, overwrite=T)

co2_tech_map <- read.csv(paste0(here::here(), "/inst/extdata/mappings", "/CO2_tech_map.csv"), skip = 1, na = "") %>% gather_map()
use_data(co2_tech_map, overwrite=T)

kyoto_sector_map <- read.csv(paste0(here::here(), "/inst/extdata/mappings", "/Kyotogas_sector.csv"), skip = 1, na = "") %>% gather_map()
use_data(kyoto_sector_map, overwrite=T)

nonco2_emis_sector_map <- read.csv(paste0(here::here(), "/inst/extdata/mappings", "/nonCO2_emissions_sector_map.csv"), skip = 1, na = "") %>% gather_map()
use_data(nonco2_emis_sector_map, overwrite=T)

nonco2_emis_resource_map <- read.csv(paste0(here::here(), "/inst/extdata/mappings", "/nonCO2_emissions_resource_map.csv"), skip = 1, na = "") %>% gather_map()
use_data(nonco2_emis_resource_map, overwrite=T)

carbon_seq_tech_map <- read.csv(paste0(here::here(), "/inst/extdata/mappings", "/carbon_seq_tech_map.csv"), skip = 1, na = "") %>% gather_map()
use_data(carbon_seq_tech_map, overwrite=T)


# ag maps
ag_demand_map <- read.csv(paste0(here::here(), "/inst/extdata/mappings", "/ag_demand_map.csv"), skip = 1) %>% gather_map()
use_data(ag_demand_map, overwrite=T)

ag_prices_map <- read.csv(paste0(here::here(), "/inst/extdata/mappings", "/ag_prices_map.csv"), skip = 1) %>% gather_map()
use_data(ag_prices_map, overwrite=T)

land_use_map <- read.csv(paste0(here::here(), "/inst/extdata/mappings", "/land_use_map.csv"), skip = 1) %>% gather_map()
use_data(land_use_map, overwrite=T)


# primary, secondary, final energy maps
primary_energy_map <- read.csv(paste0(here::here(), "/inst/extdata/mappings", "/en_primary_map.csv"), skip = 1) %>% gather_map()
use_data(primary_energy_map, overwrite=T)

production_map <- read.csv(paste0(here::here(), "/inst/extdata/mappings","/production_map.csv"), skip = 1) %>% gather_map()
use_data(production_map, overwrite=T)

elec_gen_map <- read.csv(paste0(here::here(), "/inst/extdata/mappings", "/elec_gen_map_core.csv"), skip = 1) %>%
  dplyr::filter(!grepl("cogen", technology)) %>%
  gather_map()
use_data(elec_gen_map, overwrite=T)

capacity_map <- read.csv(paste0(here::here(), "/inst/extdata/mappings", "/capacity_map.csv"), skip = 1) %>%
  dplyr::filter(!grepl("cogen", technology)) %>%
  gather_map()
use_data(capacity_map, overwrite=T)

cf_gcam <- read.csv(paste0(here::here(), "/inst/extdata/mappings", "/A23.globaltech_capacity_factor.csv"), skip = 9, na = "")
use_data(cf_gcam, overwrite=T)

cf_rgn <- read.csv(paste0(here::here(), "/inst/extdata/mappings", "/L223.StubTechCapFactor_elec.csv"), skip = 1, na = "")
use_data(cf_rgn, overwrite=T)

se_gen_map <- read.csv(paste0(here::here(), "/inst/extdata/mappings", "/secondary_energy_gen_map.csv"), skip = 1) %>% gather_map()
use_data(se_gen_map, overwrite=T)

final_energy_map <- read.csv(paste0(here::here(), "/inst/extdata/mappings", "/final_energy_map.csv"), skip = 1) %>% gather_map()
use_data(final_energy_map, overwrite=T)

transport_final_en_map <- read.csv(paste0(here::here(), "/inst/extdata/mappings", "/transport_final_en_map.csv"), skip = 1, na = "") %>% gather_map()
use_data(transport_final_en_map, overwrite=T)

energy_prices_map <- read.csv(paste0(here::here(), "/inst/extdata/mappings", "/energy_prices_map.csv"), skip = 1, na = "") %>% gather_map()
use_data(energy_prices_map, overwrite=T)


#Energy Service maps
transport_en_service <- read.csv(paste0(here::here(), "/inst/extdata/mappings", "/energy_service_transportation.csv"), skip = 1) %>% gather_map()
use_data(transport_en_service, overwrite=T)

buildings_en_service <- read.csv(paste0(here::here(), "/inst/extdata/mappings", "/energy_service_buildings.csv"), skip = 1) %>% gather_map()
use_data(buildings_en_service, overwrite=T)


# capital updates
capital_gcam <- read.csv(paste0(here::here(), "/inst/extdata/mappings", "/L223.GlobalIntTechCapital_elec.csv"), skip = 2, na = "",
                         fileEncoding = "UTF-8-BOM") %>%
  dplyr::rename(technology = intermittent.technology) %>%
  dplyr::bind_rows(read.csv(paste0(here::here(), "/inst/extdata/mappings", "/L223.GlobalTechCapital_elec.csv"), skip = 2, na = ""))%>%
  dplyr::select(sector = sector.name, subsector = subsector.name, technology, year, capital.overnight)
use_data(capital_gcam, overwrite=T)

investment <- read.csv(paste0(here::here(), "/inst/extdata/mappings", "/investment.csv"), na = "") %>%
  tidyr::gather(year, value, X2015:X2100) %>%
  dplyr::mutate(year = as.integer(sub("X", "", year))) %>%
  dplyr::mutate(value = gsub("%", "", value)) %>%
  dplyr::mutate(value = as.numeric(value))
use_data(investment, overwrite=T)


carbon_content <- read.csv(paste0(here::here(), "/inst/extdata/mappings", "/L202.CarbonCoef.csv"), skip = 2, na = "")
use_data(carbon_content, overwrite=T)

nonCO2_content <-read.csv(paste0(here::here(), "/inst/extdata/mappings", "/L201.ghg_res.csv"), skip = 2, na = "")
use_data(nonCO2_content, overwrite=T)

iea_capacity <- read.csv(paste0(here::here(), "/inst/extdata/mappings", "/IEAWEO2019_Capacity.csv"))
use_data(iea_capacity, overwrite=T)

CO2_market <- read.csv(paste0(here::here(), "/inst/extdata/mappings", "/CO2market_new.csv"), skip = 1)
use_data(CO2_market, overwrite=T)

co2_market_frag_map <- read.csv(paste0(here::here(), "/inst/extdata/mappings", "/CO2market_frag_map.csv"), skip = 1)
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


# Reporting years
GCAM_years <- c(1990, seq(2005, 2100, 5))
use_data(GCAM_years, overwrite=T)

reporting_years <- seq(2005, 2100, 5)
use_data(reporting_years, overwrite=T)

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

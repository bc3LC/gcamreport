# Converting raw data into package data
library(usethis)
library(magrittr)

### paths
rawDataFolder <- here::here()


# nonCO2 emissions considered
nonco2_emissions_list_v7.1 <- c(
  "BC", "BC_AWB", "C2F6", "CF4", "CH4", "CH4_AGR", "CH4_AWB", "CO", "CO_AWB",
  "H2", "H2_AWB", "HFC125", "HFC134a", "HFC143a", "HFC152a", "HFC227ea", "HFC23", "HFC236fa",
  "HFC245fa", "HFC32", "HFC365mfc", "HFC43", "N2O", "N2O_AGR", "N2O_AWB", "NH3", "NH3_AGR",
  "NH3_AWB", "NMVOC", "NMVOC_AGR", "NMVOC_AWB", "NOx", "NOx_AGR", "NOx_AWB", "OC", "OC_AWB",
  "PM10", "PM2.5", "SF6", "SO2_1", "SO2_1_AWB", "SO2_2", "SO2_2_AWB", "SO2_3", "SO2_3_AWB",
  "SO2_4", "SO2_4_AWB"
)
use_data(nonco2_emissions_list_v7.1, overwrite = T)




# regions_continents_map
reg_cont_v7.1 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.1", "regions_continents_map.csv"),
                          skip = 1,
                          stringsAsFactors = FALSE
)
use_data(reg_cont_v7.1, overwrite = T)

# variables_functions_mapping
var_fun_map_v7.1 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.1", "variables_functions_mapping.csv"),
                             sep = ";", header = T, na.strings = c("", "NA"), stringsAsFactors = FALSE
)

var_fun_map_v7.1$dependencies <- as.list(strsplit(var_fun_map_v7.1$dependencies, ","))
var_fun_map_v7.1$checks <- as.list(strsplit(var_fun_map_v7.1$checks, ","))
var_fun_map_v7.1$queries <- as.list(strsplit(var_fun_map_v7.1$queries, ","))
use_data(var_fun_map_v7.1, overwrite = T)


# Read in template
template_v7.1 <- read.csv(file.path(rawDataFolder, "inst/extdata/template/GCAM7.1", "reporting_template.csv"),
                          fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE
)
decode_html <- function(text) {
  xml2::xml_text(xml2::read_xml(paste0("<x>", text, "</x>")))
}
# Applying the function to decode HTML entities in col1
template_v7.1$Unit <- sapply(template_v7.1$Unit, decode_html)
use_data(template_v7.1, overwrite = T)

# emissions maps
co2_sector_map_v7.1 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.1", "CO2_sector_map.csv"),
                                skip = 1, na = "",
                                stringsAsFactors = FALSE
) %>% gather_map()
use_data(co2_sector_map_v7.1, overwrite = T)

co2_ets_sector_map_v7.1 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.1", "CO2_ETS_sector_map.csv"),
                                    skip = 1, na = "",
                                    stringsAsFactors = FALSE
) %>% gather_map()
use_data(co2_ets_sector_map_v7.1, overwrite = T)

co2_tech_map_v7.1 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.1", "CO2_tech_map.csv"),
                              skip = 1, na = "",
                              stringsAsFactors = FALSE
) %>% gather_map()
use_data(co2_tech_map_v7.1, overwrite = T)

kyoto_sector_map_v7.1 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.1", "Kyotogas_sector.csv"),
                                  skip = 1, na = "",
                                  stringsAsFactors = FALSE
) %>% gather_map()
use_data(kyoto_sector_map_v7.1, overwrite = T)

nonco2_emis_sector_map_v7.1 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.1", "nonCO2_emissions_sector_map.csv"),
                                        skip = 1, na = "", stringsAsFactors = FALSE
) %>% gather_map()
use_data(nonco2_emis_sector_map_v7.1, overwrite = T)

nonco2_emis_resource_map_v7.1 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.1", "nonCO2_emissions_resource_map.csv"),
                                          skip = 1, na = "", stringsAsFactors = FALSE
) %>% gather_map()
use_data(nonco2_emis_resource_map_v7.1, overwrite = T)

carbon_seq_tech_map_v7.1 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.1", "carbon_seq_tech_map.csv"),
                                     skip = 1, na = "",
                                     stringsAsFactors = FALSE
) %>% gather_map()
use_data(carbon_seq_tech_map_v7.1, overwrite = T)


# ag maps
ag_demand_map_v7.1 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.1", "ag_demand_map.csv"),
                               skip = 1,
                               stringsAsFactors = FALSE
) %>% gather_map()
use_data(ag_demand_map_v7.1, overwrite = T)

ag_prices_map_v7.1 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.1", "ag_prices_map.csv"),
                               skip = 1,
                               stringsAsFactors = FALSE
) %>% gather_map()
use_data(ag_prices_map_v7.1, overwrite = T)

land_use_map_v7.1 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.1", "land_use_map.csv"),
                              skip = 1,
                              stringsAsFactors = FALSE
) %>% gather_map()
use_data(land_use_map_v7.1, overwrite = T)


# primary, secondary, final energy maps
primary_energy_map_v7.1 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.1", "en_primary_map.csv"),
                                    skip = 1,
                                    stringsAsFactors = FALSE
) %>% gather_map()
use_data(primary_energy_map_v7.1, overwrite = T)

production_map_v7.1 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.1", "production_map.csv"),
                                skip = 1,
                                stringsAsFactors = FALSE
) %>% gather_map()
use_data(production_map_v7.1, overwrite = T)

elec_gen_map_v7.1 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.1", "elec_gen_map_core.csv"),
                              skip = 1,
                              stringsAsFactors = FALSE
) %>%
  dplyr::filter(!grepl("cogen", technology)) %>%
  gather_map()
use_data(elec_gen_map_v7.1, overwrite = T)

capacity_map_v7.1 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.1", "capacity_map.csv"),
                              skip = 1,
                              stringsAsFactors = FALSE
) %>%
  dplyr::filter(!grepl("cogen", technology)) %>%
  gather_map()
use_data(capacity_map_v7.1, overwrite = T)

cf_gcam_v7.1 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.1", "A23.globaltech_capacity_factor.csv"),
                         skip = 9, na = "",
                         stringsAsFactors = FALSE
)
use_data(cf_gcam_v7.1, overwrite = T)

cf_rgn_v7.1 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.1", "L223.StubTechCapFactor_elec.csv"),
                        skip = 1, na = "",
                        stringsAsFactors = FALSE
)
use_data(cf_rgn_v7.1, overwrite = T)

se_gen_map_v7.1 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.1", "secondary_energy_gen_map.csv"),
                            skip = 1,
                            stringsAsFactors = FALSE
) %>% gather_map()
use_data(se_gen_map_v7.1, overwrite = T)

final_energy_map_v7.1 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.1", "final_energy_map.csv"),
                                  skip = 1,
                                  stringsAsFactors = FALSE
) %>% gather_map()
use_data(final_energy_map_v7.1, overwrite = T)

transport_final_en_map_v7.1 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.1", "transport_final_en_map.csv"),
                                        skip = 1, na = "",
                                        stringsAsFactors = FALSE
) %>% gather_map()
use_data(transport_final_en_map_v7.1, overwrite = T)

energy_prices_map_v7.1 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.1", "energy_prices_map.csv"),
                                   skip = 1, na = "",
                                   stringsAsFactors = FALSE
) %>% gather_map()
use_data(energy_prices_map_v7.1, overwrite = T)


# Energy Service maps
transport_en_service_v7.1 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.1", "transport_en_service.csv"),
                                      skip = 1,
                                      stringsAsFactors = FALSE
) %>% gather_map()
use_data(transport_en_service_v7.1, overwrite = T)

buildings_en_service_v7.1 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.1", "buildings_en_service.csv"),
                                      skip = 1,
                                      stringsAsFactors = FALSE
) %>% gather_map()
use_data(buildings_en_service_v7.1, overwrite = T)


# capital updates
capital_gcam_v7.1 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.1", "L223.GlobalIntTechCapital_elec.csv"),
                              skip = 2, na = "",
                              fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE
) %>%
  dplyr::rename(technology = intermittent.technology) %>%
  dplyr::bind_rows(read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.1", "L223.GlobalTechCapital_elec.csv"),
                            skip = 2, na = "",
                            stringsAsFactors = FALSE
  )) %>%
  dplyr::select(sector = sector.name, subsector = subsector.name, technology, year, capital.overnight)
use_data(capital_gcam_v7.1, overwrite = T)

investment_v7.1 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.1", "investment.csv"),
                            na = "",
                            fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE
) %>%
  tidyr::gather(year, value, X2015:X2100) %>%
  dplyr::mutate(year = as.integer(sub("X", "", year))) %>%
  dplyr::mutate(value = gsub("%", "", value)) %>%
  dplyr::mutate(value = as.numeric(value))
use_data(investment_v7.1, overwrite = T)


carbon_content_v7.1 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.1", "L202.CarbonCoef.csv"),
                                skip = 2, na = "",
                                stringsAsFactors = FALSE
)
use_data(carbon_content_v7.1, overwrite = T)

nonco2_content_v7.1 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.1", "L201.ghg_res.csv"),
                                skip = 2, na = "",
                                stringsAsFactors = FALSE
)
use_data(nonco2_content_v7.1, overwrite = T)

iea_capacity_v7.1 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.1", "IEAWEO2019_Capacity.csv"), stringsAsFactors = FALSE)
use_data(iea_capacity_v7.1, overwrite = T)

co2_market_v7.1 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.1", "CO2market_new.csv"), skip = 1, stringsAsFactors = FALSE)
use_data(co2_market_v7.1, overwrite = T)

co2_market_frag_map_v7.1 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.1", "CO2market_frag_map.csv"),
                                     skip = 1,
                                     stringsAsFactors = FALSE
)
use_data(co2_market_frag_map_v7.1, overwrite = T)

# iron and steel
iron_steel_trade_map_v7.1 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.1", "iron_steel_trade.csv"), skip = 1,
                                      stringsAsFactors = FALSE) %>% gather_map()
use_data(iron_steel_trade_map_v7.1, overwrite = T)


# Reporting years
last_historical_year_v7.1 <- 2015
use_data(last_historical_year_v7.1, overwrite = T)




# CONSTANTS
# List of Constants
convert_v7.1 <- list(
  # Basic format conv_[from]_[to]
  conv_thousand_million = 1 / 1000,
  conv_million_billion = 1 / 1000,
  # NOTE: These values are only used for queries that don't have an associated mapping file
  # for queries such as primary_fuel_prices this conversion is specified in the mapping file
  # These values are taken from GDP inflator in the GCAM R package
  conv_90USD_10USD = 1.515897,
  conv_75USD_10USD = 3.227608,
  conv_15USD_10USD = 0.91863,
  conv_19USD_75USD = 0.2658798,
  conv_C_CO2 = 44 / 12,
  # Elec related conversions
  hr_per_yr = 8760,
  EJ_to_GWh = 0.0000036,
  bcm_to_EJ = 0.03600,
  GJ_to_EJ = 1.0E9,
  # ghg * CO2_equivalent gives CO2 units
  CO2_equivalent = 3.666667
)
use_data(convert_v7.1, overwrite = T)

# GHG emission conversion
F_GASES_v7.1 <- c(
  "C2F6", "CF4", "HFC125", "HFC134a", "HFC245fa", "SF6", "HFC143a",
  "HFC152a", "HFC227ea", "HFC23", "HFC236fa", "HFC32", "HFC365mfc",
  "HFC43", "HFC245fa", "HFC43-10"
)
use_data(F_GASES_v7.1, overwrite = T)

GHG_gases_v7.1 <- c("CH4", "N2O", F_GASES_v7.1, "CO2", "CO2LUC")
use_data(GHG_gases_v7.1, overwrite = T)



# QUERY files

# gcamreport7 queries complete
queryFile <- file.path(rawDataFolder, "inst/extdata/queries/GCAM7.1", "queries_gcamreport_general.xml")
queries_general_v7.1 <- rgcam::parse_batch_query(queryFile)
use_data(queries_general_v7.1, overwrite = T)

# gcamreport7 queries nonCO2
queryFile <- file.path(rawDataFolder, "inst/extdata/queries/GCAM7.1", "queries_gcamreport_nonCO2.xml")
queries_nonCO2_v7.1 <- rgcam::parse_batch_query(queryFile)
use_data(queries_nonCO2_v7.1, overwrite = T)



# TEMPLATE & VARIABLES

# Read in template
template_v7.1 <- read.csv(file.path(rawDataFolder, "inst/extdata", "template/GCAM7.1/reporting_template.csv"),
                          fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE
)
decode_html <- function(text) {
  xml2::xml_text(xml2::read_xml(paste0("<x>", text, "</x>")))
}
# Applying the function to decode HTML entities in col1
template_v7.1$Unit <- sapply(template_v7.1$Unit, decode_html)
use_data(template_v7.1, overwrite = T)


# variables_functions_mapping
var_fun_map_v7.1 <- read.csv(file.path(rawDataFolder, "inst/extdata", "mappings/GCAM7.1/variables_functions_mapping.csv"),
                             sep = ";", header = T, na.strings = c("", "NA"), stringsAsFactors = FALSE
)

var_fun_map_v7.1$dependencies <- as.list(strsplit(var_fun_map_v7.1$dependencies, ","))
var_fun_map_v7.1$checks <- as.list(strsplit(var_fun_map_v7.1$checks, ","))
var_fun_map_v7.1$queries <- as.list(strsplit(var_fun_map_v7.1$queries, ","))
use_data(var_fun_map_v7.1, overwrite = T)


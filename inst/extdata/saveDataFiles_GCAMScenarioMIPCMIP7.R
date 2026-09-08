# Converting raw data into package data
library(usethis)
library(magrittr)

### paths
rawDataFolder <- here::here()


# nonCO2 emissions considered
nonco2_emissions_list_vScenarioMIPCMIP7 <- c(
  "BC", "BC_AWB", "C2F6", "CF4", "CH4", "CH4_AGR", "CH4_AWB", "CO", "CO_AWB",
  "H2", "H2_AWB", "HFC125", "HFC134a", "HFC143a", "HFC152a", "HFC227ea", "HFC23", "HFC236fa",
  "HFC245fa", "HFC32", "HFC365mfc", "HFC43", "N2O", "N2O_AGR", "N2O_AWB", "NH3", "NH3_AGR",
  "NH3_AWB", "NMVOC", "NMVOC_AGR", "NMVOC_AWB", "NOx", "NOx_AGR", "NOx_AWB", "OC", "OC_AWB",
  "PM10", "PM2.5", "SF6", "SO2_1", "SO2_1_AWB", "SO2_2", "SO2_2_AWB", "SO2_3", "SO2_3_AWB",
  "SO2_4", "SO2_4_AWB"
)
use_data(nonco2_emissions_list_vScenarioMIPCMIP7, overwrite = T)




# regions_continents_map
reg_cont_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "regions_continents_map.csv"),
                                       comment = "#"
)
use_data(reg_cont_vScenarioMIPCMIP7, overwrite = T)

# emissions maps
co2_ets_sector_map_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "CO2_ETS_sector_map.csv"),
                                    comment = "#", na = ""
) %>% gather_map()
use_data(co2_ets_sector_map_vScenarioMIPCMIP7, overwrite = T)

co2_tech_map_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "CO2_tech_map.csv"),
                              comment = "#", na = ""
) %>% gather_map()
use_data(co2_tech_map_vScenarioMIPCMIP7, overwrite = T)

co2_resource_map_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "CO2_resource_map.csv"),
                                  comment = "#", na = ""
) %>% gather_map()
use_data(co2_resource_map_vScenarioMIPCMIP7, overwrite = T)

kyoto_sector_map_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "kyotogas_sector.csv"),
                                  comment = "#", na = ""
) %>% gather_map()
use_data(kyoto_sector_map_vScenarioMIPCMIP7, overwrite = T)

nonco2_emis_sector_map_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "nonCO2_emissions_sector_map.csv"),
                                        comment = "#", na = ""
) %>% gather_map()
use_data(nonco2_emis_sector_map_vScenarioMIPCMIP7, overwrite = T)

nonco2_emis_resource_map_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "nonCO2_emissions_resource_map.csv"),
                                          comment = "#", na = ""
) %>% gather_map()
use_data(nonco2_emis_resource_map_vScenarioMIPCMIP7, overwrite = T)

carbon_seq_tech_map_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "carbon_seq_tech_map.csv"),
                                     comment = "#", na = ""
) %>% gather_map()
use_data(carbon_seq_tech_map_vScenarioMIPCMIP7, overwrite = T)


# ag maps
fertilizer_map_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "fertilizer_map.csv"),
                               comment = "#"
) %>% gather_map()
use_data(fertilizer_map_vScenarioMIPCMIP7, overwrite = T)

ag_demand_map_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "ag_demand_map.csv"),
                               comment = "#"
) %>% gather_map()
use_data(ag_demand_map_vScenarioMIPCMIP7, overwrite = T)

ag_price_map_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "ag_price_map.csv"),
                              comment = "#"
) %>% gather_map()
use_data(ag_price_map_vScenarioMIPCMIP7, overwrite = T)

ag_production_map_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "ag_production_map.csv"),
                                   comment = "#"
) %>% gather_map()
use_data(ag_production_map_vScenarioMIPCMIP7, overwrite = T)

ag_demand_price_map_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "ag_demand_price_map.csv"),
                                     comment = "#"
)
use_data(ag_demand_price_map_vScenarioMIPCMIP7, overwrite = T)

trade_ag_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "trade_ag.csv"), comment = "#") %>% gather_map()
use_data(trade_ag_vScenarioMIPCMIP7, overwrite = T)

land_use_map_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "land_use_map.csv"),
                              comment = "#"
) %>% gather_map()
use_data(land_use_map_vScenarioMIPCMIP7, overwrite = T)

yield_map_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "yield_map.csv"),
                              comment = "#"
)
use_data(yield_map_vScenarioMIPCMIP7, overwrite = T)

food_intake_map_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "food_intake_map.csv"),
                                 comment = "#"
) %>% gather_map()
use_data(food_intake_map_vScenarioMIPCMIP7, overwrite = T)

food_items_map_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "food_items_map.csv"),
                                comment = "#"
)
use_data(food_items_map_vScenarioMIPCMIP7, overwrite = T)

food_expenditures_average_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "food_expenditures_average.csv"),
                                                   comment = "#"
)
use_data(food_expenditures_average_vScenarioMIPCMIP7, overwrite = T)

# waste share (waste / supply), exogenously driven per SSP scenario.
L100.AgMIP_FoodWaste_Share_Pathway_SSP_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "L100.AgMIP_FoodWaste_Share_Pathway_SSP.csv"),
                                comment = "#"
) %>%
  dplyr::rename(ssp = scenario) %>%
  dplyr::select(-GCAM_region_ID)
use_data(L100.AgMIP_FoodWaste_Share_Pathway_SSP_vScenarioMIPCMIP7, overwrite = T)

# wood fuel - industrial roundwood
WoodFuel_IndRoundwood_ratio_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "FAO_GCAM_Ratio_WoodFuel_IndRoundwood_R_Yh.csv"),
                                                    comment = "#"
)
use_data(WoodFuel_IndRoundwood_ratio_vScenarioMIPCMIP7, overwrite = T)

# cereal yield and land scalar
cereal_scaler_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "GCAM_Yield_R_Cereals_Scaler_Y.csv"),
                                                    comment = "#"
)
use_data(cereal_scaler_vScenarioMIPCMIP7, overwrite = T)

# food pcal, mt, kg conversion
food_cal_mt_kg_conversion_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "food_cal_mt_kg_conversion.csv"),
                                                    comment = "#"
)
use_data(food_cal_mt_kg_conversion_vScenarioMIPCMIP7, overwrite = T)



# primary, secondary, final energy maps
primary_energy_map_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "primary_energy_map.csv"),
                                    comment = "#"
) %>% gather_map()
use_data(primary_energy_map_vScenarioMIPCMIP7, overwrite = T)

production_map_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "production_map.csv"),
                                comment = "#"
) %>% gather_map()
use_data(production_map_vScenarioMIPCMIP7, overwrite = T)

secondary_energy_map_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "capacity_map.csv"),
                                      comment = "#"
) %>%
  dplyr::filter(!grepl("cogen", technology)) %>%
  gather_map()
use_data(secondary_energy_map_vScenarioMIPCMIP7, overwrite = T)

capacity_map_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "capacity_map.csv"),
                              comment = "#"
) %>%
  dplyr::filter(!grepl("cogen", technology)) %>%
  gather_map()
use_data(capacity_map_vScenarioMIPCMIP7, overwrite = T)

cf_gcam_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "A23.globaltech_capacity_factor.csv"),
                         comment = "#", na = ""
)
use_data(cf_gcam_vScenarioMIPCMIP7, overwrite = T)

cf_rgn_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "L223.StubTechCapFactor_elec.csv"),
                        comment = "#", na = ""
)
use_data(cf_rgn_vScenarioMIPCMIP7, overwrite = T)

final_energy_map_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "final_energy_map.csv"),
                                  comment = "#"
) %>% gather_map()
use_data(final_energy_map_vScenarioMIPCMIP7, overwrite = T)

en_demand_price_map_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "en_demand_price_map.csv"),
                                     comment = "#"
)
use_data(en_demand_price_map_vScenarioMIPCMIP7, overwrite = T)

res_extraction_map_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "res_extraction_map.csv"),
                                    comment = "#"
) %>% gather_map()
use_data(res_extraction_map_vScenarioMIPCMIP7, overwrite = T)

transport_final_en_map_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "transport_final_en_map.csv"),
                                        comment = "#", na = ""
) %>% gather_map()
use_data(transport_final_en_map_vScenarioMIPCMIP7, overwrite = T)

energy_price_map_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "en_price_map.csv"),
                                  comment = "#", na = ""
) %>% gather_map()
use_data(energy_price_map_vScenarioMIPCMIP7, overwrite = T)

en_demand_price_map_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "en_demand_price_map.csv"),
                                     comment = "#", na = ""
)
use_data(en_demand_price_map_vScenarioMIPCMIP7, overwrite = T)


# Energy Service maps
en_multiplier_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "en_multiplier.csv"),
                                                          comment = "#"
)
use_data(en_multiplier_vScenarioMIPCMIP7, overwrite = T)

transport_en_service_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "transport_en_service.csv"),
                                      comment = "#"
) %>% gather_map()
use_data(transport_en_service_vScenarioMIPCMIP7, overwrite = T)

buildings_en_service_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "buildings_en_service.csv"),
                                      comment = "#"
) %>% gather_map()
use_data(buildings_en_service_vScenarioMIPCMIP7, overwrite = T)

hdd_cdd_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "hdd_cdd.csv"),
                                      comment = "#"
) %>% gather_map()
use_data(hdd_cdd_vScenarioMIPCMIP7, overwrite = T)


# capital updates
capital_gcam_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "L223.GlobalIntTechCapital_elec.csv"),
                              comment = "#", na = ""
) %>%
  dplyr::rename(technology = intermittent.technology) %>%
  dplyr::bind_rows(readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "L223.GlobalTechCapital_elec.csv"),
                            comment = "#", na = ""
  )) %>%
  dplyr::select(sector = sector.name, subsector = subsector.name, technology, year, capital.overnight)
use_data(capital_gcam_vScenarioMIPCMIP7, overwrite = T)

investment_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "investment.csv"),
                            na = ""
) %>%
  tidyr::gather(year, value, `2015`:`2100`) %>%
  dplyr::mutate(year = as.integer(sub("X", "", year))) %>%
  dplyr::mutate(value = gsub("%", "", value)) %>%
  dplyr::mutate(value = as.numeric(value))
use_data(investment_vScenarioMIPCMIP7, overwrite = T)

nonelec_investment_map_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "nonelec_investment_map.csv"),
                                               comment = "#", na = ""
) %>% gather_map()
use_data(nonelec_investment_map_vScenarioMIPCMIP7, overwrite = T)

carbon_content_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "L202.CarbonCoef.csv"),
                                comment = "#", na = ""
)
use_data(carbon_content_vScenarioMIPCMIP7, overwrite = T)

nonco2_content_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "L201.ghg_res.csv"),
                                comment = "#", na = ""
)
use_data(nonco2_content_vScenarioMIPCMIP7, overwrite = T)

iea_capacity_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "IEAWEO2019_Capacity.csv"))
use_data(iea_capacity_vScenarioMIPCMIP7, overwrite = T)

co2_market_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "CO2market_new.csv"), comment = "#")
use_data(co2_market_vScenarioMIPCMIP7, overwrite = T)

co2_market_frag_map_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "CO2market_frag_map.csv"),
                                     comment = "#"
)
use_data(co2_market_frag_map_vScenarioMIPCMIP7, overwrite = T)

# iron and steel
iron_steel_trade_map_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "iron_steel_trade.csv"), comment = "#") %>% gather_map()
use_data(iron_steel_trade_map_vScenarioMIPCMIP7, overwrite = T)

# water
water_map_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "water.csv"), comment = "#") %>% gather_map()
use_data(water_map_vScenarioMIPCMIP7, overwrite = T)

conveyance.eff_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7", "conveyance.eff.csv"), comment = "#")
use_data(conveyance.eff_vScenarioMIPCMIP7, overwrite = T)

# transport sales & stock
ucd_size_class_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7",
                                          "UCD_size_class_revisions.csv"), comment = "#")
use_data(ucd_size_class_vScenarioMIPCMIP7, overwrite = T)

ucd_core_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7",
                                    "UCD_trn_data_CORE.csv"), comment = "#") %>%
  tidyr::gather(year, value, `2005`:`2100`) %>%
  dplyr::mutate(year = as.integer(sub("X", "", year)))
use_data(ucd_core_vScenarioMIPCMIP7, overwrite = T)

region_mapping_ucd_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7",
                                              "region_mapping_ucd.csv"), comment = "#")
use_data(region_mapping_ucd_vScenarioMIPCMIP7, overwrite = T)

transport_stock_map_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7",
                                               "trn_stock_map.csv"), comment = "#") %>% gather_map()
use_data(transport_stock_map_vScenarioMIPCMIP7, overwrite = T)
transport_sales_map_vScenarioMIPCMIP7 <- readr::read_csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAMScenarioMIPCMIP7",
                                               "trn_sales_map.csv"), comment = "#") %>% gather_map()
use_data(transport_sales_map_vScenarioMIPCMIP7, overwrite = T)


# Reporting years
last_historical_year_vScenarioMIPCMIP7 <- 2015
use_data(last_historical_year_vScenarioMIPCMIP7, overwrite = T)




# CONSTANTS
# List of Constants
convert_vScenarioMIPCMIP7 <- list(
  # Basic format conv_[from]_[to]
  conv_thousand_million = 1 / 1000,
  conv_million_billion = 1 / 1000,
  # NOTE: These values are only used for queries that don't have an associated mapping file
  # for queries such as primary_fuel_prices this conversion is specified in the mapping file
  # These values are taken from GDP inflator in the GCAM R package
  conv_05USD_10USD = 1.100372,
  conv_90USD_10USD = 1.515897,
  conv_10USD_25USD = 1.492, # source: internet
  conv_75USD_10USD = 3.227608,
  conv_15USD_10USD = 0.91863,
  conv_19USD_75USD = 0.2658798,
  conv_17USD_90USD = 0.5880752,
  conv_C_CO2 = 44 / 12,
  # Elec related conversions
  hr_per_yr = 8760,
  EJ_to_GWh = 0.0000036,
  bcm_to_EJ = 0.03600,
  GJ_to_EJ = 1.0E9,
  # Energy content of biomass, GJ/ton
  aglu.BIO_ENERGY_CONTENT_GJT = 17.5,
  # 1Mt (million metric ton) = 1e6kg
  kg_to_Mt = 1e6,
  # land units
  km2_to_ha = 100,
  # ghg * CO2_equivalent gives CO2 units
  CO2_equivalent = 3.666667
)
use_data(convert_vScenarioMIPCMIP7, overwrite = T)

# GHG emission conversion
F_GASES_vScenarioMIPCMIP7 <- c(
  "C2F6", "CF4", "HFC125", "HFC134a", "HFC245fa", "SF6", "HFC143a",
  "HFC152a", "HFC227ea", "HFC23", "HFC236fa", "HFC32", "HFC365mfc",
  "HFC43", "HFC245fa", "HFC43-10"
)
use_data(F_GASES_vScenarioMIPCMIP7, overwrite = T)

GHG_gases_vScenarioMIPCMIP7 <- c("CH4", "N2O", F_GASES_vScenarioMIPCMIP7, "CO2", "CO2LUC")
use_data(GHG_gases_vScenarioMIPCMIP7, overwrite = T)



# QUERY files

# gcamreport7 queries complete
queryFile <- file.path(rawDataFolder, "inst/extdata/queries/GCAMScenarioMIPCMIP7", "queries_gcamreport_general.xml")
queries_general_vScenarioMIPCMIP7 <- rgcam::parse_batch_query(queryFile)
use_data(queries_general_vScenarioMIPCMIP7, overwrite = T)

# gcamreport7 queries nonCO2
queryFile <- file.path(rawDataFolder, "inst/extdata/queries/GCAMScenarioMIPCMIP7", "queries_gcamreport_nonCO2.xml")
queries_nonCO2_vScenarioMIPCMIP7 <- rgcam::parse_batch_query(queryFile)
use_data(queries_nonCO2_vScenarioMIPCMIP7, overwrite = T)



# TEMPLATE & VARIABLES

# Read in template
template_vScenarioMIPCMIP7 <- read.csv2(file.path(rawDataFolder, "inst/extdata", "template/GCAMScenarioMIPCMIP7/common-definitions-template.csv"),
                                        comment = "#", sep = ',', fileEncoding = "UTF-8"
) %>%
  dplyr::select(Variable = variable, Unit = unit, Tier = tier, Internal_variable) %>%
  dplyr::mutate(Model = "GCAM ScenarioMIPCMIP7") %>%
  as.data.frame()
decode_html <- function(text) {
  xml2::xml_text(xml2::read_xml(paste0("<x>", text, "</x>")))
}
# Applying the function to decode HTML entities in col1
template_vScenarioMIPCMIP7$Unit <- sapply(template_vScenarioMIPCMIP7$Unit, decode_html)
use_data(template_vScenarioMIPCMIP7, overwrite = T)


# variables_functions_mapping
var_fun_map_vScenarioMIPCMIP7 <- read.csv(file.path(rawDataFolder, "inst/extdata", "mappings/GCAMScenarioMIPCMIP7/variables_functions_mapping.csv"),
                             sep = ";", header = T, na.strings = c("", "NA")
)

var_fun_map_vScenarioMIPCMIP7$dependencies <- as.list(strsplit(var_fun_map_vScenarioMIPCMIP7$dependencies, ","))
var_fun_map_vScenarioMIPCMIP7$queries <- as.list(strsplit(var_fun_map_vScenarioMIPCMIP7$queries, ","))
use_data(var_fun_map_vScenarioMIPCMIP7, overwrite = T)


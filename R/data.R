#' GCAM_regions_number
#'
#' @source local
#' @format .csv
#' @description number of GCAM regions
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::GCAM_regions_number
#' }
"GCAM_regions_number"

#' available_GCAM_versions
#'
#' @source local
#' @format .csv
#' @description available GCAM_versions
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::available_GCAM_versions
#' }
"available_GCAM_versions"

#' deciles_GCAM_versions
#'
#' @source local
#' @format .csv
#' @description GCAM_versions containing deciles
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::deciles_GCAM_versions
#' }
"deciles_GCAM_versions"

#' available_GWP_versions
#'
#' @source local
#' @format .csv
#' @description available GWP_versions
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::available_GWP_versions
#' }
"available_GWP_versions"

#' ghg_GWP_AR4
#'
#' @source local
#' @format .csv
#' @description Global Warming Potential vales - uses 100-yr GWPs from AR4 in GCAM unit
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::ghg_GWP_AR4
#' }
"ghg_GWP_AR4"

#' ghg_GWP_AR5
#'
#' @source local
#' @format .csv
#' @description Global Warming Potential vales - uses 100-yr GWPs from AR5 in GCAM unit
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::ghg_GWP_AR5
#' }
"ghg_GWP_AR5"

#' ghg_GWP_AR6
#'
#' @source local
#' @format .csv
#' @description Global Warming Potential vales - uses 100-yr GWPs from AR6 in GCAM unit
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::ghg_GWP_AR6
#' }
"ghg_GWP_AR6"

#' global_vet_values
#'
#' @source local
#' @format .csv
#' @description vetting values to test
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::global_vet_values
#' }
"global_vet_values"

#' long_columns
#'
#' @description reporting columns.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::long_columns
#' }
"long_columns"

#' en_blocks
#'
#' @description energy variables hierarchy
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::en_blocks
#' }
"en_blocks"


#' queries_general_v8.2
#'
#' @source local
#' @format vector
#' @description gcamreport queries compatible with GCAM 8.2 version.
#' Contain all queries except for nonCO2 queries
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::queries_general_v8.2
#' }
"queries_general_v8.2"

#' queries_nonCO2_v8.2
#'
#' @source local
#' @format vector
#' @description gcamreport nonCO2 query compatible with GCAM 8.2 version compatible with GCAM8.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::queries_nonCO2_v8.2
#' }
"queries_nonCO2_v8.2"


#' nonco2_emissions_list_v8.2
#'
#' @source local
#' @format vector
#' @description nonCO2 pollutants list
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::nonco2_emissions_list_v8.2
#' }
"nonco2_emissions_list_v8.2"

#' var_fun_map_v8.2
#'
#' @source local
#' @format .csv
#' @description mapping between variables, functions to load them, dependent variables, and available verifications compatible with GCAM8.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::var_fun_map_v8.2
#' }
"var_fun_map_v8.2"

#' template_v8.2
#'
#' @source github
#' @format .csv
#' @description read in template compatible with GCAM8.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::template_v8.2
#' }
"template_v8.2"

#' co2_ets_sector_map_v8.2
#'
#' @source github
#' @format .csv
#' @description emissions maps compatible with GCAM8.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::co2_ets_sector_map_v8.2
#' }
"co2_ets_sector_map_v8.2"

#' co2_tech_map_v8.2
#'
#' @source github
#' @format .csv
#' @description emissions maps compatible with GCAM8.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::co2_tech_map_v8.2
#' }
"co2_tech_map_v8.2"

#' co2_resource_map_v8.2
#'
#' @source github
#' @format .csv
#' @description emissions map by resource production compatible with GCAM8.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::co2_resource_map_v8.2
#' }
"co2_resource_map_v8.2"

#' kyoto_sector_map_v8.2
#'
#' @source github
#' @format .csv
#' @description emissions maps compatible with GCAM8.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::kyoto_sector_map_v8.2
#' }
"kyoto_sector_map_v8.2"

#' nonco2_emis_sector_map_v8.2
#'
#' @source github
#' @format .csv
#' @description emissions maps compatible with GCAM8.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::nonco2_emis_sector_map_v8.2
#' }
"nonco2_emis_sector_map_v8.2"

#' nonco2_emis_resource_map_v8.2
#'
#' @source github
#' @format .csv
#' @description emissions maps compatible with GCAM8.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::nonco2_emis_resource_map_v8.2
#' }
"nonco2_emis_resource_map_v8.2"

#' carbon_seq_tech_map_v8.2
#'
#' @source github
#' @format .csv
#' @description emissions maps compatible with GCAM8.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::carbon_seq_tech_map_v8.2
#' }
"carbon_seq_tech_map_v8.2"

#' ag_demand_map_v8.2
#'
#' @source github
#' @format .csv
#' @description ag maps compatible with GCAM8.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::ag_demand_map_v8.2
#' }
"ag_demand_map_v8.2"

#' fertilizer_map_v8.2
#'
#' @source github
#' @format .csv
#' @description ag maps compatible with GCAM8.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::fertilizer_map_v8.2
#' }
"fertilizer_map_v8.2"

#' ag_price_map_v8.2
#'
#' @source github
#' @format .csv
#' @description ag maps compatible with GCAM8.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::ag_price_map_v8.2
#' }
"ag_price_map_v8.2"

#' ag_demand_price_map_v8.2
#'
#' @source github
#' @format .csv
#' @description ag maps compatible with GCAM8.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::ag_demand_price_map_v8.2
#' }
"ag_demand_price_map_v8.2"

#' ag_production_map_v8.2
#'
#' @source github
#' @format .csv
#' @description ag maps compatible with GCAM8.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::ag_production_map_v8.2
#' }
"ag_production_map_v8.2"

#' trade_ag_v8.2
#'
#' @source github
#' @format .csv
#' @description ag maps compatible with GCAM8.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::trade_ag_v8.2
#' }
"trade_ag_v8.2"

#' land_use_map_v8.2
#'
#' @source github
#' @format .csv
#' @description ag maps compatible with GCAM8.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::land_use_map_v8.2
#' }
"land_use_map_v8.2"

#' yield_map_v8.2
#'
#' @source github
#' @format .csv
#' @description ag maps compatible with yield_map_v8.2
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::yield_map_v8.2
#' }
"yield_map_v8.2"

#' food_intake_map_v8.2
#'
#' @source github
#' @format .csv
#' @description food maps compatible with GCAM8.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::food_intake_map_v8.2
#' }
"food_intake_map_v8.2"

#' food_items_map_v8.2
#'
#' @source github
#' @format .csv
#' @description food maps compatible with GCAM8.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::food_items_map_v8.2
#' }
"food_items_map_v8.2"

#' food_expenditures_average_v8.2
#'
#' @source github
#' @format .csv
#' @description food maps compatible with GCAM8.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::food_expenditures_average_v8.2
#' }
"food_expenditures_average_v8.2"

#' L100.AgMIP_FoodWaste_Share_Pathway_SSP_v8.2
#'
#' @source github
#' @format .csv
#' @description food waste share (waste/supppy) compatible with GCAM8.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::L100.AgMIP_FoodWaste_Share_Pathway_SSP_v8.2
#' }
"L100.AgMIP_FoodWaste_Share_Pathway_SSP_v8.2"

#' primary_energy_map_v8.2
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM8.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::primary_energy_map_v8.2
#' }
"primary_energy_map_v8.2"

#' production_map_v8.2
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM8.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::production_map_v8.2
#' }
"production_map_v8.2"

#' capacity_map_v8.2
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM8.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::capacity_map_v8.2
#' }
"capacity_map_v8.2"

#' cf_gcam_v8.2
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM8.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::cf_gcam_v8.2
#' }
"cf_gcam_v8.2"

#' cf_rgn_v8.2
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM8.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::cf_rgn_v8.2
#' }
"cf_rgn_v8.2"

#' final_energy_map_v8.2
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM8.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::final_energy_map_v8.2
#' }
"final_energy_map_v8.2"

#' en_demand_price_map_v8.2
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy demand - price maps compatible with GCAM8.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::en_demand_price_map_v8.2
#' }
"en_demand_price_map_v8.2"

#' res_extraction_map_v8.2
#'
#' @source github
#' @format .csv
#' @description resource extraction map compatible with GCAM8.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::res_extraction_map_v8.2
#' }
"res_extraction_map_v8.2"


#' transport_final_en_map_v8.2
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM8.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::transport_final_en_map_v8.2
#' }
"transport_final_en_map_v8.2"

#' energy_price_map_v8.2
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM8.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::energy_price_map_v8.2
#' }
"energy_price_map_v8.2"

#' transport_en_service_v8.2
#'
#' @source github
#' @format .csv
#' @description transport energy services compatible with GCAM8.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::transport_en_service_v8.2
#' }
"transport_en_service_v8.2"

#' en_multiplier_v8.2
#'
#' @source github
#' @format .csv
#' @description energy services compatible with GCAM8.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::en_multiplier_v8.2
#' }
"en_multiplier_v8.2"

#' buildings_en_service_v8.2
#'
#' @source github
#' @format .csv
#' @description buildings energy services compatible with GCAM8.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::buildings_en_service_v8.2
#' }
"buildings_en_service_v8.2"

#' hdd_cdd_v8.2
#'
#' @source github
#' @format .csv
#' @description buildings energy services compatible with GCAM8.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::hdd_cdd_v8.2
#' }
"hdd_cdd_v8.2"

#' capital_gcam_v8.2
#'
#' @source github
#' @format .csv
#' @description capital update compatible with GCAM8.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::capital_gcam_v8.2
#' }
"capital_gcam_v8.2"

#' investment_v8.2
#'
#' @source github
#' @format .csv
#' @description capital investment compatible with GCAM8.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::investment_v8.2
#' }
"investment_v8.2"

#' nonelec_investment_map_v8.2
#'
#' @source github
#' @format .csv
#' @description non-electricity investment mapping compatible with GCAM8.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::nonelec_investment_map_v8.2
#' }
"nonelec_investment_map_v8.2"

#' carbon_content_v8.2
#'
#' @source github
#' @format .csv
#' @description carbon content compatible with GCAM8.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::carbon_content_v8.2
#' }
"carbon_content_v8.2"

#' nonco2_content_v8.2
#'
#' @source github
#' @format .csv
#' @description non CO2 content compatible with GCAM8.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::nonco2_content_v8.2
#' }
"nonco2_content_v8.2"

#' iea_capacity_v8.2
#'
#' @source github
#' @format .csv
#' @description iea 2019 capacity compatible with GCAM8.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::iea_capacity_v8.2
#' }
"iea_capacity_v8.2"

#' co2_market_v8.2
#'
#' @source github
#' @format .csv
#' @description new CO2 market compatible with GCAM8.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::co2_market_v8.2
#' }
"co2_market_v8.2"

#' co2_market_frag_map_v8.2
#'
#' @source github
#' @format .csv
#' @description new CO2 regional markets map compatible with GCAM8.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::co2_market_frag_map_v8.2
#' }
"co2_market_frag_map_v8.2"

#' iron_steel_trade_map_v8.2
#'
#' @source github
#' @format .csv
#' @description iron steel imports and exports map
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::iron_steel_trade_map_v8.2
#' }
"iron_steel_trade_map_v8.2"

#' water_map_v8.2
#'
#' @source github
#' @format .csv
#' @description water withdrawals/consumption map
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::water_map_v8.2
#' }
"water_map_v8.2"

#' conveyance.eff_v8.2
#'
#' @source github
#' @format .csv
#' @description irrigation water conveyance loss factor
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::conveyance.eff_v8.2
#' }
"conveyance.eff_v8.2"

#' ucd_size_class_v8.2
#'
#' @source github
#' @format .csv
#' @description UCD transport size class
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::ucd_size_class_v8.2
#' }
"ucd_size_class_v8.2"

#' ucd_core_v8.2
#'
#' @source github
#' @format .csv
#' @description UCD transport data
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::ucd_core_v8.2
#' }
"ucd_core_v8.2"

#' transport_sales_map_v8.2
#'
#' @source github
#' @format .csv
#' @description transport sales mapping
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::transport_sales_map_v8.2
#' }
"transport_sales_map_v8.2"

#' transport_stock_map_v8.2
#'
#' @source github
#' @format .csv
#' @description transport stock mapping
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::transport_stock_map_v8.2
#' }
"transport_stock_map_v8.2"

#' convert_v8.2
#'
#' @description units conversion list
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::convert_v8.2
#' }
"convert_v8.2"

#' F_GASES_v8.2
#'
#' @description ghg emission conversion.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::F_GASES_v8.2
#' }
"F_GASES_v8.2"

#' GHG_gases_v8.2
#'
#' @description ghg emission conversion.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::GHG_gases_v8.2
#' }
"GHG_gases_v8.2"

#' last_historical_year_v8.2
#'
#' @description last historical year compatible with GCAM8.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::last_historical_year_v8.2
#' }
"last_historical_year_v8.2"

#' reg_cont_v8.2
#'
#' @source local
#' @format .csv
#' @description mapping between regions and continents compatible with GCAM8.2
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::reg_cont_v8.2
#' }
"reg_cont_v8.2"

#' queries_general_vScenarioMIPCMIP7
#'
#' @source local
#' @format vector
#' @description gcamreport queries compatible with GCAM ScenarioMIPCMIP7 version.
#' Contain all queries except for nonCO2 queries
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::queries_general_vScenarioMIPCMIP7
#' }
"queries_general_vScenarioMIPCMIP7"

#' queries_nonCO2_vScenarioMIPCMIP7
#'
#' @source local
#' @format vector
#' @description gcamreport nonCO2 query compatible with GCAM ScenarioMIPCMIP7 version compatible with GCAMScenarioMIPCMIP7.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::queries_nonCO2_vScenarioMIPCMIP7
#' }
"queries_nonCO2_vScenarioMIPCMIP7"


#' nonco2_emissions_list_vScenarioMIPCMIP7
#'
#' @source local
#' @format vector
#' @description nonCO2 pollutants list
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::nonco2_emissions_list_vScenarioMIPCMIP7
#' }
"nonco2_emissions_list_vScenarioMIPCMIP7"

#' var_fun_map_vScenarioMIPCMIP7
#'
#' @source local
#' @format .csv
#' @description mapping between variables, functions to load them, dependent variables, and available verifications compatible with GCAMScenarioMIPCMIP7.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::var_fun_map_vScenarioMIPCMIP7
#' }
"var_fun_map_vScenarioMIPCMIP7"

#' template_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description read in template compatible with GCAMScenarioMIPCMIP7.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::template_vScenarioMIPCMIP7
#' }
"template_vScenarioMIPCMIP7"

#' co2_ets_sector_map_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description emissions maps compatible with GCAMScenarioMIPCMIP7.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::co2_ets_sector_map_vScenarioMIPCMIP7
#' }
"co2_ets_sector_map_vScenarioMIPCMIP7"

#' carbon_densities_vegsoil_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description vegetation and soil carbon densities per basin compatible with GCAMScenarioMIPCMIP7.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::carbon_densities_vegsoil_vScenarioMIPCMIP7
#' }
"carbon_densities_vegsoil_vScenarioMIPCMIP7"

#' co2_tech_map_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description emissions maps compatible with GCAMScenarioMIPCMIP7.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::co2_tech_map_vScenarioMIPCMIP7
#' }
"co2_tech_map_vScenarioMIPCMIP7"

#' co2_resource_map_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description emissions map by resource production compatible with GCAMScenarioMIPCMIP7.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::co2_resource_map_vScenarioMIPCMIP7
#' }
"co2_resource_map_vScenarioMIPCMIP7"

#' kyoto_sector_map_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description emissions maps compatible with GCAMScenarioMIPCMIP7.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::kyoto_sector_map_vScenarioMIPCMIP7
#' }
"kyoto_sector_map_vScenarioMIPCMIP7"

#' nonco2_emis_sector_map_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description emissions maps compatible with GCAMScenarioMIPCMIP7.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::nonco2_emis_sector_map_vScenarioMIPCMIP7
#' }
"nonco2_emis_sector_map_vScenarioMIPCMIP7"

#' nonco2_emis_resource_map_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description emissions maps compatible with GCAMScenarioMIPCMIP7.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::nonco2_emis_resource_map_vScenarioMIPCMIP7
#' }
"nonco2_emis_resource_map_vScenarioMIPCMIP7"

#' carbon_seq_tech_map_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description emissions maps compatible with GCAMScenarioMIPCMIP7.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::carbon_seq_tech_map_vScenarioMIPCMIP7
#' }
"carbon_seq_tech_map_vScenarioMIPCMIP7"

#' ag_demand_map_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description ag maps compatible with GCAMScenarioMIPCMIP7.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::ag_demand_map_vScenarioMIPCMIP7
#' }
"ag_demand_map_vScenarioMIPCMIP7"

#' fertilizer_map_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description ag maps compatible with GCAMScenarioMIPCMIP7.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::fertilizer_map_vScenarioMIPCMIP7
#' }
"fertilizer_map_vScenarioMIPCMIP7"

#' ag_price_map_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description ag maps compatible with GCAMScenarioMIPCMIP7.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::ag_price_map_vScenarioMIPCMIP7
#' }
"ag_price_map_vScenarioMIPCMIP7"

#' ag_demand_price_map_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description ag maps compatible with GCAMScenarioMIPCMIP7.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::ag_demand_price_map_vScenarioMIPCMIP7
#' }
"ag_demand_price_map_vScenarioMIPCMIP7"

#' ag_production_map_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description ag maps compatible with GCAMScenarioMIPCMIP7.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::ag_production_map_vScenarioMIPCMIP7
#' }
"ag_production_map_vScenarioMIPCMIP7"

#' trade_ag_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description ag maps compatible with GCAMScenarioMIPCMIP7.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::trade_ag_vScenarioMIPCMIP7
#' }
"trade_ag_vScenarioMIPCMIP7"

#' land_use_map_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description ag maps compatible with GCAMScenarioMIPCMIP7.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::land_use_map_vScenarioMIPCMIP7
#' }
"land_use_map_vScenarioMIPCMIP7"

#' yield_map_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description ag maps compatible with yield_map_vScenarioMIPCMIP7
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::yield_map_vScenarioMIPCMIP7
#' }
"yield_map_vScenarioMIPCMIP7"

#' food_intake_map_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description food maps compatible with GCAMScenarioMIPCMIP7.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::food_intake_map_vScenarioMIPCMIP7
#' }
"food_intake_map_vScenarioMIPCMIP7"

#' food_items_map_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description food maps compatible with GCAMScenarioMIPCMIP7.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::food_items_map_vScenarioMIPCMIP7
#' }
"food_items_map_vScenarioMIPCMIP7"

#' food_expenditures_average_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description food maps compatible with GCAMScenarioMIPCMIP7.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::food_expenditures_average_vScenarioMIPCMIP7
#' }
"food_expenditures_average_vScenarioMIPCMIP7"

#' L100.AgMIP_FoodWaste_Share_Pathway_SSP_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description food waste share (waste/supppy) compatible with GCAMScenarioMIPCMIP7.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::L100.AgMIP_FoodWaste_Share_Pathway_SSP_vScenarioMIPCMIP7
#' }
"L100.AgMIP_FoodWaste_Share_Pathway_SSP_vScenarioMIPCMIP7"

#' WoodFuel_IndRoundwood_ratio_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description wood fuel - industrial roundwood ratio; compatible with GCAMScenarioMIPCMIP7.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::WoodFuel_IndRoundwood_ratio_vScenarioMIPCMIP7
#' }
"WoodFuel_IndRoundwood_ratio_vScenarioMIPCMIP7"

#' cereal_scaler_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description cereal yield and physical land scaler; compatible with GCAMScenarioMIPCMIP7.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::cereal_scaler_vScenarioMIPCMIP7
#' }
"cereal_scaler_vScenarioMIPCMIP7"

#' primary_energy_map_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAMScenarioMIPCMIP7.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::primary_energy_map_vScenarioMIPCMIP7
#' }
"primary_energy_map_vScenarioMIPCMIP7"

#' production_map_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAMScenarioMIPCMIP7.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::production_map_vScenarioMIPCMIP7
#' }
"production_map_vScenarioMIPCMIP7"

#' capacity_map_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAMScenarioMIPCMIP7.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::capacity_map_vScenarioMIPCMIP7
#' }
"capacity_map_vScenarioMIPCMIP7"

#' cf_gcam_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAMScenarioMIPCMIP7.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::cf_gcam_vScenarioMIPCMIP7
#' }
"cf_gcam_vScenarioMIPCMIP7"

#' cf_rgn_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAMScenarioMIPCMIP7.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::cf_rgn_vScenarioMIPCMIP7
#' }
"cf_rgn_vScenarioMIPCMIP7"

#' final_energy_map_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAMScenarioMIPCMIP7.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::final_energy_map_vScenarioMIPCMIP7
#' }
"final_energy_map_vScenarioMIPCMIP7"

#' en_demand_price_map_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy demand - price maps compatible with GCAMScenarioMIPCMIP7.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::en_demand_price_map_vScenarioMIPCMIP7
#' }
"en_demand_price_map_vScenarioMIPCMIP7"

#' res_extraction_map_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description resource extraction map compatible with GCAMScenarioMIPCMIP7.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::res_extraction_map_vScenarioMIPCMIP7
#' }
"res_extraction_map_vScenarioMIPCMIP7"


#' transport_final_en_map_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAMScenarioMIPCMIP7.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::transport_final_en_map_vScenarioMIPCMIP7
#' }
"transport_final_en_map_vScenarioMIPCMIP7"

#' energy_price_map_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAMScenarioMIPCMIP7.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::energy_price_map_vScenarioMIPCMIP7
#' }
"energy_price_map_vScenarioMIPCMIP7"

#' transport_en_service_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description transport energy services compatible with GCAMScenarioMIPCMIP7.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::transport_en_service_vScenarioMIPCMIP7
#' }
"transport_en_service_vScenarioMIPCMIP7"

#' en_multiplier_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description energy services compatible with GCAMScenarioMIPCMIP7.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::en_multiplier_vScenarioMIPCMIP7
#' }
"en_multiplier_vScenarioMIPCMIP7"

#' buildings_en_service_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description buildings energy services compatible with GCAMScenarioMIPCMIP7.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::buildings_en_service_vScenarioMIPCMIP7
#' }
"buildings_en_service_vScenarioMIPCMIP7"

#' hdd_cdd_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description buildings energy services compatible with GCAMScenarioMIPCMIP7.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::hdd_cdd_vScenarioMIPCMIP7
#' }
"hdd_cdd_vScenarioMIPCMIP7"

#' capital_gcam_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description capital update compatible with GCAMScenarioMIPCMIP7.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::capital_gcam_vScenarioMIPCMIP7
#' }
"capital_gcam_vScenarioMIPCMIP7"

#' investment_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description capital investment compatible with GCAMScenarioMIPCMIP7.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::investment_vScenarioMIPCMIP7
#' }
"investment_vScenarioMIPCMIP7"

#' nonelec_investment_map_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description non-electricity investment mapping compatible with GCAMScenarioMIPCMIP7.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::nonelec_investment_map_vScenarioMIPCMIP7
#' }
"nonelec_investment_map_vScenarioMIPCMIP7"

#' carbon_content_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description carbon content compatible with GCAMScenarioMIPCMIP7.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::carbon_content_vScenarioMIPCMIP7
#' }
"carbon_content_vScenarioMIPCMIP7"

#' nonco2_content_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description non CO2 content compatible with GCAMScenarioMIPCMIP7.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::nonco2_content_vScenarioMIPCMIP7
#' }
"nonco2_content_vScenarioMIPCMIP7"

#' iea_capacity_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description iea 2019 capacity compatible with GCAMScenarioMIPCMIP7.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::iea_capacity_vScenarioMIPCMIP7
#' }
"iea_capacity_vScenarioMIPCMIP7"

#' co2_market_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description new CO2 market compatible with GCAMScenarioMIPCMIP7.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::co2_market_vScenarioMIPCMIP7
#' }
"co2_market_vScenarioMIPCMIP7"

#' co2_market_frag_map_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description new CO2 regional markets map compatible with GCAMScenarioMIPCMIP7.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::co2_market_frag_map_vScenarioMIPCMIP7
#' }
"co2_market_frag_map_vScenarioMIPCMIP7"

#' iron_steel_trade_map_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description iron steel imports and exports map
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::iron_steel_trade_map_vScenarioMIPCMIP7
#' }
"iron_steel_trade_map_vScenarioMIPCMIP7"

#' water_map_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description water withdrawals/consumption map
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::water_map_vScenarioMIPCMIP7
#' }
"water_map_vScenarioMIPCMIP7"

#' conveyance.eff_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description irrigation water conveyance loss factor
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::conveyance.eff_vScenarioMIPCMIP7
#' }
"conveyance.eff_vScenarioMIPCMIP7"

#' ucd_size_class_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description UCD transport size class
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::ucd_size_class_vScenarioMIPCMIP7
#' }
"ucd_size_class_vScenarioMIPCMIP7"

#' ucd_core_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description UCD transport data
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::ucd_core_vScenarioMIPCMIP7
#' }
"ucd_core_vScenarioMIPCMIP7"

#' transport_sales_map_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description transport sales mapping
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::transport_sales_map_vScenarioMIPCMIP7
#' }
"transport_sales_map_vScenarioMIPCMIP7"

#' transport_stock_map_vScenarioMIPCMIP7
#'
#' @source github
#' @format .csv
#' @description transport stock mapping
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::transport_stock_map_vScenarioMIPCMIP7
#' }
"transport_stock_map_vScenarioMIPCMIP7"

#' convert_vScenarioMIPCMIP7
#'
#' @description units conversion list
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::convert_vScenarioMIPCMIP7
#' }
"convert_vScenarioMIPCMIP7"

#' F_GASES_vScenarioMIPCMIP7
#'
#' @description ghg emission conversion.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::F_GASES_vScenarioMIPCMIP7
#' }
"F_GASES_vScenarioMIPCMIP7"

#' GHG_gases_vScenarioMIPCMIP7
#'
#' @description ghg emission conversion.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::GHG_gases_vScenarioMIPCMIP7
#' }
"GHG_gases_vScenarioMIPCMIP7"

#' last_historical_year_vScenarioMIPCMIP7
#'
#' @description last historical year compatible with GCAMScenarioMIPCMIP7.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::last_historical_year_vScenarioMIPCMIP7
#' }
"last_historical_year_vScenarioMIPCMIP7"

#' reg_cont_vScenarioMIPCMIP7
#'
#' @source local
#' @format .csv
#' @description mapping between regions and continents compatible with GCAMScenarioMIPCMIP7
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::reg_cont_vScenarioMIPCMIP7
#' }
"reg_cont_vScenarioMIPCMIP7"



#' queries_general_v7.2
#'
#' @source local
#' @format vector
#' @description gcamreport queries compatible with GCAM 7.2 version.
#' Contain all queries except for nonCO2 queries
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::queries_general_v7.2
#' }
"queries_general_v7.2"

#' queries_nonCO2_v7.2
#'
#' @source local
#' @format vector
#' @description gcamreport nonCO2 query compatible with GCAM 7.2 version compatible with GCAM7.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::queries_nonCO2_v7.2
#' }
"queries_nonCO2_v7.2"


#' nonco2_emissions_list_v7.2
#'
#' @source local
#' @format vector
#' @description nonCO2 pollutants list
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::nonco2_emissions_list_v7.2
#' }
"nonco2_emissions_list_v7.2"

#' var_fun_map_v7.2
#'
#' @source local
#' @format .csv
#' @description mapping between variables, functions to load them, dependent variables, and available verifications compatible with GCAM7.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::var_fun_map_v7.2
#' }
"var_fun_map_v7.2"

#' template_v7.2
#'
#' @source github
#' @format .csv
#' @description read in template compatible with GCAM7.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::template_v7.2
#' }
"template_v7.2"

#' co2_ets_sector_map_v7.2
#'
#' @source github
#' @format .csv
#' @description emissions maps compatible with GCAM7.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::co2_ets_sector_map_v7.2
#' }
"co2_ets_sector_map_v7.2"

#' co2_tech_map_v7.2
#'
#' @source github
#' @format .csv
#' @description emissions maps compatible with GCAM7.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::co2_tech_map_v7.2
#' }
"co2_tech_map_v7.2"

#' co2_resource_map_v7.2
#'
#' @source github
#' @format .csv
#' @description emissions map by resource production compatible with GCAM7.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::co2_resource_map_v7.2
#' }
"co2_resource_map_v7.2"

#' kyoto_sector_map_v7.2
#'
#' @source github
#' @format .csv
#' @description emissions maps compatible with GCAM7.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::kyoto_sector_map_v7.2
#' }
"kyoto_sector_map_v7.2"

#' nonco2_emis_sector_map_v7.2
#'
#' @source github
#' @format .csv
#' @description emissions maps compatible with GCAM7.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::nonco2_emis_sector_map_v7.2
#' }
"nonco2_emis_sector_map_v7.2"

#' nonco2_emis_resource_map_v7.2
#'
#' @source github
#' @format .csv
#' @description emissions maps compatible with GCAM7.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::nonco2_emis_resource_map_v7.2
#' }
"nonco2_emis_resource_map_v7.2"

#' carbon_seq_tech_map_v7.2
#'
#' @source github
#' @format .csv
#' @description emissions maps compatible with GCAM7.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::carbon_seq_tech_map_v7.2
#' }
"carbon_seq_tech_map_v7.2"

#' ag_demand_map_v7.2
#'
#' @source github
#' @format .csv
#' @description ag maps compatible with GCAM7.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::ag_demand_map_v7.2
#' }
"ag_demand_map_v7.2"

#' fertilizer_map_v7.2
#'
#' @source github
#' @format .csv
#' @description ag maps compatible with GCAM7.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::fertilizer_map_v7.2
#' }
"fertilizer_map_v7.2"

#' ag_price_map_v7.2
#'
#' @source github
#' @format .csv
#' @description ag maps compatible with GCAM7.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::ag_price_map_v7.2
#' }
"ag_price_map_v7.2"

#' ag_demand_price_map_v7.2
#'
#' @source github
#' @format .csv
#' @description ag maps compatible with GCAM7.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::ag_demand_price_map_v7.2
#' }
"ag_demand_price_map_v7.2"

#' ag_production_map_v7.2
#'
#' @source github
#' @format .csv
#' @description ag maps compatible with GCAM7.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::ag_production_map_v7.2
#' }
"ag_production_map_v7.2"

#' trade_ag_v7.2
#'
#' @source github
#' @format .csv
#' @description ag maps compatible with GCAM7.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::trade_ag_v7.2
#' }
"trade_ag_v7.2"

#' land_use_map_v7.2
#'
#' @source github
#' @format .csv
#' @description ag maps compatible with GCAM7.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::land_use_map_v7.2
#' }
"land_use_map_v7.2"

#' yield_map_v7.2
#'
#' @source github
#' @format .csv
#' @description ag maps compatible with yield_map_v7.2
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::yield_map_v7.2
#' }
"yield_map_v7.2"

#' food_intake_map_v7.2
#'
#' @source github
#' @format .csv
#' @description food maps compatible with GCAM7.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::food_intake_map_v7.2
#' }
"food_intake_map_v7.2"

#' food_items_map_v7.2
#'
#' @source github
#' @format .csv
#' @description food maps compatible with GCAM7.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::food_items_map_v7.2
#' }
"food_items_map_v7.2"

#' food_expenditures_average_v7.2
#'
#' @source github
#' @format .csv
#' @description food maps compatible with GCAM7.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::food_expenditures_average_v7.2
#' }
"food_expenditures_average_v7.2"

#' L100.AgMIP_FoodWaste_Share_Pathway_SSP_v7.2
#'
#' @source github
#' @format .csv
#' @description food waste share (waste/supppy) compatible with GCAM7.2
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::L100.AgMIP_FoodWaste_Share_Pathway_SSP_v7.2
#' }
"L100.AgMIP_FoodWaste_Share_Pathway_SSP_v7.2"

#' primary_energy_map_v7.2
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM7.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::primary_energy_map_v7.2
#' }
"primary_energy_map_v7.2"

#' production_map_v7.2
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM7.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::production_map_v7.2
#' }
"production_map_v7.2"

#' capacity_map_v7.2
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM7.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::capacity_map_v7.2
#' }
"capacity_map_v7.2"

#' cf_gcam_v7.2
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM7.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::cf_gcam_v7.2
#' }
"cf_gcam_v7.2"

#' cf_rgn_v7.2
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM7.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::cf_rgn_v7.2
#' }
"cf_rgn_v7.2"

#' final_energy_map_v7.2
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM7.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::final_energy_map_v7.2
#' }
"final_energy_map_v7.2"

#' en_demand_price_map_v7.2
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy demand - price maps compatible with GCAM7.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::en_demand_price_map_v7.2
#' }
"en_demand_price_map_v7.2"

#' res_extraction_map_v7.2
#'
#' @source github
#' @format .csv
#' @description resource extraction map compatible with GCAM7.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::res_extraction_map_v7.2
#' }
"res_extraction_map_v7.2"


#' transport_final_en_map_v7.2
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM7.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::transport_final_en_map_v7.2
#' }
"transport_final_en_map_v7.2"

#' energy_price_map_v7.2
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM7.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::energy_price_map_v7.2
#' }
"energy_price_map_v7.2"

#' transport_en_service_v7.2
#'
#' @source github
#' @format .csv
#' @description transport energy services compatible with GCAM7.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::transport_en_service_v7.2
#' }
"transport_en_service_v7.2"

#' en_multiplier_v7.2
#'
#' @source github
#' @format .csv
#' @description energy services compatible with GCAM7.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::en_multiplier_v7.2
#' }
"en_multiplier_v7.2"

#' buildings_en_service_v7.2
#'
#' @source github
#' @format .csv
#' @description buildings energy services compatible with GCAM7.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::buildings_en_service_v7.2
#' }
"buildings_en_service_v7.2"

#' hdd_cdd_v7.2
#'
#' @source github
#' @format .csv
#' @description buildings energy services compatible with GCAM7.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::hdd_cdd_v7.2
#' }
"hdd_cdd_v7.2"

#' capital_gcam_v7.2
#'
#' @source github
#' @format .csv
#' @description capital update compatible with GCAM7.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::capital_gcam_v7.2
#' }
"capital_gcam_v7.2"

#' investment_v7.2
#'
#' @source github
#' @format .csv
#' @description capital investment compatible with GCAM7.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::investment_v7.2
#' }
"investment_v7.2"

#' nonelec_investment_map_v7.2
#'
#' @source github
#' @format .csv
#' @description non-electricity investment mapping compatible with GCAM7.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::nonelec_investment_map_v7.2
#' }
"nonelec_investment_map_v7.2"

#' carbon_content_v7.2
#'
#' @source github
#' @format .csv
#' @description carbon content compatible with GCAM7.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::carbon_content_v7.2
#' }
"carbon_content_v7.2"

#' nonco2_content_v7.2
#'
#' @source github
#' @format .csv
#' @description non CO2 content compatible with GCAM7.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::nonco2_content_v7.2
#' }
"nonco2_content_v7.2"

#' iea_capacity_v7.2
#'
#' @source github
#' @format .csv
#' @description iea 2019 capacity compatible with GCAM7.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::iea_capacity_v7.2
#' }
"iea_capacity_v7.2"

#' co2_market_v7.2
#'
#' @source github
#' @format .csv
#' @description new CO2 market compatible with GCAM7.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::co2_market_v7.2
#' }
"co2_market_v7.2"

#' co2_market_frag_map_v7.2
#'
#' @source github
#' @format .csv
#' @description new CO2 regional markets map compatible with GCAM7.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::co2_market_frag_map_v7.2
#' }
"co2_market_frag_map_v7.2"

#' iron_steel_trade_map_v7.2
#'
#' @source github
#' @format .csv
#' @description iron steel imports and exports map
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::iron_steel_trade_map_v7.2
#' }
"iron_steel_trade_map_v7.2"

#' water_map_v7.2
#'
#' @source github
#' @format .csv
#' @description water withdrawals/consumption map
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::water_map_v7.2
#' }
"water_map_v7.2"

#' conveyance.eff_v7.2
#'
#' @source github
#' @format .csv
#' @description irrigation water conveyance loss factor
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::conveyance.eff_v7.2
#' }
"conveyance.eff_v7.2"

#' ucd_size_class_v7.2
#'
#' @source github
#' @format .csv
#' @description UCD transport size class
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::ucd_size_class_v7.2
#' }
"ucd_size_class_v7.2"

#' ucd_core_v7.2
#'
#' @source github
#' @format .csv
#' @description UCD transport data
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::ucd_core_v7.2
#' }
"ucd_core_v7.2"

#' transport_sales_map_v7.2
#'
#' @source github
#' @format .csv
#' @description transport sales mapping
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::transport_sales_map_v7.2
#' }
"transport_sales_map_v7.2"

#' transport_stock_map_v7.2
#'
#' @source github
#' @format .csv
#' @description transport stock mapping
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::transport_stock_map_v7.2
#' }
"transport_stock_map_v7.2"

#' convert_v7.2
#'
#' @description units conversion list
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::convert_v7.2
#' }
"convert_v7.2"

#' F_GASES_v7.2
#'
#' @description ghg emission conversion.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::F_GASES_v7.2
#' }
"F_GASES_v7.2"

#' GHG_gases_v7.2
#'
#' @description ghg emission conversion.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::GHG_gases_v7.2
#' }
"GHG_gases_v7.2"

#' last_historical_year_v7.2
#'
#' @description last historical year compatible with GCAM7.2.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::last_historical_year_v7.2
#' }
"last_historical_year_v7.2"

#' reg_cont_v7.2
#'
#' @source local
#' @format .csv
#' @description mapping between regions and continents compatible with GCAM7.2
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::reg_cont_v7.2
#' }
"reg_cont_v7.2"


#' queries_general_v7.1
#'
#' @source local
#' @format vector
#' @description gcamreport queries compatible with GCAM 7.1 version.
#' Contain all queries except for nonCO2 queries
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::queries_general_v7.1
#' }
"queries_general_v7.1"

#' queries_nonCO2_v7.1
#'
#' @source local
#' @format vector
#' @description gcamreport nonCO2 query compatible with GCAM 7.1 version compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::queries_nonCO2_v7.1
#' }
"queries_nonCO2_v7.1"


#' nonco2_emissions_list_v7.1
#'
#' @source local
#' @format vector
#' @description nonCO2 pollutants list
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::nonco2_emissions_list_v7.1
#' }
"nonco2_emissions_list_v7.1"

#' var_fun_map_v7.1
#'
#' @source local
#' @format .csv
#' @description mapping between variables, functions to load them, dependent variables, and available verifications compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::var_fun_map_v7.1
#' }
"var_fun_map_v7.1"

#' template_v7.1
#'
#' @source github
#' @format .csv
#' @description read in template compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::template_v7.1
#' }
"template_v7.1"

#' co2_ets_sector_map_v7.1
#'
#' @source github
#' @format .csv
#' @description emissions maps compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::co2_ets_sector_map_v7.1
#' }
"co2_ets_sector_map_v7.1"

#' co2_tech_map_v7.1
#'
#' @source github
#' @format .csv
#' @description emissions maps compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::co2_tech_map_v7.1
#' }
"co2_tech_map_v7.1"

#' co2_resource_map_v7.1
#'
#' @source github
#' @format .csv
#' @description emissions map by resource production compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::co2_resource_map_v7.1
#' }
"co2_resource_map_v7.1"

#' kyoto_sector_map_v7.1
#'
#' @source github
#' @format .csv
#' @description emissions maps compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::kyoto_sector_map_v7.1
#' }
"kyoto_sector_map_v7.1"

#' nonco2_emis_sector_map_v7.1
#'
#' @source github
#' @format .csv
#' @description emissions maps compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::nonco2_emis_sector_map_v7.1
#' }
"nonco2_emis_sector_map_v7.1"

#' nonco2_emis_resource_map_v7.1
#'
#' @source github
#' @format .csv
#' @description emissions maps compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::nonco2_emis_resource_map_v7.1
#' }
"nonco2_emis_resource_map_v7.1"

#' carbon_seq_tech_map_v7.1
#'
#' @source github
#' @format .csv
#' @description emissions maps compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::carbon_seq_tech_map_v7.1
#' }
"carbon_seq_tech_map_v7.1"

#' ag_demand_map_v7.1
#'
#' @source github
#' @format .csv
#' @description ag maps compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::ag_demand_map_v7.1
#' }
"ag_demand_map_v7.1"

#' fertilizer_map_v7.1
#'
#' @source github
#' @format .csv
#' @description ag maps compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::fertilizer_map_v7.1
#' }
"fertilizer_map_v7.1"

#' ag_price_map_v7.1
#'
#' @source github
#' @format .csv
#' @description ag maps compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::ag_price_map_v7.1
#' }
"ag_price_map_v7.1"

#' ag_demand_price_map_v7.1
#'
#' @source github
#' @format .csv
#' @description ag maps compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::ag_demand_price_map_v7.1
#' }
"ag_demand_price_map_v7.1"

#' ag_production_map_v7.1
#'
#' @source github
#' @format .csv
#' @description ag maps compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::ag_production_map_v7.1
#' }
"ag_production_map_v7.1"

#' trade_ag_v7.1
#'
#' @source github
#' @format .csv
#' @description ag maps compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::trade_ag_v7.1
#' }
"trade_ag_v7.1"

#' land_use_map_v7.1
#'
#' @source github
#' @format .csv
#' @description ag maps compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::land_use_map_v7.1
#' }
"land_use_map_v7.1"


#' yield_map_v7.1
#'
#' @source github
#' @format .csv
#' @description ag maps compatible with yield_map_v7.1
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::yield_map_v7.1
#' }
"yield_map_v7.1"

#' food_intake_map_v7.1
#'
#' @source github
#' @format .csv
#' @description food maps compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::food_intake_map_v7.1
#' }
"food_intake_map_v7.1"

#' food_items_map_v7.1
#'
#' @source github
#' @format .csv
#' @description food maps compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::food_items_map_v7.1
#' }
"food_items_map_v7.1"

#' food_expenditures_average_v7.1
#'
#' @source github
#' @format .csv
#' @description food maps compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::food_expenditures_average_v7.1
#' }
"food_expenditures_average_v7.1"

#' L100.AgMIP_FoodWaste_Share_Pathway_SSP_v7.1
#'
#' @source github
#' @format .csv
#' @description food waste share (waste/supppy) compatible with GCAM7.1
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::L100.AgMIP_FoodWaste_Share_Pathway_SSP_v7.1
#' }
"L100.AgMIP_FoodWaste_Share_Pathway_SSP_v7.1"

#' primary_energy_map_v7.1
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::primary_energy_map_v7.1
#' }
"primary_energy_map_v7.1"

#' production_map_v7.1
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::production_map_v7.1
#' }
"production_map_v7.1"

#' capacity_map_v7.1
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::capacity_map_v7.1
#' }
"capacity_map_v7.1"

#' cf_gcam_v7.1
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::cf_gcam_v7.1
#' }
"cf_gcam_v7.1"

#' cf_rgn_v7.1
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::cf_rgn_v7.1
#' }
"cf_rgn_v7.1"

#' final_energy_map_v7.1
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::final_energy_map_v7.1
#' }
"final_energy_map_v7.1"

#' en_demand_price_map_v7.1
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy demand - price maps compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::en_demand_price_map_v7.1
#' }
"en_demand_price_map_v7.1"

#' res_extraction_map_v7.1
#'
#' @source github
#' @format .csv
#' @description resource extraction map compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::res_extraction_map_v7.1
#' }
"res_extraction_map_v7.1"

#' transport_final_en_map_v7.1
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::transport_final_en_map_v7.1
#' }
"transport_final_en_map_v7.1"

#' energy_price_map_v7.1
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::energy_price_map_v7.1
#' }
"energy_price_map_v7.1"

#' transport_en_service_v7.1
#'
#' @source github
#' @format .csv
#' @description transport energy services compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::transport_en_service_v7.1
#' }
"transport_en_service_v7.1"

#' en_multiplier_v7.1
#'
#' @source github
#' @format .csv
#' @description energy services compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::en_multiplier_v7.1
#' }
"en_multiplier_v7.1"

#' buildings_en_service_v7.1
#'
#' @source github
#' @format .csv
#' @description buildings energy services compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::buildings_en_service_v7.1
#' }
"buildings_en_service_v7.1"

#' hdd_cdd_v7.1
#'
#' @source github
#' @format .csv
#' @description buildings energy services compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::hdd_cdd_v7.1
#' }
"hdd_cdd_v7.1"

#' capital_gcam_v7.1
#'
#' @source github
#' @format .csv
#' @description capital update compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::capital_gcam_v7.1
#' }
"capital_gcam_v7.1"

#' investment_v7.1
#'
#' @source github
#' @format .csv
#' @description capital investment compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::investment_v7.1
#' }
"investment_v7.1"

#' nonelec_investment_map_v7.1
#'
#' @source github
#' @format .csv
#' @description non-electricity investment mapping compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::nonelec_investment_map_v7.1
#' }
"nonelec_investment_map_v7.1"

#' carbon_content_v7.1
#'
#' @source github
#' @format .csv
#' @description carbon content compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::carbon_content_v7.1
#' }
"carbon_content_v7.1"

#' nonco2_content_v7.1
#'
#' @source github
#' @format .csv
#' @description non CO2 content compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::nonco2_content_v7.1
#' }
"nonco2_content_v7.1"

#' iea_capacity_v7.1
#'
#' @source github
#' @format .csv
#' @description iea 2019 capacity compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::iea_capacity_v7.1
#' }
"iea_capacity_v7.1"

#' co2_market_v7.1
#'
#' @source github
#' @format .csv
#' @description new CO2 market compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::co2_market_v7.1
#' }
"co2_market_v7.1"

#' co2_market_frag_map_v7.1
#'
#' @source github
#' @format .csv
#' @description new CO2 regional markets map compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::co2_market_frag_map_v7.1
#' }
"co2_market_frag_map_v7.1"

#' iron_steel_trade_map_v7.1
#'
#' @source github
#' @format .csv
#' @description iron steel imports and exports map
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::iron_steel_trade_map_v7.1
#' }
"iron_steel_trade_map_v7.1"

#' water_map_v7.1
#'
#' @source github
#' @format .csv
#' @description water withdrawals/consumption map
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::water_map_v7.1
#' }
"water_map_v7.1"

#' conveyance.eff_v7.1
#'
#' @source github
#' @format .csv
#' @description irrigation water conveyance loss factor
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::conveyance.eff_v7.1
#' }
"conveyance.eff_v7.1"

#' ucd_size_class_v7.1
#'
#' @source github
#' @format .csv
#' @description UCD transport size class
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::ucd_size_class_v7.1
#' }
"ucd_size_class_v7.1"

#' ucd_core_v7.1
#'
#' @source github
#' @format .csv
#' @description UCD transport data
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::ucd_core_v7.1
#' }
"ucd_core_v7.1"

#' transport_sales_map_v7.1
#'
#' @source github
#' @format .csv
#' @description transport sales mapping
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::transport_sales_map_v7.1
#' }
"transport_sales_map_v7.1"

#' transport_stock_map_v7.1
#'
#' @source github
#' @format .csv
#' @description transport stock mapping
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::transport_stock_map_v7.1
#' }
"transport_stock_map_v7.1"

#' convert_v7.1
#'
#' @description units conversion list
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::convert_v7.1
#' }
"convert_v7.1"

#' F_GASES_v7.1
#'
#' @description ghg emission conversion.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::F_GASES_v7.1
#' }
"F_GASES_v7.1"

#' GHG_gases_v7.1
#'
#' @description ghg emission conversion.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::GHG_gases_v7.1
#' }
"GHG_gases_v7.1"

#' last_historical_year_v7.1
#'
#' @description last historical year compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::last_historical_year_v7.1
#' }
"last_historical_year_v7.1"

#' reg_cont_v7.1
#'
#' @source local
#' @format .csv
#' @description mapping between regions and continents compatible with GCAM7.1
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::reg_cont_v7.1
#' }
"reg_cont_v7.1"


#' queries_general_v7.0
#'
#' @source local
#' @format vector
#' @description gcamreport queries compatible with GCAM 7.0 version.
#' Contain all queries except for nonCO2 queries
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::queries_general_v7.0
#' }
"queries_general_v7.0"

#' queries_nonCO2_v7.0
#'
#' @source local
#' @format vector
#' @description gcamreport nonCO2 query compatible with GCAM 7.0 version.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::queries_nonCO2_v7.0
#' }
"queries_nonCO2_v7.0"


#' nonco2_emissions_list_v7.0
#'
#' @source local
#' @format vector
#' @description nonCO2 pollutants list
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::nonco2_emissions_list_v7.0
#' }
"nonco2_emissions_list_v7.0"

#' var_fun_map_v7.0
#'
#' @source local
#' @format .csv
#' @description mapping between variables, functions to load them, dependent variables, and available verifications compatible with GCAM7.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::var_fun_map_v7.0
#' }
"var_fun_map_v7.0"

#' template_v7.0
#'
#' @source github
#' @format .csv
#' @description read in template compatible with GCAM7.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::template_v7.0
#' }
"template_v7.0"

#' co2_ets_sector_map_v7.0
#'
#' @source github
#' @format .csv
#' @description emissions maps compatible with GCAM7.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::co2_ets_sector_map_v7.0
#' }
"co2_ets_sector_map_v7.0"

#' co2_tech_map_v7.0
#'
#' @source github
#' @format .csv
#' @description emissions maps compatible with GCAM7.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::co2_tech_map_v7.0
#' }
"co2_tech_map_v7.0"

#' co2_resource_map_v7.0
#'
#' @source github
#' @format .csv
#' @description emissions map by resource production compatible with GCAM7.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::co2_resource_map_v7.0
#' }
"co2_resource_map_v7.0"

#' kyoto_sector_map_v7.0
#'
#' @source github
#' @format .csv
#' @description emissions maps compatible with GCAM7.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::kyoto_sector_map_v7.0
#' }
"kyoto_sector_map_v7.0"

#' nonco2_emis_sector_map_v7.0
#'
#' @source github
#' @format .csv
#' @description emissions maps compatible with GCAM7.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::nonco2_emis_sector_map_v7.0
#' }
"nonco2_emis_sector_map_v7.0"

#' nonco2_emis_resource_map_v7.0
#'
#' @source github
#' @format .csv
#' @description emissions maps compatible with GCAM7.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::nonco2_emis_resource_map_v7.0
#' }
"nonco2_emis_resource_map_v7.0"

#' carbon_seq_tech_map_v7.0
#'
#' @source github
#' @format .csv
#' @description emissions maps compatible with GCAM7.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::carbon_seq_tech_map_v7.0
#' }
"carbon_seq_tech_map_v7.0"

#' ag_demand_map_v7.0
#'
#' @source github
#' @format .csv
#' @description ag maps compatible with GCAM7.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::ag_demand_map_v7.0
#' }
"ag_demand_map_v7.0"

#' fertilizer_map_v7.0
#'
#' @source github
#' @format .csv
#' @description ag maps compatible with GCAM7.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::fertilizer_map_v7.0
#' }
"fertilizer_map_v7.0"

#' ag_price_map_v7.0
#'
#' @source github
#' @format .csv
#' @description ag maps compatible with GCAM7.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::ag_price_map_v7.0
#' }
"ag_price_map_v7.0"

#' ag_demand_price_map_v7.0
#'
#' @source github
#' @format .csv
#' @description ag maps compatible with GCAM7.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::ag_demand_price_map_v7.0
#' }
"ag_demand_price_map_v7.0"

#' ag_production_map_v7.0
#'
#' @source github
#' @format .csv
#' @description ag maps compatible with GCAM7.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::ag_production_map_v7.0
#' }
"ag_production_map_v7.0"

#' trade_ag_v7.0
#'
#' @source github
#' @format .csv
#' @description ag maps compatible with GCAM7.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::trade_ag_v7.0
#' }
"trade_ag_v7.0"

#' land_use_map_v7.0
#'
#' @source github
#' @format .csv
#' @description ag maps compatible with GCAM7.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::land_use_map_v7.0
#' }
"land_use_map_v7.0"


#' yield_map_v7.0
#'
#' @source github
#' @format .csv
#' @description ag maps compatible with yield_map_v7.0
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::yield_map_v7.0
#' }
"yield_map_v7.0"

#' food_intake_map_v7.0
#'
#' @source github
#' @format .csv
#' @description food maps compatible with GCAM7.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::food_intake_map_v7.0
#' }
"food_intake_map_v7.0"

#' food_items_map_v7.0
#'
#' @source github
#' @format .csv
#' @description food maps compatible with GCAM7.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::food_items_map_v7.0
#' }
"food_items_map_v7.0"

#' L100.AgMIP_FoodWaste_Share_Pathway_SSP_v7.0
#'
#' @source github
#' @format .csv
#' @description food waste share (waste/supppy) compatible with GCAM7.0
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::L100.AgMIP_FoodWaste_Share_Pathway_SSP_v7.0
#' }
"L100.AgMIP_FoodWaste_Share_Pathway_SSP_v7.0"

#' primary_energy_map_v7.0
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM7.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::primary_energy_map_v7.0
#' }
"primary_energy_map_v7.0"

#' production_map_v7.0
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM7.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::production_map_v7.0
#' }
"production_map_v7.0"

#' capacity_map_v7.0
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM7.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::capacity_map_v7.0
#' }
"capacity_map_v7.0"

#' cf_gcam_v7.0
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM7.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::cf_gcam_v7.0
#' }
"cf_gcam_v7.0"

#' cf_rgn_v7.0
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM7.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::cf_rgn_v7.0
#' }
"cf_rgn_v7.0"

#' final_energy_map_v7.0
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM7.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::final_energy_map_v7.0
#' }
"final_energy_map_v7.0"

#' en_demand_price_map_v7.0
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy demand - price maps compatible with GCAM7.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::en_demand_price_map_v7.0
#' }

"en_demand_price_map_v7.0"

#' res_extraction_map_v7.0
#'
#' @source github
#' @format .csv
#' @description resource extraction map compatible with GCAM7.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::res_extraction_map_v7.0
#' }
"res_extraction_map_v7.0"

#' transport_final_en_map_v7.0
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM7.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::transport_final_en_map_v7.0
#' }
"transport_final_en_map_v7.0"

#' energy_price_map_v7.0
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM7.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::energy_price_map_v7.0
#' }
"energy_price_map_v7.0"

#' transport_en_service_v7.0
#'
#' @source github
#' @format .csv
#' @description transport energy services compatible with GCAM7.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::transport_en_service_v7.0
#' }
"transport_en_service_v7.0"

#' buildings_en_service_v7.0
#'
#' @source github
#' @format .csv
#' @description buildings energy services compatible with GCAM7.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::buildings_en_service_v7.0
#' }
"buildings_en_service_v7.0"

#' capital_gcam_v7.0
#'
#' @source github
#' @format .csv
#' @description capital update compatible with GCAM7.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::capital_gcam_v7.0
#' }
"capital_gcam_v7.0"

#' investment_v7.0
#'
#' @source github
#' @format .csv
#' @description capital investment compatible with GCAM7.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::investment_v7.0
#' }
"investment_v7.0"

#' nonelec_investment_map_v7.0
#'
#' @source github
#' @format .csv
#' @description non-electricity investment mapping compatible with GCAM7.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::nonelec_investment_map_v7.0
#' }
"nonelec_investment_map_v7.0"

#' carbon_content_v7.0
#'
#' @source github
#' @format .csv
#' @description carbon content compatible with GCAM7.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::carbon_content_v7.0
#' }
"carbon_content_v7.0"

#' nonco2_content_v7.0
#'
#' @source github
#' @format .csv
#' @description non CO2 content compatible with GCAM7.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::nonco2_content_v7.0
#' }
"nonco2_content_v7.0"

#' iea_capacity_v7.0
#'
#' @source github
#' @format .csv
#' @description iea 2019 capacity compatible with GCAM7.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::iea_capacity_v7.0
#' }
"iea_capacity_v7.0"

#' co2_market_v7.0
#'
#' @source github
#' @format .csv
#' @description new CO2 market compatible with GCAM7.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::co2_market_v7.0
#' }
"co2_market_v7.0"

#' co2_market_frag_map_v7.0
#'
#' @source github
#' @format .csv
#' @description new CO2 regional markets map compatible with GCAM7.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::co2_market_frag_map_v7.0
#' }
"co2_market_frag_map_v7.0"

#' iron_steel_trade_map_v7.0
#'
#' @source github
#' @format .csv
#' @description iron steel imports and exports map
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::iron_steel_trade_map_v7.0
#' }
"iron_steel_trade_map_v7.0"

#' water_map_v7.0
#'
#' @source github
#' @format .csv
#' @description water withdrawals/consumption map
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::water_map_v7.0
#' }
"water_map_v7.0"

#' conveyance.eff_v7.0
#'
#' @source github
#' @format .csv
#' @description irrigation water conveyance loss factor
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::conveyance.eff_v7.0
#' }
"conveyance.eff_v7.0"

#' ucd_size_class_v7.0
#'
#' @source github
#' @format .csv
#' @description UCD transport size class
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::ucd_size_class_v7.0
#' }
"ucd_size_class_v7.0"

#' ucd_core_v7.0
#'
#' @source github
#' @format .csv
#' @description UCD transport data
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::ucd_core_v7.0
#' }
"ucd_core_v7.0"

#' transport_sales_map_v7.0
#'
#' @source github
#' @format .csv
#' @description transport sales mapping
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::transport_sales_map_v7.0
#' }
"transport_sales_map_v7.0"

#' transport_stock_map_v7.0
#'
#' @source github
#' @format .csv
#' @description transport stock mapping
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::transport_stock_map_v7.0
#' }
"transport_stock_map_v7.0"

#' convert_v7.0
#'
#' @description units conversion list
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::convert_v7.0
#' }
"convert_v7.0"

#' F_GASES_v7.0
#'
#' @description ghg emission conversion.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::F_GASES_v7.0
#' }
"F_GASES_v7.0"

#' GHG_gases_v7.0
#'
#' @description ghg emission conversion.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::GHG_gases_v7.0
#' }
"GHG_gases_v7.0"

#' last_historical_year_v7.0
#'
#' @description last historical year compatible with GCAM7.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::last_historical_year_v7.0
#' }
"last_historical_year_v7.0"

#' reg_cont_v7.0
#'
#' @source local
#' @format .csv
#' @description mapping between regions and continents compatible with GCAM7.0
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::reg_cont_v7.0
#' }
"reg_cont_v7.0"

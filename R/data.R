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

#' gcam_years
#'
#' @source local
#' @format vector
#' @description GCAM model years
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::gcam_years
#' }
"gcam_years"

#' available_reporting_years
#'
#' @source local
#' @format vector
#' @description gcamreport reporting available years
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::available_reporting_years
#' }
"available_reporting_years"

#' available_final_year
#'
#' @source local
#' @format vector
#' @description gcamreport available final years
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::available_final_year
#' }
"available_final_year"

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

#' co2_sector_map_v7.1
#'
#' @source github
#' @format .csv
#' @description emissions maps compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::co2_sector_map_v7.1
#' }
"co2_sector_map_v7.1"

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

#' ag_prices_map_v7.1
#'
#' @source github
#' @format .csv
#' @description ag maps compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::ag_prices_map_v7.1
#' }
"ag_prices_map_v7.1"

#' ag_demand_prices_map_v7.1
#'
#' @source github
#' @format .csv
#' @description ag maps compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::ag_demand_prices_map_v7.1
#' }
"ag_demand_prices_map_v7.1"

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

#' elec_gen_map_v7.1
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::elec_gen_map_v7.1
#' }
"elec_gen_map_v7.1"

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

#' se_gen_map_v7.1
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::se_gen_map_v7.1
#' }
"se_gen_map_v7.1"

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

#' en_demand_prices_map_v7.1
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy demand - price maps compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::en_demand_prices_map_v7.1
#' }
"en_demand_prices_map_v7.1"

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

#' energy_prices_map_v7.1
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM7.1.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::energy_prices_map_v7.1
#' }
"energy_prices_map_v7.1"

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

#' co2_sector_map_v7.0
#'
#' @source github
#' @format .csv
#' @description emissions maps compatible with GCAM7.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::co2_sector_map_v7.0
#' }
"co2_sector_map_v7.0"

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

#' ag_prices_map_v7.0
#'
#' @source github
#' @format .csv
#' @description ag maps compatible with GCAM7.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::ag_prices_map_v7.0
#' }
"ag_prices_map_v7.0"

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

#' elec_gen_map_v7.0
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM7.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::elec_gen_map_v7.0
#' }
"elec_gen_map_v7.0"

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

#' se_gen_map_v7.0
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM7.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::se_gen_map_v7.0
#' }
"se_gen_map_v7.0"

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

#' energy_prices_map_v7.0
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM7.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::energy_prices_map_v7.0
#' }
"energy_prices_map_v7.0"

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


#' queries_general_v6.0
#'
#' @source local
#' @format vector
#' @description gcamreport queries compatible with GCAM 6.0 version.
#' Contain all queries except for nonCO2 queries
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::queries_general_v6.0
#' }
"queries_general_v6.0"

#' queries_nonCO2_v6.0
#'
#' @source local
#' @format vector
#' @description gcamreport nonCO2 query compatible with GCAM 6.0 version compatible with GCAM6.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::queries_nonCO2_v6.0
#' }
"queries_nonCO2_v6.0"


#' nonco2_emissions_list_v6.0
#'
#' @source local
#' @format vector
#' @description nonCO2 pollutants list
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::nonco2_emissions_list_v6.0
#' }
"nonco2_emissions_list_v6.0"

#' var_fun_map_v6.0
#'
#' @source local
#' @format .csv
#' @description mapping between variables, functions to load them, dependent variables, and available verifications compatible with GCAM6.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::var_fun_map_v6.0
#' }
"var_fun_map_v6.0"

#' template_v6.0
#'
#' @source github
#' @format .csv
#' @description read in template compatible with GCAM6.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::template_v6.0
#' }
"template_v6.0"

#' co2_sector_map_v6.0
#'
#' @source github
#' @format .csv
#' @description emissions maps compatible with GCAM6.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::co2_sector_map_v6.0
#' }
"co2_sector_map_v6.0"

#' co2_ets_sector_map_v6.0
#'
#' @source github
#' @format .csv
#' @description emissions maps compatible with GCAM6.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::co2_ets_sector_map_v6.0
#' }
"co2_ets_sector_map_v6.0"

#' co2_tech_map_v6.0
#'
#' @source github
#' @format .csv
#' @description emissions maps compatible with GCAM6.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::co2_tech_map_v6.0
#' }
"co2_tech_map_v6.0"

#' kyoto_sector_map_v6.0
#'
#' @source github
#' @format .csv
#' @description emissions maps compatible with GCAM6.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::kyoto_sector_map_v6.0
#' }
"kyoto_sector_map_v6.0"

#' nonco2_emis_sector_map_v6.0
#'
#' @source github
#' @format .csv
#' @description emissions maps compatible with GCAM6.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::nonco2_emis_sector_map_v6.0
#' }
"nonco2_emis_sector_map_v6.0"

#' nonco2_emis_resource_map_v6.0
#'
#' @source github
#' @format .csv
#' @description emissions maps compatible with GCAM6.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::nonco2_emis_resource_map_v6.0
#' }
"nonco2_emis_resource_map_v6.0"

#' carbon_seq_tech_map_v6.0
#'
#' @source github
#' @format .csv
#' @description emissions maps compatible with GCAM6.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::carbon_seq_tech_map_v6.0
#' }
"carbon_seq_tech_map_v6.0"

#' ag_demand_map_v6.0
#'
#' @source github
#' @format .csv
#' @description ag maps compatible with GCAM6.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::ag_demand_map_v6.0
#' }
"ag_demand_map_v6.0"

#' ag_prices_map_v6.0
#'
#' @source github
#' @format .csv
#' @description ag maps compatible with GCAM6.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::ag_prices_map_v6.0
#' }
"ag_prices_map_v6.0"

#' land_use_map_v6.0
#'
#' @source github
#' @format .csv
#' @description ag maps compatible with GCAM6.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::land_use_map_v6.0
#' }
"land_use_map_v6.0"

#' primary_energy_map_v6.0
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM6.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::primary_energy_map_v6.0
#' }
"primary_energy_map_v6.0"

#' production_map_v6.0
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM6.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::production_map_v6.0
#' }
"production_map_v6.0"

#' elec_gen_map_v6.0
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM6.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::elec_gen_map_v6.0
#' }
"elec_gen_map_v6.0"

#' capacity_map_v6.0
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM6.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::capacity_map_v6.0
#' }
"capacity_map_v6.0"

#' cf_gcam_v6.0
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM6.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::cf_gcam_v6.0
#' }
"cf_gcam_v6.0"

#' cf_rgn_v6.0
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM6.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::cf_rgn_v6.0
#' }
"cf_rgn_v6.0"

#' se_gen_map_v6.0
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM6.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::se_gen_map_v6.0
#' }
"se_gen_map_v6.0"

#' final_energy_map_v6.0
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM6.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::final_energy_map_v6.0
#' }
"final_energy_map_v6.0"

#' transport_final_en_map_v6.0
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM6.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::transport_final_en_map_v6.0
#' }
"transport_final_en_map_v6.0"

#' energy_prices_map_v6.0
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps compatible with GCAM6.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::energy_prices_map_v6.0
#' }
"energy_prices_map_v6.0"

#' transport_en_service_v6.0
#'
#' @source github
#' @format .csv
#' @description transport energy services compatible with GCAM6.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::transport_en_service_v6.0
#' }
"transport_en_service_v6.0"

#' buildings_en_service_v6.0
#'
#' @source github
#' @format .csv
#' @description buildings energy services compatible with GCAM6.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::buildings_en_service_v6.0
#' }
"buildings_en_service_v6.0"

#' capital_gcam_v6.0
#'
#' @source github
#' @format .csv
#' @description capital update compatible with GCAM6.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::capital_gcam_v6.0
#' }
"capital_gcam_v6.0"

#' investment_v6.0
#'
#' @source github
#' @format .csv
#' @description capital investment compatible with GCAM6.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::investment_v6.0
#' }
"investment_v6.0"

#' carbon_content_v6.0
#'
#' @source github
#' @format .csv
#' @description carbon content compatible with GCAM6.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::carbon_content_v6.0
#' }
"carbon_content_v6.0"

#' nonco2_content_v6.0
#'
#' @source github
#' @format .csv
#' @description non CO2 content compatible with GCAM6.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::nonco2_content_v6.0
#' }
"nonco2_content_v6.0"

#' iea_capacity_v6.0
#'
#' @source github
#' @format .csv
#' @description iea 2019 capacity compatible with GCAM6.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::iea_capacity_v6.0
#' }
"iea_capacity_v6.0"

#' co2_market_v6.0
#'
#' @source github
#' @format .csv
#' @description new CO2 market compatible with GCAM6.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::co2_market_v6.0
#' }
"co2_market_v6.0"

#' co2_market_frag_map_v6.0
#'
#' @source github
#' @format .csv
#' @description new CO2 regional markets map compatible with GCAM6.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::co2_market_frag_map_v6.0
#' }
"co2_market_frag_map_v6.0"

#' iron_steel_trade_map_v6.0
#'
#' @source github
#' @format .csv
#' @description iron steel imports and exports map
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::iron_steel_trade_map_v6.0
#' }
"iron_steel_trade_map_v6.0"

#' convert_v6.0
#'
#' @description units conversion list
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::convert_v6.0
#' }
"convert_v6.0"

#' F_GASES_v6.0
#'
#' @description ghg emission conversion.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::F_GASES_v6.0
#' }
"F_GASES_v6.0"

#' GHG_gases_v6.0
#'
#' @description ghg emission conversion.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::GHG_gases_v6.0
#' }
"GHG_gases_v6.0"

#' last_historical_year_v6.0
#'
#' @description last historical year compatible with GCAM6.0.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::last_historical_year_v6.0
#' }
"last_historical_year_v6.0"

#' reg_cont_v6.0
#'
#' @source local
#' @format .csv
#' @description mapping between regions and continents compatible with GCAM6.0
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::reg_cont_v6.0
#' }
"reg_cont_v6.0"

#' queries_general
#'
#' @source local
#' @format vector
#' @description gcamreport queries compatible with GCAM 7.0 version.
#' Contain all queries except for nonCO2 queries
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::queries_general
#' }
"queries_general"

#' queries_nonCO2
#'
#' @source local
#' @format vector
#' @description gcamreport nonCO2 query compatible with GCAM 7.0 version.
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::queries_nonCO2
#' }
"queries_nonCO2"


#' emissions_list
#'
#' @source local
#' @format vector
#' @description nonCO2 pollutants list
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::emissions_list
#' }
"emissions_list"

#' GWP_adjuster
#'
#' @source local
#' @format .csv
#' @description ghg adjuster
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::GWP_adjuster
#' }
"GWP_adjuster"

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

#' reg_cont
#'
#' @source local
#' @format .csv
#' @description mapping between regions and continents
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::reg_cont
#' }
"reg_cont"

#' var_fun_map
#'
#' @source local
#' @format .csv
#' @description mapping between variables, functions to load them, dependent variables, and available verifications
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::var_fun_map
#' }
"var_fun_map"

#' template
#'
#' @source github
#' @format .csv
#' @description read in template
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::template
#' }
"template"

#' co2_sector_map
#'
#' @source github
#' @format .csv
#' @description emissions maps
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::co2_sector_map
#' }
"co2_sector_map"

#' co2_tech_map
#'
#' @source github
#' @format .csv
#' @description emissions maps
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::co2_tech_map
#' }
"co2_tech_map"

#' kyoto_sector_map
#'
#' @source github
#' @format .csv
#' @description emissions maps
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::kyoto_sector_map
#' }
"kyoto_sector_map"

#' nonco2_emis_sector_map
#'
#' @source github
#' @format .csv
#' @description emissions maps
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::nonco2_emis_sector_map
#' }
"nonco2_emis_sector_map"

#' nonco2_emis_resource_map
#'
#' @source github
#' @format .csv
#' @description emissions maps
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::nonco2_emis_resource_map
#' }
"nonco2_emis_resource_map"

#' carbon_seq_tech_map
#'
#' @source github
#' @format .csv
#' @description emissions maps
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::carbon_seq_tech_map
#' }
"carbon_seq_tech_map"

#' ag_demand_map
#'
#' @source github
#' @format .csv
#' @description ag maps
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::ag_demand_map
#' }
"ag_demand_map"

#' ag_prices_map
#'
#' @source github
#' @format .csv
#' @description ag maps
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::ag_prices_map
#' }
"ag_prices_map"

#' land_use_map
#'
#' @source github
#' @format .csv
#' @description ag maps
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::land_use_map
#' }
"land_use_map"

#' primary_energy_map
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::primary_energy_map
#' }
"primary_energy_map"

#' production_map
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::production_map
#' }
"production_map"

#' elec_gen_map
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::elec_gen_map
#' }
"elec_gen_map"

#' capacity_map
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::capacity_map
#' }
"capacity_map"

#' cf_gcam
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::cf_gcam
#' }
"cf_gcam"

#' cf_rgn
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::cf_rgn
#' }
"cf_rgn"

#' se_gen_map
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::se_gen_map
#' }
"se_gen_map"

#' final_energy_map
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::final_energy_map
#' }
"final_energy_map"

#' transport_final_en_map
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::transport_final_en_map
#' }
"transport_final_en_map"

#' energy_prices_map
#'
#' @source github
#' @format .csv
#' @description primary, secondary, final energy maps
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::energy_prices_map
#' }
"energy_prices_map"

#' transport_en_service
#'
#' @source github
#' @format .csv
#' @description energy services
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::transport_en_service
#' }
"transport_en_service"

#' buildings_en_service
#'
#' @source github
#' @format .csv
#' @description energy services
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::buildings_en_service
#' }
"buildings_en_service"

#' capital_gcam
#'
#' @source github
#' @format .csv
#' @description capital update
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::capital_gcam
#' }
"capital_gcam"

#' investment
#'
#' @source github
#' @format .csv
#' @description capital investment
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::investment
#' }
"investment"

#' carbon_content
#'
#' @source github
#' @format .csv
#' @description carbon content
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::carbon_content
#' }
"carbon_content"

#' nonCO2_content
#'
#' @source github
#' @format .csv
#' @description non CO2 content
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::nonCO2_content
#' }
"nonCO2_content"

#' iea_capacity
#'
#' @source github
#' @format .csv
#' @description iea 2019 capacity
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::iea_capacity
#' }
"iea_capacity"

#' CO2_market
#'
#' @source github
#' @format .csv
#' @description new CO2 market
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::CO2_market
#' }
"CO2_market"

#' co2_market_frag_map
#'
#' @source github
#' @format .csv
#' @description new CO2 market fragmented map
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::co2_market_frag_map
#' }
"co2_market_frag_map"

#' iron_steel_trade_map
#'
#' @source github
#' @format .csv
#' @description iron steel imports and exports map
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::iron_steel_trade_map
#' }
"iron_steel_trade_map"

#' convert
#'
#' @description units conversion list
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::convert
#' }
"convert"

#' F_GASES
#'
#' @description ghg emission conversion
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::F_GASES
#' }
"F_GASES"

#' GHG_gases
#'
#' @description ghg emission conversion
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::GHG_gases
#' }
"GHG_gases"

#' CO2_equivalent
#'
#' @description CO2 equivalency conversion
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::CO2_equivalent
#' }
"CO2_equivalent"

#' GCAM_years
#'
#' @description reporting years
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::GCAM_years
#' }
"GCAM_years"

#' reporting_years
#'
#' @description reporting years
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::reporting_years
#' }
"reporting_years"

#' last_historical_year
#'
#' @description last historical years
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::last_historical_year
#' }
"last_historical_year"

#' long_columns
#'
#' @description reporting columns
#' @examples
#' \dontrun{
#' library(gcamreport)
#' gcamreport::long_columns
#' }
"long_columns"

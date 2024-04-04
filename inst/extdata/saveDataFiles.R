# Converting raw data into package data
library(usethis)
library(magrittr)

### paths
rawDataFolder <- here::here()

# emissions considered nonCO2
emissions_list <- c(
  "BC", "BC_AWB", "C2F6", "CF4", "CH4", "CH4_AGR", "CH4_AWB", "CO", "CO2", "CO_AWB", "CO2_ETS",
  "H2", "H2_AWB", "HFC125", "HFC134a", "HFC143a", "HFC152a", "HFC227ea", "HFC23", "HFC236fa",
  "HFC245fa", "HFC32", "HFC365mfc", "HFC43", "N2O", "N2O_AGR", "N2O_AWB", "NH3", "NH3_AGR",
  "NH3_AWB", "NMVOC", "NMVOC_AGR", "NMVOC_AWB", "NOx", "NOx_AGR", "NOx_AWB", "OC", "OC_AWB",
  "PM10", "PM2.5", "SF6", "SO2_1", "SO2_1_AWB", "SO2_2", "SO2_2_AWB", "SO2_3", "SO2_3_AWB",
  "SO2_4", "SO2_4_AWB"
)
use_data(emissions_list, overwrite = T)

# regions_continents_map
reg_cont <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "regions_continents_map.csv"),
  skip = 1,
  stringsAsFactors = FALSE
)
use_data(reg_cont, overwrite = T)

# variables_functions_mapping
var_fun_map <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "variables_functions_mapping.csv"),
  sep = ";", header = T, na.strings = c("", "NA"), stringsAsFactors = FALSE
)

var_fun_map$dependencies <- as.list(strsplit(var_fun_map$dependencies, ","))
var_fun_map$checks <- as.list(strsplit(var_fun_map$checks, ","))
var_fun_map$queries <- as.list(strsplit(var_fun_map$queries, ","))
use_data(var_fun_map, overwrite = T)

# ghg adjuster
GWP_adjuster <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "ghg_GWP.csv"),
  skip = 1, na = "",
  stringsAsFactors = FALSE
)
use_data(GWP_adjuster, overwrite = T)

# vetting test
global_vet_values <- read.csv(file.path(rawDataFolder, "inst/extdata/vetting", "global_vet_values.csv"),
  stringsAsFactors = FALSE
)
use_data(global_vet_values, overwrite = T)

# Read in template
template <- read.csv(file.path(rawDataFolder, "inst/extdata", "template/reporting_template.csv"),
  fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE
)
decode_html <- function(text) {
  xml2::xml_text(xml2::read_xml(paste0("<x>", text, "</x>")))
}
# Applying the function to decode HTML entities in col1
template$Unit <- sapply(template$Unit, decode_html)
use_data(template, overwrite = T)

# emissions maps
co2_sector_map <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "CO2_sector_map.csv"),
  skip = 1, na = "",
  stringsAsFactors = FALSE
) %>% gather_map()
use_data(co2_sector_map, overwrite = T)

co2_ets_sector_map <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "CO2_ETS_sector_map.csv"),
  skip = 1, na = "",
  stringsAsFactors = FALSE
) %>% gather_map()
use_data(co2_ets_sector_map, overwrite = T)

co2_tech_map <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "CO2_tech_map.csv"),
  skip = 1, na = "",
  stringsAsFactors = FALSE
) %>% gather_map()
use_data(co2_tech_map, overwrite = T)

kyoto_sector_map <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "Kyotogas_sector.csv"),
  skip = 1, na = "",
  stringsAsFactors = FALSE
) %>% gather_map()
use_data(kyoto_sector_map, overwrite = T)

nonco2_emis_sector_map <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "nonCO2_emissions_sector_map.csv"),
  skip = 1, na = "", stringsAsFactors = FALSE
) %>% gather_map()
use_data(nonco2_emis_sector_map, overwrite = T)

nonco2_emis_resource_map <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "nonCO2_emissions_resource_map.csv"),
  skip = 1, na = "", stringsAsFactors = FALSE
) %>% gather_map()
use_data(nonco2_emis_resource_map, overwrite = T)

carbon_seq_tech_map <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "carbon_seq_tech_map.csv"),
  skip = 1, na = "",
  stringsAsFactors = FALSE
) %>% gather_map()
use_data(carbon_seq_tech_map, overwrite = T)


# ag maps
ag_demand_map <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "ag_demand_map.csv"),
  skip = 1,
  stringsAsFactors = FALSE
) %>% gather_map()
use_data(ag_demand_map, overwrite = T)

ag_prices_map <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "ag_prices_map.csv"),
  skip = 1,
  stringsAsFactors = FALSE
) %>% gather_map()
use_data(ag_prices_map, overwrite = T)

land_use_map <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "land_use_map.csv"),
  skip = 1,
  stringsAsFactors = FALSE
) %>% gather_map()
use_data(land_use_map, overwrite = T)


# primary, secondary, final energy maps
primary_energy_map <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "en_primary_map.csv"),
  skip = 1,
  stringsAsFactors = FALSE
) %>% gather_map()
use_data(primary_energy_map, overwrite = T)

production_map <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "production_map.csv"),
  skip = 1,
  stringsAsFactors = FALSE
) %>% gather_map()
use_data(production_map, overwrite = T)

elec_gen_map <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "elec_gen_map_core.csv"),
  skip = 1,
  stringsAsFactors = FALSE
) %>%
  dplyr::filter(!grepl("cogen", technology)) %>%
  gather_map()
use_data(elec_gen_map, overwrite = T)

capacity_map <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "capacity_map.csv"),
  skip = 1,
  stringsAsFactors = FALSE
) %>%
  dplyr::filter(!grepl("cogen", technology)) %>%
  gather_map()
use_data(capacity_map, overwrite = T)

cf_gcam <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "A23.globaltech_capacity_factor.csv"),
  skip = 9, na = "",
  stringsAsFactors = FALSE
)
use_data(cf_gcam, overwrite = T)

cf_rgn <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "L223.StubTechCapFactor_elec.csv"),
  skip = 1, na = "",
  stringsAsFactors = FALSE
)
use_data(cf_rgn, overwrite = T)

se_gen_map <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "secondary_energy_gen_map.csv"),
  skip = 1,
  stringsAsFactors = FALSE
) %>% gather_map()
use_data(se_gen_map, overwrite = T)

final_energy_map <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "final_energy_map.csv"),
  skip = 1,
  stringsAsFactors = FALSE
) %>% gather_map()
use_data(final_energy_map, overwrite = T)

transport_final_en_map <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "transport_final_en_map.csv"),
  skip = 1, na = "",
  stringsAsFactors = FALSE
) %>% gather_map()
use_data(transport_final_en_map, overwrite = T)

energy_prices_map <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "energy_prices_map.csv"),
  skip = 1, na = "",
  stringsAsFactors = FALSE
) %>% gather_map()
use_data(energy_prices_map, overwrite = T)


# Energy Service maps
transport_en_service <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "energy_service_transportation.csv"),
  skip = 1,
  stringsAsFactors = FALSE
) %>% gather_map()
use_data(transport_en_service, overwrite = T)

buildings_en_service <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "energy_service_buildings.csv"),
  skip = 1,
  stringsAsFactors = FALSE
) %>% gather_map()
use_data(buildings_en_service, overwrite = T)


# capital updates
capital_gcam <- read.csv(paste0(rawDataFolder, "/inst/extdata/mappings", "/L223.StubTechCapital.csv"),
  skip = 2, na = "",
  fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE
) %>%
  dplyr::select(region, sector = supplysector, subsector = subsector, technology = stub.technology, year, capital.overnight)
use_data(capital_gcam, overwrite = T)

investment <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "investment.csv"),
  na = "",
  fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE
) %>%
  tidyr::gather(year, value, X2015:X2100) %>%
  dplyr::mutate(year = as.integer(sub("X", "", year))) %>%
  dplyr::mutate(value = gsub("%", "", value)) %>%
  dplyr::mutate(value = as.numeric(value))
use_data(investment, overwrite = T)


carbon_content <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "L202.CarbonCoef.csv"),
  skip = 2, na = "",
  stringsAsFactors = FALSE
)
use_data(carbon_content, overwrite = T)

nonCO2_content <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "L201.ghg_res.csv"),
  skip = 2, na = "",
  stringsAsFactors = FALSE
)
use_data(nonCO2_content, overwrite = T)

iea_capacity <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "IEAWEO2019_Capacity.csv"), stringsAsFactors = FALSE)
use_data(iea_capacity, overwrite = T)

CO2_market <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "CO2market_new.csv"), skip = 1, stringsAsFactors = FALSE)
use_data(CO2_market, overwrite = T)

co2_market_frag_map <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "CO2market_frag_map.csv"),
  skip = 1,
  stringsAsFactors = FALSE
)
use_data(co2_market_frag_map, overwrite = T)

# List of Constants

convert <- list(
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
use_data(convert, overwrite = T)

# GHG emission conversion
F_GASES <- c(
  "C2F6", "CF4", "HFC125", "HFC134a", "HFC245fa", "SF6", "HFC143a",
  "HFC152a", "HFC227ea", "HFC23", "HFC236fa", "HFC32", "HFC365mfc",
  "HFC43", "HFC245fa", "HFC43-10"
)
use_data(F_GASES, overwrite = T)

GHG_gases <- c("CH4", "N2O", F_GASES, "CO2", "CO2LUC")
use_data(GHG_gases, overwrite = T)


# Reporting years
GCAM_years <- c(1990, seq(2005, 2100, 5))
use_data(GCAM_years, overwrite = T)

reporting_years <- seq(2005, 2100, 5)
use_data(reporting_years, overwrite = T)

last_historical_year <- 2015
use_data(last_historical_year, overwrite = T)


# REPLACE with reporting years

# Reporting columns
long_columns <- c("scenario", "region", "var", "year", "value")
use_data(long_columns, overwrite = T)


# QUERY files

# gcamreport7 queries complete
queryFile <- file.path(rawDataFolder, "inst/extdata/queries/queries_gcamreport_gcam7.0_general.xml")
queries_general <- parse_batch_query(queryFile)
use_data(queries_general, overwrite = T)

# gcamreport7 queries nonCO2
queryFile <- file.path(rawDataFolder, "inst/extdata/queries/queries_gcamreport_gcam7.0_nonCO2.xml")
queries_nonCO2 <- parse_batch_query(queryFile)
use_data(queries_nonCO2, overwrite = T)


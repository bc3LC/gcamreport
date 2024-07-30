# Converting raw data into package data
library(usethis)
library(magrittr)

### paths
rawDataFolder <- here::here()

# regions_continents_map
reg_cont_v7.0 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.0", "regions_continents_map.csv"),
                     skip = 1,
                     stringsAsFactors = FALSE
)
use_data(reg_cont_v7.0, overwrite = T)

# variables_functions_mapping
var_fun_map_v7.0 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.0", "variables_functions_mapping.csv"),
                        sep = ";", header = T, na.strings = c("", "NA"), stringsAsFactors = FALSE
)

var_fun_map_v7.0$dependencies <- as.list(strsplit(var_fun_map_v7.0$dependencies, ","))
var_fun_map_v7.0$checks <- as.list(strsplit(var_fun_map_v7.0$checks, ","))
var_fun_map_v7.0$queries <- as.list(strsplit(var_fun_map_v7.0$queries, ","))
use_data(var_fun_map_v7.0, overwrite = T)

# ghg adjuster
GWP_adjuster_v7.0 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.0", "ghg_GWP.csv"),
                         skip = 1, na = "",
                         stringsAsFactors = FALSE
)
use_data(GWP_adjuster_v7.0, overwrite = T)


# Read in template
template_v7.0 <- read.csv(file.path(rawDataFolder, "inst/extdata/template/GCAM7.0", "reporting_template.csv"),
                     fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE
)
decode_html <- function(text) {
  xml2::xml_text(xml2::read_xml(paste0("<x>", text, "</x>")))
}
# Applying the function to decode HTML entities in col1
template_v7.0$Unit <- sapply(template_v7.0$Unit, decode_html)
use_data(template_v7.0, overwrite = T)

# emissions maps
co2_sector_map_v7.0 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.0", "CO2_sector_map.csv"),
                           skip = 1, na = "",
                           stringsAsFactors = FALSE
) %>% gather_map()
use_data(co2_sector_map_v7.0, overwrite = T)

co2_ets_sector_map_v7.0 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.0", "CO2_ETS_sector_map.csv"),
                               skip = 1, na = "",
                               stringsAsFactors = FALSE
) %>% gather_map()
use_data(co2_ets_sector_map_v7.0, overwrite = T)

co2_tech_map_v7.0 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.0", "CO2_tech_map.csv"),
                         skip = 1, na = "",
                         stringsAsFactors = FALSE
) %>% gather_map()
use_data(co2_tech_map_v7.0, overwrite = T)

kyoto_sector_map_v7.0 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.0", "Kyotogas_sector.csv"),
                             skip = 1, na = "",
                             stringsAsFactors = FALSE
) %>% gather_map()
use_data(kyoto_sector_map_v7.0, overwrite = T)

nonco2_emis_sector_map_v7.0 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.0", "nonCO2_emissions_sector_map.csv"),
                                   skip = 1, na = "", stringsAsFactors = FALSE
) %>% gather_map()
use_data(nonco2_emis_sector_map_v7.0, overwrite = T)

nonco2_emis_resource_map_v7.0 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.0", "nonCO2_emissions_resource_map.csv"),
                                     skip = 1, na = "", stringsAsFactors = FALSE
) %>% gather_map()
use_data(nonco2_emis_resource_map_v7.0, overwrite = T)

carbon_seq_tech_map_v7.0 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.0", "carbon_seq_tech_map.csv"),
                                skip = 1, na = "",
                                stringsAsFactors = FALSE
) %>% gather_map()
use_data(carbon_seq_tech_map_v7.0, overwrite = T)


# ag maps
ag_demand_map_v7.0 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.0", "ag_demand_map.csv"),
                          skip = 1,
                          stringsAsFactors = FALSE
) %>% gather_map()
use_data(ag_demand_map_v7.0, overwrite = T)

ag_prices_map_v7.0 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.0", "ag_prices_map.csv"),
                          skip = 1,
                          stringsAsFactors = FALSE
) %>% gather_map()
use_data(ag_prices_map_v7.0, overwrite = T)

land_use_map_v7.0 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.0", "land_use_map.csv"),
                         skip = 1,
                         stringsAsFactors = FALSE
) %>% gather_map()
use_data(land_use_map_v7.0, overwrite = T)


# primary, secondary, final energy maps
primary_energy_map_v7.0 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.0", "en_primary_map.csv"),
                               skip = 1,
                               stringsAsFactors = FALSE
) %>% gather_map()
use_data(primary_energy_map_v7.0, overwrite = T)

production_map_v7.0 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.0", "production_map.csv"),
                           skip = 1,
                           stringsAsFactors = FALSE
) %>% gather_map()
use_data(production_map_v7.0, overwrite = T)

elec_gen_map_v7.0 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.0", "elec_gen_map_core.csv"),
                         skip = 1,
                         stringsAsFactors = FALSE
) %>%
  dplyr::filter(!grepl("cogen", technology)) %>%
  gather_map()
use_data(elec_gen_map_v7.0, overwrite = T)

capacity_map_v7.0 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.0", "capacity_map.csv"),
                         skip = 1,
                         stringsAsFactors = FALSE
) %>%
  dplyr::filter(!grepl("cogen", technology)) %>%
  gather_map()
use_data(capacity_map_v7.0, overwrite = T)

cf_gcam_v7.0 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.0", "A23.globaltech_capacity_factor.csv"),
                    skip = 9, na = "",
                    stringsAsFactors = FALSE
)
use_data(cf_gcam_v7.0, overwrite = T)

cf_rgn_v7.0 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.0", "L223.StubTechCapFactor_elec.csv"),
                   skip = 1, na = "",
                   stringsAsFactors = FALSE
)
use_data(cf_rgn_v7.0, overwrite = T)

se_gen_map_v7.0 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.0", "secondary_energy_gen_map.csv"),
                       skip = 1,
                       stringsAsFactors = FALSE
) %>% gather_map()
use_data(se_gen_map_v7.0, overwrite = T)

final_energy_map_v7.0 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.0", "final_energy_map.csv"),
                             skip = 1,
                             stringsAsFactors = FALSE
) %>% gather_map()
use_data(final_energy_map_v7.0, overwrite = T)

transport_final_en_map_v7.0 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.0", "transport_final_en_map.csv"),
                                   skip = 1, na = "",
                                   stringsAsFactors = FALSE
) %>% gather_map()
use_data(transport_final_en_map_v7.0, overwrite = T)

energy_prices_map_v7.0 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.0", "energy_prices_map.csv"),
                              skip = 1, na = "",
                              stringsAsFactors = FALSE
) %>% gather_map()
use_data(energy_prices_map_v7.0, overwrite = T)


# Energy Service maps
transport_en_service_v7.0 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.0", "energy_service_transportation.csv"),
                                 skip = 1,
                                 stringsAsFactors = FALSE
) %>% gather_map()
use_data(transport_en_service_v7.0, overwrite = T)

buildings_en_service_v7.0 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.0", "energy_service_buildings.csv"),
                                 skip = 1,
                                 stringsAsFactors = FALSE
) %>% gather_map()
use_data(buildings_en_service_v7.0, overwrite = T)


# capital updates
capital_gcam_v7.0 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.0", "L223.GlobalIntTechCapital_elec.csv"),
                         skip = 2, na = "",
                         fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE
) %>%
  dplyr::rename(technology = intermittent.technology) %>%
  dplyr::bind_rows(read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.0", "L223.GlobalTechCapital_elec.csv"),
                            skip = 2, na = "",
                            stringsAsFactors = FALSE
  )) %>%
  dplyr::select(sector = sector.name, subsector = subsector.name, technology, year, capital.overnight)
use_data(capital_gcam_v7.0, overwrite = T)

investment_v7.0 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.0", "investment.csv"),
                       na = "",
                       fileEncoding = "UTF-8-BOM", stringsAsFactors = FALSE
) %>%
  tidyr::gather(year, value, X2015:X2100) %>%
  dplyr::mutate(year = as.integer(sub("X", "", year))) %>%
  dplyr::mutate(value = gsub("%", "", value)) %>%
  dplyr::mutate(value = as.numeric(value))
use_data(investment_v7.0, overwrite = T)


carbon_content_v7.0 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.0", "L202.CarbonCoef.csv"),
                           skip = 2, na = "",
                           stringsAsFactors = FALSE
)
use_data(carbon_content_v7.0, overwrite = T)

nonCO2_content_v7.0 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.0", "L201.ghg_res.csv"),
                           skip = 2, na = "",
                           stringsAsFactors = FALSE
)
use_data(nonCO2_content_v7.0, overwrite = T)

iea_capacity_v7.0 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.0", "IEAWEO2019_Capacity.csv"), stringsAsFactors = FALSE)
use_data(iea_capacity_v7.0, overwrite = T)

CO2_market_v7.0 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.0", "CO2market_new.csv"), skip = 1, stringsAsFactors = FALSE)
use_data(CO2_market_v7.0, overwrite = T)

co2_market_frag_map_v7.0 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/GCAM7.0", "CO2market_frag_map.csv"),
                                skip = 1,
                                stringsAsFactors = FALSE
)
use_data(co2_market_frag_map_v7.0, overwrite = T)


# Reporting years
GCAM_years_v7.0 <- c(1990, seq(2005, 2100, 5))
use_data(GCAM_years_v7.0, overwrite = T)

reporting_years_v7.0 <- seq(2005, 2100, 5)
use_data(reporting_years_v7.0, overwrite = T)

last_historical_year_v7.0 <- 2015
use_data(last_historical_year_v7.0, overwrite = T)




# QUERY files

# gcamreport7 queries complete
queryFile <- file.path(rawDataFolder, "inst/extdata/queries/GCAM7.0", "queries_gcamreport_general.xml")
queries_general_v7.0 <- parse_batch_query(queryFile)
use_data(queries_general_v7.0, overwrite = T)

# gcamreport7 queries nonCO2
queryFile <- file.path(rawDataFolder, "inst/extdata/queries/GCAM7.0", "queries_gcamreport_nonCO2.xml")
queries_nonCO2_v7.0 <- parse_batch_query(queryFile)
use_data(queries_nonCO2_v7.0, overwrite = T)


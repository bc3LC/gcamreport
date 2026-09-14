# Converting raw data into package data
library(usethis)
library(magrittr)

### paths
rawDataFolder <- here::here()

# GDP PPP OECD per capita average
# Source: https://www.theglobaleconomy.com/rankings/gdp_per_capita_ppp/OECD/
GDP_PPP_OECD_pc_av <- 55.142 # US dollar 2024
use_data(GDP_PPP_OECD_pc_av, overwrite = T)

# Reference scenario names
# List all possible default names for the Reference scenario
scen_ref_patterns <- c('Reference','Baseline','Ref','Base')
use_data(scen_ref_patterns, overwrite = T)

# ghg adjusters
ghg_GWP_AR4 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "ghg_GWP_AR4.csv"),
                        skip = 1, na = "",
                        stringsAsFactors = FALSE
)
use_data(ghg_GWP_AR4, overwrite = T)

ghg_GWP_AR5 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "ghg_GWP_AR5.csv"),
                        skip = 1, na = "",
                        stringsAsFactors = FALSE
)
use_data(ghg_GWP_AR5, overwrite = T)

ghg_GWP_AR6 <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings", "ghg_GWP_AR6.csv"),
                        skip = 1, na = "",
                        stringsAsFactors = FALSE
)
use_data(ghg_GWP_AR6, overwrite = T)



# Reporting columns
long_columns <- c("scenario", "region", "var", "year", "value")
use_data(long_columns, overwrite = T)

# GCAM versions with deciles
deciles_GCAM_versions <- c('v7.1', 'v7.2', 'v8.2', 'vScenarioMIPCMIP7', 'vEurope7.2', 'vEurope8.7')
use_data(deciles_GCAM_versions, overwrite = T)

# Available GCAM versions
available_GCAM_versions <- c('v7.0', 'v7.1', 'v7.2', 'v8.2', 'vScenarioMIPCMIP7', 'vEurope7.2', 'vEurope8.7')
use_data(available_GCAM_versions, overwrite = T)

# Available GWP versions
available_GWP_versions <- c('AR4', 'AR5', 'AR6')
use_data(available_GWP_versions, overwrite = T)

# Number of GCAM regions
GCAM_regions_number <- 32
use_data(GCAM_regions_number, overwrite = T)


# vetting test
global_vet_values <- read.csv(file.path(rawDataFolder, "inst/extdata/vetting", "global_vet_values.csv"),
                              stringsAsFactors = FALSE
)
use_data(global_vet_values, overwrite = T)


# energy blocks
en_blocks <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/common", "en_blocks.csv"),
                      skip = 1,
                      stringsAsFactors = FALSE,
                      fileEncoding = "UTF-8-BOM"
)
use_data(en_blocks, overwrite = T)

# water content in agricultural items
water_content <- read.csv(file.path(rawDataFolder, "inst/extdata/mappings/common", "ag_water_content.csv"),
                          skip = 1,
                          stringsAsFactors = FALSE,
                          fileEncoding = "UTF-8-BOM"
) %>%
  dplyr::select(GCAM_commodity, mean_water_content = mean)
use_data(water_content, overwrite = T)

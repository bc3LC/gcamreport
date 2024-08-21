# Converting raw data into package data
library(usethis)
library(magrittr)

### paths
rawDataFolder <- here::here()


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

# Available GCAM versions
available_GCAM_versions <- c('v6.0', 'v7.0', 'v7.1')
use_data(available_GCAM_versions, overwrite = T)

# Available GWP versions
available_GWP_versions <- c('AR4', 'AR5', 'AR6')
use_data(available_GWP_versions, overwrite = T)

# Number of GCAM regions
GCAM_regions_number <- 32
use_data(GCAM_regions_number, overwrite = T)

# gcamreport available final year
available_final_year <- seq(2025, 2100, by = 5)
use_data(available_final_year, overwrite = T)

# gcamreport available reporting years
available_reporting_years <- seq(2005, 2100, by = 5)
use_data(available_reporting_years, overwrite = T)

# gcam model years
gcam_years <- c(1990, seq(2005, 2100, by = 5))
use_data(gcam_years, overwrite = T)


# vetting test
global_vet_values <- read.csv(file.path(rawDataFolder, "inst/extdata/vetting", "global_vet_values.csv"),
                              stringsAsFactors = FALSE
)
use_data(global_vet_values, overwrite = T)


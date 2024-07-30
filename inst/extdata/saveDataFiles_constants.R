# Converting raw data into package data
library(usethis)
library(magrittr)

### paths
rawDataFolder <- here::here()


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


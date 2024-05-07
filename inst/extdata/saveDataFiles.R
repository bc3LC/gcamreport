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


# vetting test
global_vet_values <- read.csv(file.path(rawDataFolder, "inst/extdata/vetting", "global_vet_values.csv"),
  stringsAsFactors = FALSE
)
use_data(global_vet_values, overwrite = T)



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


# Reporting columns
long_columns <- c("scenario", "region", "var", "year", "value")
use_data(long_columns, overwrite = T)


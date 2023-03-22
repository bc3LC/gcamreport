# This is the preparation script of GCAM IAMC reporting
# define functions
# Ryna Cui, April 2020

# function covert GHG to CO2e
conv_ghg_co2e <- function (data) {
  require(dplyr)
  require(tidyr)

  # GHG emission conversion
  F_GASES <- c("C2F6", "CF4", "HFC125", "HFC134a", "HFC245fa", "SF6", "HFC143a", "HFC152a", "HFC227ea", "HFC23", "HFC236fa", "HFC32", "HFC365mfc", "HFC43",
               "HFC245fa", "HFC43-10")
  GHG_gases <- c("CH4", "N2O", F_GASES, "CO2", "CO2LUC")

  GWP_adjuster <- read.csv(paste0(map_dir, "/ghg_GWP.csv"), skip = 1, na = "")

  data %>%
    separate(ghg, into = c("variable", "sector"), sep = "_", fill = "right") %>%
    filter(variable %in% GHG_gases) %>%
    left_join(GWP_adjuster, by = c("variable" = "GHG_gases")) %>%
    mutate(value = value * GWP, Units = "CO2e") %>%
    select(-GWP) %>%
    return()
}

# function convert capacity to generation
conv_EJ_TWh <- function (data, EJ){
  data %>%
    mutate(TWh = EJ / EJ_to_GWh / 1000)
}

conv_TWh_EJ <- function (data, TWh){
  data %>%
    mutate(EJ = TWh * EJ_to_GWh * 1000)
}

conv_GW_EJ <- function (data, cf, GW){
  # Elec related conversions
  hr_per_yr <- 8760
  EJ_to_GWh <- 0.0000036

  data %>%
    mutate(EJ = GW * (cf * hr_per_yr * EJ_to_GWh))
}

conv_EJ_GW <- function (data, cf, EJ){
  # Elec related conversions
  hr_per_yr <- 8760
  EJ_to_GWh <- 0.0000036

  data %>%
    mutate(gw = EJ / (cf * hr_per_yr * EJ_to_GWh))
}

# interpolation function
approx_fun <- function(year, value, rule = 1) {
  if(rule == 1 | rule == 2) {
    tryCatch(stats::approx(as.vector(year), value, rule = rule, xout = year)$y,
             error = function(e) NA)

  } else {
    stop("Use fill_exp_decay_extrapolate!")
  }
}

# function to make the first letter of every variable capitalized
firstup <- function(x) {
  x <- tolower(x)
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  x
}

# Create a simple function to format all maps into a long table
gather_map <- function(df){
  untouched_cols <- names(df) %>% .[!grepl("var", names(df))]
  df %>%
    gather(identifier, var, -untouched_cols) %>%
    select(-identifier) %>%
    filter(!is.na(var), var != "") %>%
    return()
}

# function to categorize technologies
tech.list <- c("other", "geothermal", "solar", "wind", "biomass w/ ccs", "biomass w/o ccs",
               "hydro", "nuclear",
               "gas w/ ccs", "gas w/o ccs", "natural gas_ccs", "natural gas",
               "oil w/ ccs", "oil w/o ccs", "coal w/ ccs","coal w/o ccs", "coal.total",
               "biomass 1st generation", "traditional biomass", "biomass traditional")
standard_tech_group <- function(data, technology) {
  data %>%
    mutate(tech = technology,
           tech = replace(tech, grepl("wind", technology), "wind"),
           tech = replace(tech, grepl("biomass", technology), "biomass"),
           tech = replace(tech, grepl("PV", technology), "solar"),
           tech = replace(tech, grepl("pv", technology), "solar"),
           tech = replace(tech, grepl("CSP", technology), "solar"),
           tech = replace(tech, grepl("geothermal", technology), "geothermal"),
           tech = replace(tech, grepl("hydro", technology), "hydro"),
           tech = replace(tech, grepl("Gen", technology), "nuclear"),
           tech = replace(tech, grepl("coal", technology), "coal"),
           tech = replace(tech, grepl("gas", technology), "gas"),
           tech = replace(tech, grepl("refined liquids", technology), "oil"),
           tech = replace(tech, grepl("oil", technology), "oil"),
           tech = replace(tech, grepl("CCS", technology), paste(tech[grepl("CCS", technology)], "ccs", sep = " w/ "))) %>%
    mutate(tech = factor(tech, levels = tech.list))
}

# color by tech
tech.color <- c( "biomass w/o ccs" = "darkgreen", "biomass w/ ccs" = "olivedrab",
                 "traditional biomass" = "olivedrab3", "biomass traditional" = "olivedrab3",
                 "biomass 1st generation" = "olivedrab1",
                 "solar" = "goldenrod1",
                 "wind" = "skyblue",
                 "geothermal" = "olivedrab1",
                 "hydro" = "mediumpurple4",
                 "coal w/o ccs" = "darkred", "coal w/ ccs" = "firebrick3", "coal.total" = "firebrick4",
                 "nuclear" = "darkorange1",
                 "gas w/o ccs" = "mediumorchid3", "gas w/ ccs" = "plum",
                 "natural gas" = "mediumorchid3", "natural gas_ccs" = "plum",
                 "oil w/o ccs" = "hotpink", "oil w/ ccs" = "lightpink1",
                 # "other" = "firebrick3")
                 "other" = "plum")

colScaleTech <- ggplot2::scale_colour_manual(name = "technology",
                                    values = tech.color,
                                    na.translate = FALSE,
                                    guide = ggplot2::guide_legend(reverse = F, ncol = 1))
fillScaleTech <- ggplot2::scale_fill_manual(name = "technology",
                                   values = tech.color,
                                   na.translate = FALSE,
                                   guide = ggplot2::guide_legend(reverse = F, ncol = 1))

# categorize fuel
fuel.list <- c("electricity","biomass", "biomass|modern", "biomass|traditional", "gas", "coal", "liquids", "hydrogen")

# color by fuel
fuel.color <- c( "biomass" = "darkgreen",
                 "biomass|modern" = "darkgreen",
                 "biomass|traditional" = "limegreen",
                 "electricity" = "goldenrod1",
                 "hydrogen" = "mediumpurple4",
                 "coal" = "firebrick4",
                 "gas" = "mediumorchid3",
                 "liquids" = "hotpink",
                 "heat" = "darkorange1")

colScaleFuel <- ggplot2::scale_colour_manual(name = "fuel",
                                    values = fuel.color,
                                    na.translate = FALSE,
                                    guide = ggplot2::guide_legend(reverse = F, ncol = 1))
fillScaleFuel <- ggplot2::scale_fill_manual(name = "fuel",
                                   values = fuel.color,
                                   na.translate = FALSE,
                                   guide = ggplot2::guide_legend(reverse = F, ncol = 1))

# categorize sector
sector.list <- c("Electricity", "Residential and commercial", "Other energy supply", "Refining",
                 "Transportation", "Industrial processes", "AFOLU CO2", "NonCO2")

# color by fuel
sector.color <- c(
  "Electricity" = "goldenrod1",
  "Refining" = "purple",
  "Industry" = "gray",
  "Other energy supply" = "navy",
  "Residential and commercial" = "deepskyblue2",
  "Transportation" = "darkgreen",
  "AFOLU CO2" = "lime green",
  "NonCO2" = "brown")

fillScaleSector <- ggplot2::scale_fill_manual(name = "Sector",
                                     values = sector.color,
                                     na.translate = FALSE,
                                     guide = ggplot2::guide_legend(reverse = F, ncol = 1))




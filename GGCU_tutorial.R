# # ----------------------------------------------------------
# # # SET UP THE ENVIRONMENT
# # ----------------------------------------------------------
# # clean the environment
# rm(list = ls())
#
# # load the pkg
# devtools::load_all()
#
#
# ----------------------------------------------------------
## EXAMPLE 1
# ----------------------------------------------------------
# # some error messages will appear. Let's deal with them!
# generate_report(prj_name = "tests/testthat/testInputs/v_8.2/gcamreport_onboard8p2_Ctax_260210.dat",
#                 desired_variables = 'Food Intake*',
#                 launch_ui = FALSE, GCAM_version = 'v8.2', save_output = FALSE,
#                 ignore = c('bio-ceiling','coal-elec-constraint',
#                            'CO2_NearTerm','wind_offshore-trial-supply',
#                            'CO2_LTG','globalCO2_LTG'))
#
#
# ----------------------------------------------------------
# # EXAMPLE 2
# ----------------------------------------------------------
# let's add Energy Service|Transportation variables
# -- Variable 1: Energy Service|Transportation|Passenger|Active Transport
# generate_report(prj_name = "tests/testthat/testInputs/v_8.2/gcamreport_onboard8p2_Ctax_260210.dat",
#                 desired_variables = 'Energy Service|Transportation|Passenger*',
#                 launch_ui = FALSE, GCAM_version = 'v8.2', save_output = FALSE,
#                 ignore = c('bio-ceiling','coal-elec-constraint',
#                            'CO2_NearTerm','wind_offshore-trial-supply',
#                            'CO2_LTG','globalCO2_LTG'))


# Energy Service|Transportation|Passenger|Active Transport,Energy service demand of walking and cycling in passenger transport,billion pkm/yr,,energy_service_transportation_clean,done,,,,,,,"[0, 100]",,,TRUE,7,,,,Energy Service|Transportation|Passenger|ActiveTransport|Share,,
# trn_pass,Cycle,Cycle,Energy Service|Transportation|Passenger,,Energy Service|Transportation|Passenger|Active Transport|Cycling [Share],Energy Service|Transportation|Passenger|Active Transport,Energy Service|Transportation|Passenger|Domestic,1
# trn_pass,Walk,Walk,Energy Service|Transportation|Passenger,,Energy Service|Transportation|Passenger|Active Transport|Walking [Share],Energy Service|Transportation|Passenger|Active Transport,Energy Service|Transportation|Passenger|Domestic,1


## -- Variable 2: Energy Service|Transportation|Passenger|Active Transport|Walking/Cycling [Share]
# energy_service_transportation_share <-
#   energy_service_transportation %>%
#   dplyr::filter((grepl("Passenger", var) & grepl("[Share]",var, fixed = TRUE)) |
#                   var == "Energy Service|Transportation|Passenger" |
#                   var == "Energy Service|Transportation|Passenger|Active Transport") %>%
#   dplyr::group_by(scenario, region, year) %>%
#   dplyr::mutate(
#     total_p = value[var == "Energy Service|Transportation|Passenger"],
#     total_active_p = value[var == "Energy Service|Transportation|Passenger|Active Transport"],
#     ratio_active_cycl_p = dplyr::if_else(var == "Energy Service|Transportation|Passenger|Active Transport|Cycling [Share]", 100 * value / total_active_p, NA),
#     ratio_active_walk_p = dplyr::if_else(var == "Energy Service|Transportation|Passenger|Active Transport|Walking [Share]", 100 * value / total_active_p, NA),
#     ratio_active_p = dplyr::if_else(var == "Energy Service|Transportation|Passenger|Active Transport [Share]", 100 * value / total_p, NA),
#     ratio_public_p = dplyr::if_else(var == "Energy Service|Transportation|Passenger|Public Transport [Share]", 100 * value / total_p, NA)
#   ) %>%
#   dplyr::ungroup() %>%
#   dplyr::mutate(value = dplyr::if_else(var == "Energy Service|Transportation|Passenger|Active Transport [Share]", ratio_active_p,
#                                        dplyr::if_else(var == "Energy Service|Transportation|Passenger|Public Transport [Share]", ratio_public_p,
#                                                       dplyr::if_else(var == "Energy Service|Transportation|Passenger|Active Transport|Cycling [Share]", ratio_active_cycl_p,
#                                                                      dplyr::if_else(var == "Energy Service|Transportation|Passenger|Active Transport|Walking [Share]", ratio_active_walk_p,
#                                                                                     value))))) %>%
#   dplyr::select(dplyr::all_of(gcamreport::long_columns))
#
#
# Energy Service|Transportation|Passenger|Active Transport|Walking [Share],Share of walking in passenger transport,%,,energy_service_transportation_clean,done,,,,,,,"[0, 100]",,,TRUE,7,,,,Energy Service|Transportation|Passenger|ActiveTransport|Share,,
# Energy Service|Transportation|Passenger|Active Transport|Cycling [Share],Share of cycling in passenger transport,%,,energy_service_transportation_clean,done,,,,,,,"[0, 100]",,,TRUE,7,,,,Energy Service|Transportation|Passenger|ActiveTransport|Share,,
# trn_pass,Cycle,Cycle,Energy Service|Transportation|Passenger,,Energy Service|Transportation|Passenger|Active Transport|Cycling [Share],Energy Service|Transportation|Passenger|Active Transport,Energy Service|Transportation|Passenger|Domestic,1
# trn_pass,Walk,Walk,Energy Service|Transportation|Passenger,,Energy Service|Transportation|Passenger|Active Transport|Walking [Share],Energy Service|Transportation|Passenger|Active Transport,Energy Service|Transportation|Passenger|Domestic,1




# # ----------------------------------------------------------
# ## EXAMPLE 3
# # ----------------------------------------------------------
# # let's do a regional-consistent gcamreport version
# queries - functions - mappings - new folder

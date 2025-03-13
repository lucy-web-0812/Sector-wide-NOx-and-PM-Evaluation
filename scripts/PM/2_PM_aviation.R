# Script to perform calculation for the ICAO - e.g. Aeroplane data 


current_aircraft_database <- read_csv("data/current_aircraft_database.csv") 


summary_values_planes <- current_aircraft_database |> 
  summarise(mean_fuel_LTO_cycle_kg = mean(`Fuel LTO Cycle (kg)`), 
            mean_NOx_LTO_total_mass_g = mean(`NOx LTO Total mass (g)`, na.rm = T),
            mean_NOx_EI_TO_g_kg = mean(`NOx EI T/O (g/kg)`), # This is the emission indicies in grams per kg of fuel for the .... take-off
            mean_NOx_EI_CO_g_kg = mean(`NOx EI C/O (g/kg)`), # ...Climb out
            mean_NOx_EI_APP_g_kg = mean(`NOx EI App (g/kg)`), # ...Approach
            mean_NOx_EI_Idle_g_kg = mean(`NOx EI Idle (g/kg)`), # .... and idle
            engine_count = n(),
            avg_rated_thrust_kN = mean(`Rated Thrust (kN)`), 
            standard_error_in_thrust = sd(`Rated Thrust (kN)`)/sqrt(n())) 



# Now performing a calculation, changing the thrust to a power

avg_cruise_speed <- 900 # This is in km/h

plane_power_approx_kw <- summary_values_planes$avg_rated_thrust_kN  * avg_cruise_speed / 3.6 # 3.6 factor is need to convert from km/h to ms-1


# Looking at the this for the total sector
# https://www.icao.int/Meetings/a41/Documents/WP/wp_093_rev_en.pdf 

total_nvpm_from_aviation_sector_tonnes <- 5000
total_fuel_consumption_from_aviation_sector_tonnes <- 188 * 1000000
kerosene_energy_content_kWh_kg <- 12


average_pm_g_kWh <- (total_nvpm_from_aviation_sector_tonnes /total_fuel_consumption_from_aviation_sector_tonnes)  /  kerosene_energy_content_kWh_kg  * 1000 


# And format it such that can be combined with other datasets to produce the mega plot

plane_data <- data.frame(plane_power_approx_kw, average_pm_g_kWh, summary_values_planes$standard_error_in_thrust)



collated_data_pm <- plane_data |> 
  mutate(Fuel = "Kerosene", Product = "Commercial Aircraft Turbofan Engine", Directive = "ICAO") |> 
  mutate(average_pm_mg_kWh = average_pm_g_kWh * 1000) |> 
  rename(`Power (kW)` = plane_power_approx_kw, `PM (mg/kWh)` = average_pm_mg_kWh ) |> 
  select(-c(average_pm_g_kWh, summary_values_planes.standard_error_in_thrust)) |> 
  rbind(collated_data_pm) |> 
  distinct()



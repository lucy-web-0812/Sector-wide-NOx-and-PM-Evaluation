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

total_nox_from_aviation_sector_Mt <- 3
total_fuel_consumption_from_aviation_sector_Mt <- 188
kerosene_energy_content_kWh_kg <- 12


average_nox_g_kWh <- (total_nox_from_aviation_sector_Mt * 1000 /total_fuel_consumption_from_aviation_sector_Mt)  /  kerosene_energy_content_kWh_kg 


# And format it such that can be combined with other datasets to produce the mega plot

plane_data <- data.frame(plane_power_approx_kw, average_nox_g_kWh, summary_values_planes$standard_error_in_thrust)



collated_data <- plane_data |> 
  mutate(Fuel = "Kerosene", Product = "Commercial Aircraft Turbofan Engine", Directive = "ICAO") |> 
  mutate(average_nox_mg_kWh = average_nox_g_kWh * 1000) |> 
  rename(`Power (kW)` = plane_power_approx_kw, `NOx (mg/kWh)` = average_nox_mg_kWh ) |> 
  select(-c(average_nox_g_kWh, summary_values_planes.standard_error_in_thrust)) |> 
  rbind(collated_data) |> 
  distinct()



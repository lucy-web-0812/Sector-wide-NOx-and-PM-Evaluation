# Euro Emission Standards!


# This script contains the following: 
# 1) Looks at the differences in emission standards overtime 
# 2) Convert to grams per kilowatt hour
# 3) Producing final outputs to feed into the mega graph. 





# Assessing the difference in emission standards

euro_emissions_raw <- read_csv("data/euro_emissions.csv") |> 
  tibble()|> 
  rename("reg_date" = "Date (first registration)", "nox" = "Nox (g/km)", "pm" = "PM (g/km)")



# Data wrangling, need to change to 'Date of first registration' column to Year, as make dates consistent
# Also change "n/a" values to make NA in NOX and PM column

euro_emissions <- euro_emissions_raw |>   
  mutate(reg_date = if_else(nchar(reg_date) != 4,
                            parse_date_time((paste0(reg_date, "-01 00:00")), "b-y-d H:M"), 
                            parse_date_time((paste0(reg_date, "-01-01 00:00")), "y-m-d H:M"))) |> 
  mutate(reg_date = year(reg_date)) |> 
  mutate(nox = ifelse(nox == "n/a", NA, as.numeric(nox)), pm = ifelse(pm == "n/a", NA, as.numeric(pm))) 

# Convert to long data 
euro_emissions_long <- euro_emissions |> 
  pivot_longer(c("nox", "pm"), names_to = "pollutant", values_to = "conc") |> 
  group_by(pollutant,Fuel)

# Summary statistics

mean_summary <- euro_emissions |> 
  group_by(`Type of Vehicle`) |> 
  summarise(mean_nox = mean(nox, na.rm = T), mean_pm = mean(pm, na.rm = T)) |> 
  arrange(mean_nox) |> 
  ungroup()

# THIS Plot should just be for the vehicles not including heavy duty diesel engines and 
# large good vehicles because these are just in g/kwh rather than anything else 

# Some vehicles types have emission limit standards in g/kwh rather than g/km. hence we need to treat these different and monitor units 

g_kwh_vehicle_list <- c("Heavy Duty Diesel Engines", "Large Goods Vehicles" )

g_km_vehicle_types <- euro_emissions_long |> 
  filter(!(`Type of Vehicle` %in% g_kwh_vehicle_list)) |> 
  filter(pollutant == "nox")

g_kwh_vehicle_types <- euro_emissions |> 
  filter(`Type of Vehicle` %in% g_kwh_vehicle_list) 


# Plot for g_km vehicles 
ggplot(g_km_vehicle_types, aes(x=reg_date, y = conc)) +
  geom_point(aes(x=reg_date, y = conc, colour = `Fuel`)) +
  scale_y_continuous(name = "NOx emission limit values (g/km)", expand = c(0,0)) +
  scale_x_continuous(name = "Year", limits = c(2000,2025)) +
  geom_line(data = filter(g_km_vehicle_types, `Fuel` == "Diesel"), colour = "pink", stat = "smooth") +
  geom_line(data = filter(g_km_vehicle_types, `Fuel` == "Petrol"), colour = "blue", stat = "smooth") +           
  theme_bw() +
  theme(legend.position = "bottom") + 
  ylab("Concentration (g km-1)") +
  facet_wrap(~factor(`Type of Vehicle`, levels = c("Passenger Vehicle", "Motor Cycles", "Light Commercial Vehicles < 1,305kg reference mass", "Light Commercial Vehicles 1760 - 3500 kg reference mass","Light Commercial Vehicles 1,305 - 1760 kg reference mass" )), 
             nrow = 3, scales = "free_x")



# And a plot for g_kwh vehicles 
ggplot(g_kwh_vehicle_types, aes(x=reg_date, y = `Nox (g/kwh)`)) +
  geom_point(aes(x=reg_date, y =`Nox (g/kwh)`)) +
  scale_y_continuous(name = "NOx emission limit values (g/kWh)", expand = c(0,0), limits = c(0,10)) +
  scale_x_continuous(name = "Year", limits = c(1990,2020)) +
  geom_line() +
  #geom_line(data = filter(g_kwh_vehicle_types, `Fuel` == "Petrol"), colour = "blue", stat = "smooth") +           
  theme_bw() +
  theme(legend.position = "bottom") + 
  ylab("Concentration (g/kWh)") +
  facet_wrap(~`Type of Vehicle`)





# ---- Plot for how the emissions vary in g kwh ------


# For this we will need a value for the fuel economy of each car, the fuel content of diesel and petrol 

petrol_energy_density <- 9.5 # This is kWh/l - kilowatt hr per litre of fuel 
diesel_energy_density <- 10 

# car fuel economy - will match the fuel economy for cars to the fuel consumption for new cars from that year 
# source: - see spreadsheet avg_new_car_fuel_consumption https://www.statista.com/statistics/780748/new-car-fuel-consumption-great-britain/
# This is for new cars that year, which should meet the eurostandards!


avg_new_car_fuel_consumption <- read_xlsx("data/avg_new_car_fuel_consumption.xlsx", sheet = "Data") |> 
  mutate(reg_date = as.numeric(year)) |> 
  tibble()

# This data only only goes back to 2000, therefore for the years before (and after, as do have two cases of 2025 data), 
# these should be added to all columns outside of the 2000-2020 range 

petrol_fuel_eff_pre_2000 <- 8 # L/100km 
diesel_fuel_eff_pre_2000 <- 6.3

petrol_fuel_eff_post_2020 <- 5.4
diesel_fuel_eff_post_2020 <- 5


# For the other fuel economy values, this is likely to be highly variable between specific uses, models, etc. 
# Now will just make an assumption based upon reasonable values, without time varying behaviour 

# All values are in the units of L/100km - this is where can change when playing with values 

petrol_fuel_eff_lcv <- 10 # changing around with these values 
petrol_fuel_eff_motorcycles <- 8
petrol_fuel_eff_lgv <- 18


diesel_fuel_eff_lcv <- 9
diesel_fuel_eff_lgv <- 25
diesel_fuel_eff_hdde <- 25

# Need to also include an efficiency factor of the fuel - for now assume 30% 

diesel_efficiency_pct <- 0.33
petrol_efficiency_pct <- 0.25


# Now to convert all the units from g/km to g/kWh   


converted_units_data <- left_join(euro_emissions_long, avg_new_car_fuel_consumption, by = "reg_date") |> 
  select(!"year") |> 
  rename(petrol_fuel_eff = petrol_cars, diesel_fuel_eff = diesel_cars) |> 
  # performing the changes if older than 2000 and younger than 2020
  mutate(petrol_fuel_eff = ifelse(reg_date <= 2000, petrol_fuel_eff_pre_2000, petrol_fuel_eff), diesel_fuel_eff = ifelse(reg_date <= 2000, diesel_fuel_eff_pre_2000, diesel_fuel_eff)) |>  
  mutate(petrol_fuel_eff = ifelse(reg_date >= 2021, petrol_fuel_eff_post_2020, petrol_fuel_eff), diesel_fuel_eff = ifelse(reg_date >= 2021, diesel_fuel_eff_post_2020, diesel_fuel_eff)) |>
  # performing the changes if not cars 
  # Note: Here for each entry we are assigning a petrol AND a diesel fuel efficiency, regardless of whether the fuel is actually diesel or petrol
  mutate(petrol_fuel_eff = ifelse(str_starts(`Type of Vehicle`, "Light Commercial Vehicle"), petrol_fuel_eff_lcv, petrol_fuel_eff), diesel_fuel_eff = ifelse(str_starts(`Type of Vehicle`, "Light Commercial Vehicle"), diesel_fuel_eff_lcv, diesel_fuel_eff )) |> 
  mutate(petrol_fuel_eff = ifelse(`Type of Vehicle` == "Motorcyles", petrol_fuel_eff_lgv, petrol_fuel_eff)) |> 
  # Now convert the units  - this is the stage with the equations
  mutate(nox_mgkwh = as.numeric(ifelse(Fuel == "Petrol" & pollutant == "nox", conc / ((petrol_fuel_eff/100) * (petrol_energy_density * petrol_efficiency_pct)) * 1000, NA)), 
         nox_mgkwh = as.numeric(ifelse(Fuel == "Diesel" & pollutant == "nox", conc / ((diesel_fuel_eff/100) * (diesel_energy_density * diesel_efficiency_pct)) * 1000, nox_mgkwh)) , 
         nox_mgkwh = as.numeric(ifelse(`Type of Vehicle` == "Heavy Duty Diesel Engines" |`Type of Vehicle` ==  "Large Goods Vehicles", `Nox (g/kwh)`* 1000, nox_mgkwh)))


# check to see if looks sensible.... 

ggplot(filter(converted_units_data, nox_mgkwh != "NA"), aes(x = reg_date, y = nox_mgkwh) ) + 
  geom_smooth(aes(x = reg_date, y = nox_mgkwh, colour = `Type of Vehicle`)) + 
  geom_point(aes(colour = `Type of Vehicle`))  +
  theme_classic() 




# Now make this into a nice table format where it can be easily read 
# This calculates the mean value of the NOX emission limit value since 2010 for each type & fuel type of vehicle
mean_table <- converted_units_data |> 
  filter(pollutant != "pm") |> 
  select(`Type of Vehicle`, `Fuel`, reg_date, petrol_fuel_eff, diesel_fuel_eff, nox_mgkwh) |> 
  filter(nox_mgkwh != "NA", reg_date >= 2010) |> 
  group_by(factor(`Type of Vehicle`), Fuel) |> 
  summarise(mean_nox_mgkwh = mean(nox_mgkwh))



# However, to get the most recent and up to date standards only want one entry per vehicle category and fuel type

# List of names of the different types of vehicles
vehicle_classes <- unique(converted_units_data$`Type of Vehicle`)

# Create empty list for products of the loop to go into 
current_elv_list <- list()

for (i in 1:length(vehicle_classes)){ # Loop through each type of vehicle
  elv_values <- converted_units_data |> 
    filter(`Type of Vehicle` == vehicle_classes[[i]], pollutant == "nox", reg_date < 2025) |> # Disclude dates older than 2025
    arrange(desc(reg_date)) |> 
    head(2) # Retain only two most recent values
  
  # Now need an if else statement to determine whether these are for two different fuel types of both diesel/petrol etc
  
  if(elv_values[1,4] == elv_values[2,4]){ 
    current_elv_list[[i]] <- elv_values[1,]
  }
  else{
    current_elv_list[[i]] <- elv_values[1:2,]
  }
}

current_standards <- bind_rows(current_elv_list)  |>  # Bind these values into one dataframe
  mutate(reg_date = as.Date(as.character(reg_date), format = "%Y")) # Make sure the reg date is in correct format


# Plot of when the standards were put into place and the NOx emission limit value 

ggplot(current_standards) +
  geom_point(aes(x = reg_date, y = nox_mgkwh, colour = `Fuel`)) +
  scale_x_date(name = "Legislation Date", date_breaks = "2 years", date_labels = "%Y") +
  scale_y_continuous(name = "NOx Emission Limit Value (mg/kWh)")

# Bar chart comparing emission limit values for different types of vehicles - length(names(`Fuel`)

ggplot(current_standards) +
  geom_col(aes(y = `Type of Vehicle`, x = nox_mgkwh, fill = `Fuel`), position = "dodge") +
  scale_y_discrete(name = "Type of Vehicle", labels = scales::label_wrap(30)) + 
  scale_x_continuous(name = "NOx Emission Limit Value (mg/kWh)", expand = c(0,0), limits = c(0,1250)) + 
  scale_fill_manual(values = c("#fa9fb5","lightblue" )) +
  ggtitle("Road Emission Standards") +
  labs(subtitle = "Current European Nitrous Oxide (NOx) Emission Standards for vehicles driven on roads") +
  theme_bw() +
  theme(axis.text.y = element_text(face = "bold", size = 8),
        axis.title.y = element_text(face = "bold", size = 12),
        axis.title.x = element_text(face = "bold", size = 12), 
        plot.title = element_text(face = "bold", size = 20), 
        plot.subtitle = element_text(face = "italic"), 
        plot.background = element_rect(fill = "aliceblue"),
        legend.background = element_rect(fill = "aliceblue")) 



# ------ Producing Final Outputs to add to later plots.... -------

# In order to compare to other values, will need to append power values to this dataset 

power_df <- data.frame("vehicle_type" = vehicle_classes, 
                       power_kw = as.numeric(c("100", # Passenger Vehicles
                                               "9",  # Motor Cycles 
                                               "125", # Light Commercial Vehicles 1760 - 3500 kg reference mass
                                               "100", # Light Commercial Vehicles 1,305 - 1760 kg reference mass
                                               "77", # Light Commercial Vehicles < 1,305kg reference mass
                                               "460"))) # Heavy Duty Diesel Engines
# Rename column to ensure the join works
euro_emissions_long <- euro_emissions_long |> 
  rename("vehicle_type" = "Type of Vehicle") 

current_standards <- current_standards |> 
  rename("vehicle_type" = "Type of Vehicle") 


# Produce the final, long dataset  
euro_standards_final <- left_join(euro_emissions_long, power_df, by = "vehicle_type") |> 
  mutate(power_kw = as.numeric(power_kw))

# Produce the final current emissions dataset 
current_standards_final <-   left_join(current_standards, power_df, by = "vehicle_type") |> 
  mutate(power_kw = as.numeric(power_kw))


collated_data <- current_standards_final |> 
  ungroup() |> 
  select(c(vehicle_type, Fuel, nox_mgkwh, power_kw)) |> 
  mutate(Directive = "Road Transport") |> 
  rename(Product = vehicle_type, `Power (kW)` = power_kw, `NOx (mg/kWh)` = nox_mgkwh ) |> 
  rbind(collated_data) |> 
  distinct()












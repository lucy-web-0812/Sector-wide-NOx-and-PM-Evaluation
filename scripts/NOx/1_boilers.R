# Boilers ! 

# --- Intro ---

# Script to take in the emissions values that have been collated for various home appliances covered by the eco-design directive


# ---- Load Data ----
# Likely to evolve as collate more data in the appliance emissions excel file 

boilers <- read_csv("data/boilers.csv") |> 
  rename(name = "Name", fuel = "Fuel Type", power_kw = "Power (kW)", nox_mgkwh = "NOx emissions (mg/kWh)", usage = "Usage")


boiler_summary <- boilers |> 
  group_by(fuel, usage) |> 
  summarise(count = n(), 
            min_power = min(power_kw), 
            mean_power = mean(power_kw),
            standard_error_power = sd(power_kw)/sqrt(n()), 
            max_power = max(power_kw),
            sd_power = sd(power_kw), 
            min_nox = min(nox_mgkwh), 
            mean_nox = mean(nox_mgkwh),
            max_nox = max(nox_mgkwh),
            standard_error_nox = sd(nox_mgkwh)/sqrt(n()), 
            sd_nox = sd(nox_mgkwh)) |> 
  mutate(product = "boiler") |> 
  mutate(fuel_state = ifelse(fuel == "NG", as.character("gaseous fuels"), as.character("liquid fuels")) )


fileName <- file.path("data", "boilers_summary.csv")

write.csv(boiler_summary, file = fileName)


# Check this all looks sensible... 

ggplot(boiler_summary, aes(x=mean_power, y = mean_nox, colour = fuel, shape = usage)) +
  geom_point(aes( colour = fuel, shape = usage), size = 3) +
  geom_errorbar(aes(ymax = mean_nox + standard_error_nox, ymin = mean_nox - standard_error_nox)) +
  geom_errorbarh(aes(xmin = mean_power - standard_error_power, xmax = mean_power + standard_error_power)) +
  scale_colour_bmj(name = "Fuel") +
  scale_shape(name = "Usage") +
  scale_size(name = "Count") +
  scale_y_continuous(name =  expression(bold("NOx (mg kWh"^"-1"*")")), breaks = seq(0,120,20), limits = c(0,120), expand = c(0,0)) +
  scale_x_continuous(name = "Power (kW)", limits = c(0,NA), expand = c(0,0)) +
  mypackage::theme_lucy() +
  theme(legend.position = "right", 
        legend.title = element_text(size = 18), 
        legend.text = element_text(size = 14))




# And make the output into useable format for collated dataset.... 

collated_data <- boiler_summary |> 
  select(c(fuel, usage, mean_power, mean_nox, product)) |> 
  mutate(Product = paste(usage, fuel, product)) |> 
  mutate(Directive = "Eco-Design") |> 
  rename(`Power (kW)` = mean_power, `NOx (mg/kWh)` = mean_nox, Fuel = fuel) |> 
  select(-c(usage, product)) |> 
  rbind(collated_data) |> 
  distinct() # Removes any extra rows that have appeared if this chunk is run twice! 
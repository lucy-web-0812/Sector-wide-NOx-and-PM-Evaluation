# Looking at the emission factors from the EMEP/EEA guidance appendix D - https://www.eea.europa.eu/publications/emep-eea-guidebook-2019/part-b-sectoral-guidance-chapters/1-energy/1-a-combustion/1-a-1-energy-industries/view


converted_data_raw <- read_xlsx("data/IED_data.xlsx", sheet = "from_EMEPEEA") |> 
  select(`Source[1]`:`...10`) 


# Renaming columns so they can be understood

col_names <- c("source", "fuel_types", "new_or_existing", "min_boiler_size", "max_boiler_size", "ref_O2_content", "low_ELV_mg_Nm3", "high_ELV_mg_Nm3", "low_EF_gGJ", "high_EF_gGJ")
colnames(converted_data_raw) <- col_names


numeric_columns <- c("min_boiler_size", "max_boiler_size", "ref_O2_content", "low_ELV_mg_Nm3", "high_ELV_mg_Nm3", "low_EF_gGJ", "high_EF_gGJ")

# Data reshaping and adding necessary columns....
converted_data <- converted_data_raw |> 
  filter(is.na(source) == FALSE) |> 
  mutate(across(all_of(numeric_columns), as.numeric)) |> # Making columns as defined above numeric 
  mutate(max_boiler_size = ifelse(max_boiler_size == "NA", min_boiler_size, max_boiler_size)) |> 
  mutate(min_boiler_size = ifelse(is.na(min_boiler_size) == T, max_boiler_size, min_boiler_size)) |> 
  pivot_longer(cols = c("min_boiler_size", "max_boiler_size"), names_to = "boiler_size", values_to = "power_kw") |> 
  pivot_longer(cols = c("low_ELV_mg_Nm3", "high_ELV_mg_Nm3"), names_to = "ELV_bound", values_to = "ELV_mgNm3") |> 
  mutate(low_EF_g_kwh = low_EF_gGJ/277.8, high_EF_g_kwh = high_EF_gGJ/277.8) |> # Converting from gGJ to gkWh 
  pivot_longer(cols = c("low_EF_g_kwh", "high_EF_g_kwh"), names_to = "converted_EF_bound", values_to = "EF_g_kwh") |> 
  mutate(ratio_conc_per_kwh = ifelse(substr(ELV_bound, 1, 4) ==  substr(converted_EF_bound, 1, 4), ELV_mgNm3/EF_g_kwh, NA))  |>  # Making sure only doing ratio if either both the low or both the high bound
  filter(is.na(ratio_conc_per_kwh) == FALSE) |> 
  mutate(source = as.factor(source), fuel_types = as.factor(fuel_types)) 



fuel_type_name <- c(`coal` = "Coal",gas = "Gas", oil = "Oil", wood = "Wood")



# Overall plot of the data, faceted by the fuel type
ggplot(converted_data) +
  geom_point(aes(x=ELV_mgNm3, y = EF_g_kwh, colour = fuel_types), size = 4) +
  geom_line(aes(x=ELV_mgNm3, y = EF_g_kwh,  colour = fuel_types), linetype = "dashed", linewidth = 1.2) +
  scale_y_continuous(name = "Emission Factor (g kWh-1)", limits = c(0,NA)) +
  scale_x_continuous(name = "Emission Limit Value (mg Nm-3)", limits = c(0,NA)) +
  scale_colour_brewer(palette = "Set1") +
  rsc_theme_full_spacing() +
  facet_wrap(~fuel_types, scales = "free", labeller = as_labeller(fuel_type_name)) +
  theme(legend.position = "none", 
        strip.text = element_text(face = "bold"), 
        strip.background = element_rect(fill = "white"))


ggsave(filename = "plots/supp_info/conversion_charts.png", device = "png", height = 20, width = 20, units = "cm")


ggplot(filter(converted_data, source == "BREF")) +
  #ggplot(converted_data) +  
  geom_bar(aes(x = ratio_conc_per_kwh, fill = fuel_types, colour = fuel_types), 
           alpha = 0.3) +
  geom_density(aes(x = ratio_conc_per_kwh,  fill = fuel_types, colour = fuel_types),  
               alpha = 0.3) +
  #scale_y_continuous(name = "Count", 
  #sec.axis = sec_axis(~ . / 1000, name = "Density")) +
  facet_wrap(~fuel_types, nrow = 2, ncol = 2, scales = "free_y")


# Now trying to model all of these 
# Do as linear model and extract the gradients... 



model_results <- list()

# Loop over unique values of fuel_types
for (fuel in unique(converted_data$fuel_types)) {
  
  # Filter the data for the current source and fuel_types
  converted_data_filtered <- converted_data |>
    filter(fuel_types == fuel)
  
  
  model_results[[fuel]] <- lm(ELV_mgNm3 ~ 0 +EF_g_kwh, data = converted_data_filtered)
  
}



# And now making a dataframe to store the results: 

results_df <- data.frame(fuel_type = character(),
                         gradient = numeric(),
                         intercept = numeric(),
                         r_squared = numeric(),
                         stringsAsFactors = FALSE)


# Loop to extract model results and put into a form when can read - to be stored in resutls_df
for (fuel in names(model_results[])) {
  
  model <- model_results[[as.character(fuel)]]
  
  # Extract coefficients and R-squared
  intercept <- 0
  gradient <- coef(model)[1]
  r_squared <- summary(model)$r.squared
  
  # Append to results_df
  results_df <- rbind(results_df, 
                      data.frame(fuel_type = fuel, 
                                 gradient,
                                 intercept, 
                                 r_squared))
  
}

# Now need to make a model that takes an input in mg Nm3, the fuel type, power plant efficiency and then outputs in g kWh 

emission_limit_value_converter <- function(ELV_mg_Nm3, fuel, efficiency){
  
  conversion_factor <- results_df |> 
    filter(fuel_type == fuel) |> 
    pull(gradient)
  
  converted_limit <-  ELV_mg_Nm3 / (conversion_factor * efficiency)
  
  print(converted_limit)
  
}


wood_150 <- data.frame(efficiency = as.numeric(), 
                       converted_value = as.numeric())


for (efficiency in seq(0.05, 1, by = 0.05)) { 
  
  
  efficiency <- efficiency
  converted_value <- emission_limit_value_converter(150, "wood", efficiency = efficiency)
  
  
  wood_150 <- wood_150 |> 
    rbind(tibble(efficiency, converted_value))
  
}

# Here we have what drax power station would look like 


ggplot(wood_150) +
  geom_line(aes(x = efficiency, y = converted_value))

emission_limit_value_converter(150,"wood",0.3)
emission_limit_value_converter(150,"wood",0.4)
emission_limit_value_converter(150,"wood",0.95)





# --- Load in the data to convert ---- 
# Here we have the simplified form without too much detail... 

mcp_ied_standards <- read_csv("data/MCP_IED_standards_for_conversion.csv") |> 
  pivot_longer(cols = c(`Gas Turbine`:`Coal Boiler`), names_to = "CP_type", values_to = "nox_ELV_mg_Nm3") |> 
  mutate(efficiency = ifelse(grepl("Gas", CP_type, ignore.case = TRUE), 0.5, 0.36)) |> 
  mutate(conversion_factor = case_when(grepl("Gas", CP_type, ignore.case = TRUE) ~ 981,
                                       grepl("Coal", CP_type, ignore.case = TRUE) ~ 767,
                                       grepl("Biomass", CP_type, ignore.case = TRUE) ~ 720))


# Now apply the conversions..... 

mcp_ied_standards_with_conversions <- mcp_ied_standards |> 
  mutate(converted_nox_g_kwh = nox_ELV_mg_Nm3/ (conversion_factor * efficiency)) |> 
  mutate(converted_nox_g_kwh_max_efficiency = nox_ELV_mg_Nm3/ (conversion_factor * 0.95)) |> 
  mutate(converted_nox_g_kwh_min_efficiency = nox_ELV_mg_Nm3/ (conversion_factor * 0.30)) |> 
  mutate(max_power = ifelse(is.na(max_power), 3000, max_power), 
         avg_power = (max_power + min_power)/2)


ggplot(mcp_ied_standards_with_conversions) +
  geom_point(aes(x = avg_power * 1000, y = converted_nox_g_kwh, colour = CP_type)) +
  geom_errorbarh(aes(xmin = min_power * 1000, xmax = max_power * 1000, y = converted_nox_g_kwh), colour = "grey") +
  geom_pointrange(aes(ymin = converted_nox_g_kwh_max_efficiency, ymax = converted_nox_g_kwh_min_efficiency, x = avg_power * 1000, y = converted_nox_g_kwh), colour = "grey") +
  geom_line(aes(x = avg_power * 1000, y = converted_nox_g_kwh, colour = CP_type)) +
  geom_line(aes(x = avg_power * 1000, y = converted_nox_g_kwh_max_efficiency, colour = CP_type)) +
  geom_line(aes(x = avg_power * 1000, y = converted_nox_g_kwh_min_efficiency, colour = CP_type)) +
  scale_x_continuous(trans = "log10") +
  facet_wrap(~CP_type, scales = "free")


write.csv(mcp_ied_standards_with_conversions, file = "data/mcp_ied_standards_with_conversions.csv")

# And converting to the standard table format 


collated_data <- mcp_ied_standards_with_conversions |> 
  select(c(directive, converted_nox_g_kwh, avg_power, CP_type)) |> 
  mutate(converted_nox_mg_kwh = converted_nox_g_kwh * 1000, avg_power_kw = avg_power * 1000) |> 
  rename(Directive = directive, Product = CP_type, `Power (kW)` = avg_power_kw,  `NOx (mg/kWh)` = converted_nox_mg_kwh) |> 
  mutate(Fuel = word(Product, 1)) |> 
  select(-c(converted_nox_g_kwh, avg_power)) |> 
  rbind(collated_data) |> 
  distinct()



# And ALSO need to add in the data collated from ESG reports...... 










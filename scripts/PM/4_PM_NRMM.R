# Looking at the NRMM values....


nrmm_data_raw <- read_csv("data/nrmm_pm_limits.csv") |> 
  clean_names() |> 
  mutate(average_power = ifelse(is.na(max_power_k_w) ==T , min_power_k_w, (min_power_k_w + max_power_k_w) / 2)) |> 
  select(c(engine_sub_catergory, average_power, pm_mass_g_k_wh))

# Also need the NRMM product list.... 


nrmm_products <- read_csv("data/nrmm_products.csv") |> 
  select(c(category, product, power_min, power_max, NRMM_engine_sub_cat_min, NRMM_engine_sub_cat_max))


# The long engine category title is taken by the engine column in 'nrmm_pm_limits'. 
# Need to change this to allow for the data to be joined

nrmm_data <- nrmm_data_raw |> 
 mutate(engine_sub_catergory = str_replace_all(engine_sub_catergory, "-v", ""), 
        engine_sub_catergory = str_replace_all(engine_sub_catergory, "-c", ""),)




nrmm_products_and_limits <- nrmm_products |> 
  left_join(nrmm_data, join_by( NRMM_engine_sub_cat_min == engine_sub_catergory), relationship = "many-to-many") |> 
  rename(pm_limit_g_kwh_lower = pm_mass_g_k_wh) |> 
  left_join(nrmm_data, join_by( NRMM_engine_sub_cat_max == engine_sub_catergory), relationship = "many-to-many") |>
  rename(pm_limit_g_kwh_upper = pm_mass_g_k_wh) |> 
  distinct()


# Make a scheme for the NRE data 



# For now just add the data based on the NRMM category 


collated_data_pm <- nrmm_data |> 
  filter(is.na(pm_mass_g_k_wh) == F) |> 
  rename("Power (kW)" = average_power, "Product" = engine_sub_catergory) |> 
  mutate(`PM (mg/kWh)` = pm_mass_g_k_wh * 1000, `Directive` = "NRMM", `Fuel` = NA)  |> 
  select(-c(pm_mass_g_k_wh)) |> 
  rbind(collated_data_pm) |> 
  distinct()



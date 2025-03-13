

# This script takes the list of products that the NAEI use in their model. 
# Alongside these power values and fuel types, the engine categories as defined by EU regulation has been added. 

# This script is to assign the power limits based upon the fuel type and engine category of the data 

raw_data <- read.csv("data/nrmm_products.csv")


# dividing into product and reference list 

product_list <- raw_data |> 
  select(c(category:nox_limit_max_cat_gkwh)) |> 
  mutate(mean_power = ifelse(is.na(power_max), power_min, ((power_max + power_min) / 2) )) |> 
  mutate(mean_nox = ifelse(is.na(nox_limit_max_cat_gkwh), nox_limit_min_cat_gkwh, ((nox_limit_max_cat_gkwh + nox_limit_min_cat_gkwh) / 2))) 
# Look at the 5_2_NRMM script where weighted values have been used instead of this quick method

reference_list <- raw_data |> 
  select(c(engine_sub_category:nox_limit)) |> 
  na.omit()


ggplot(product_list) +
  geom_point(aes(x = mean_power, y = mean_nox))


# So can be added to the Collating standards list

product_list_nrmm <- product_list |> 
  mutate(directive = "NRMM", power_kw = mean_power, nox_mgkwh = mean_nox * 1000 )






# And plotting with errors 

ggplot(product_list_nrmm, aes(x = power_kw, y = nox_mgkwh)) +
  geom_point() +
  geom_errorbar(aes(ymin = (nox_limit_min_cat_gkwh * 1000), ymax = (nox_limit_max_cat_gkwh * 1000) ),  alpha = 0.5) +
  geom_errorbarh(aes(xmin = power_min, xmax = power_max), alpha = 0.5) +
  scale_y_continuous(name = "NOx mg/kWh", limits = c(0, 7500)) +
  scale_x_continuous(trans = "log10")


# And we need the data that does not need to the ratio approportionment method applied that has been taken straight from the legislation 


non_ratio_method <- read_csv("data/nrmm_data_non_approportionment.csv") |> 
  mutate(`Power (kW)` = ifelse(is.na(`Max Power (kW)`) == T, `Min Power (kW)`, (`Min Power (kW)` + `Max Power (kW)`)/2))



collated_data <- non_ratio_method |> 
  select(c(`Engine`, `Power (kW)`, `Nox (g/kWh)`)) |> 
  mutate(Directive = "NRMM", Fuel = NA, `NOx (mg/kWh)` = `Nox (g/kWh)` * 1000) |> 
  select(-c(`Nox (g/kWh)`)) |> 
  rename(Product = Engine) |> 
  rbind(collated_data) |> 
  distinct()
 






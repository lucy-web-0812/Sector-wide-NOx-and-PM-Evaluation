
# Script to look at how the emission limit values varies across the power spectrum 

#---------- Section A ---------- 
# Emission Limit Values for the engine categories

# First create a dataframe, will go up to 1000kW

df <- data.frame(power = seq(0, 1001, by = 0.1)) # Need to amend this as only works for the secondary section when increasing in integer values
df$max_nox_diesel <- NA # Adding an empty column to be added to during loop
df$max_nox_2SG <- NA
df$max_nox_4SG <- NA
df$engine_category <- NA


ELV_df <- data.frame(engine_sub_category = c("NRE-1", "NRE-2", "NRE-3", "NRE-4", "NRE-5", "NRE-6", "NRE-7"), 
                     max_power = c("8","19","37","56","130","560", "1000"), 
                     elv_diesel = c("6.72963400236128", "6.72963400236128",
                                    "4.22334905660377", "4.30456730769231",
                                    "0.4", "0.4", "3.5"), 
                     elv_2SG = c("0.022542831", "0.020969245",
                                 "0.013140727", "0.016414435",
                                 "0.4", "0.4", "3.5"), 
                     elv_4SG = c("1.539379475","2.955882353", "1.618786693", "1.192195122", "0.4", "0.4", "3.5"))|> 
  mutate(max_power = as.numeric(max_power), 
         elv_diesel = as.numeric(elv_diesel), 
         elv_2SG = as.numeric(elv_2SG), 
         elv_4SG = as.numeric(elv_4SG))



for (i in 1:nrow(ELV_df)) {
  
  df <- df |> 
    mutate(max_nox_diesel = ifelse(power < ELV_df$max_power[i] & is.na(max_nox_diesel) == TRUE, ELV_df$elv_diesel[i], max_nox_diesel),
           max_nox_2SG = ifelse(power < ELV_df$max_power[i] & is.na(max_nox_2SG) == TRUE, ELV_df$elv_2SG[i], max_nox_2SG),
           max_nox_4SG = ifelse(power < ELV_df$max_power[i] & is.na(max_nox_4SG) == TRUE, ELV_df$elv_4SG[i], max_nox_4SG),
           engine_category = ifelse(is.na(engine_category) == TRUE & is.na(max_nox_diesel) == FALSE, ELV_df$engine_sub_category[i], engine_category)) 
  
} 


complete_df <- df |> 
  mutate(max_nox_diesel = as.numeric(max_nox_diesel), 
         max_nox_2SG = as.numeric(max_nox_2SG), 
         max_nox_4SG = as.numeric(max_nox_4SG)) |> 
  pivot_longer(cols = c(max_nox_diesel, max_nox_2SG, max_nox_4SG), names_to = "fuel_engine_type", values_to = "max_nox") |> 
  na.omit()

engine_names <- c(`max_nox_2SG` = "Two Stroke Petrol", `max_nox_4SG` = "Four Stroke Petrol", `max_nox_diesel` = "Diesel") # Changing the Labels for the Faceted plot below


ggplot(complete_df) +
  geom_area(aes(x = power, y = max_nox, fill = engine_category), alpha = 0.8) +
  scale_x_continuous(name = "Power (kW)", expand = c(0.01,0), limits = c(0.1, NA), #trans = "log10", 
                     labels = c("1", "10", "100", "1000"), 
                     breaks = c(1,10,100,1000)) + 
  scale_y_continuous(name = "NOx ELV (g/kWh)", expand = c(0,0)) +
  scale_fill_discrete(name = "Engine Sub Category") +
  theme_lucy() +
  theme(legend.position = "right") +
  facet_wrap(~fuel_engine_type, ncol = 1, labeller = as_labeller(engine_names), scales = "free")



# and focusing on just the categories up to 100 kW

ggplot(complete_df) +
  geom_area(aes(x = power, y = max_nox, fill = engine_category), alpha = 0.8) +
  scale_x_continuous(name = "Power (kW)", expand = c(0.01,0), limits = c(0.9, NA), trans = "log10", 
                     labels = c("1", "10", "100", "1000"), 
                     breaks = c(1,10,100,1000)) + 
  scale_y_continuous(name = "NOx ELV (g/kWh)", expand = c(0,0)) +
  scale_fill_discrete(name = "Engine Sub Category") +
  theme_lucy() +
  theme(legend.position = "right") +
  facet_wrap(~fuel_engine_type, ncol = 1, labeller = as_labeller(engine_names))

# ------ Section B ------ 
# Need to work out weighted means for the NOx values for NRMM products
# This assumes a uniform distribution across power ratings 


raw_product_data <- read.csv("data/nrmm_products.csv") |> 
  select(c(category:nox_limit_max_cat_gkwh)) |> 
  filter(is.na(power_max) == FALSE) # removing the one case where there is not a max power value. 

raw_product_data$sum_diesel_nox <- NA # Defining new columns to be added to
raw_product_data$avg_diesel_nox <- NA
raw_product_data$sum_2SG_nox <- NA
raw_product_data$avg_2SG_nox <- NA
raw_product_data$sum_4SG_nox <- NA
raw_product_data$avg_4SG_nox <- NA





for (i in 1:nrow(raw_product_data)) {
  
  df <- df |> 
    filter(power %in% (1:1000) ) # Require only the integer values of power - now should mean that any resolution chosen in section A should still give correct weighted NOx values
  
  if (raw_product_data$fuel[i] == "Diesel") {
    
    raw_product_data$sum_diesel_nox[i] <- sum(df$max_nox_diesel[(raw_product_data[i,]$power_min +1):(raw_product_data[i,]$power_max)]) # summing the powers from min to max 
    raw_product_data$avg_diesel_nox[i] <- ifelse(is.na(raw_product_data$nox_limit_max_cat_gkwh[i]), # Adding in an if else statement to check for cases where the product 
                                                 raw_product_data$nox_limit_min_cat_gkwh[i],        # fits entirely in one engine sub-category
                                                 raw_product_data$sum_diesel_nox[i]/ceiling(((raw_product_data[i,]$power_max) - (raw_product_data[i,]$power_min)))) 
    
  } 
  else if (raw_product_data$fuel[i] == "2SG") {
    
    raw_product_data$sum_2SG_nox[i] <- sum(df$max_nox_2SG[(raw_product_data[i,]$power_min + 1):(raw_product_data[i,]$power_max)])
    raw_product_data$avg_2SG_nox[i] <-  ifelse(is.na(raw_product_data$nox_limit_max_cat_gkwh[i]), # Adding in an if else statement to check for cases where the product 
                                               raw_product_data$nox_limit_min_cat_gkwh[i],        # fits entirely in one engine sub-category
                                               raw_product_data$sum_2SG_nox[i]/ceiling(((raw_product_data[i,]$power_max) - (raw_product_data[i,]$power_min)))) 
    
  } 
  else {
    
    raw_product_data$sum_4SG_nox[i] <- sum(df$max_nox_4SG[(raw_product_data[i,]$power_min +1):(raw_product_data[i,]$power_max)])
    raw_product_data$avg_4SG_nox[i] <-  ifelse(is.na(raw_product_data$nox_limit_max_cat_gkwh[i]), # Adding in an if else statement to check for cases where the product 
                                               raw_product_data$nox_limit_min_cat_gkwh[i],        # fits entirely in one engine sub-category
                                               raw_product_data$sum_4SG_nox[i]/ceiling(((raw_product_data[i,]$power_max) - (raw_product_data[i,]$power_min))))
    
  }
  
}


product_data_long <- raw_product_data |> 
  pivot_longer(cols = c(avg_diesel_nox, avg_2SG_nox, avg_4SG_nox), names_to = "fuel_engine_type", values_to = "weighted_mean_nox") |> 
  mutate(avg_power = ceiling((power_min + power_max) / 2))  # Adding in the average power value by just taking average across power range

# Plotting this data to give visualisation of where the products sit 

ggplot(product_data_long, aes(x = avg_power, y = weighted_mean_nox)) +
  geom_point(aes(colour = fuel_engine_type )) +
  scale_color_brewer(palette = "Set2") +
  scale_x_continuous(name = "Power (kW)", limits = c(-10,1000), expand = c(0,0)) +
  scale_y_continuous(name = "NOx ELV (g/kWh)", limits = c(0,8), expand = c(0,0)) +
  geom_errorbarh(aes(xmin = power_min, xmax = power_max, colour = fuel_engine_type)) +
  #geom_label_repel(aes(label = product))  +
  theme_lucy() +
  theme(legend.position = "right")


# Now filtering by different fuel types and overlaying the products looked at: 

# --- Diesel --- 

diesel_products <- product_data_long |> 
  filter(fuel == "Diesel") 

ggplot(diesel_products, aes(x = avg_power, y = weighted_mean_nox)) +
  geom_area(data = filter(complete_df, fuel_engine_type == "max_nox_diesel"), aes(x = power, y = max_nox, fill = engine_category), alpha = 0.8) +
  geom_point() +
  #scale_color_brewer(palette = "Set2") +
  scale_x_continuous(name = "Power (kW)", limits = c(-10,1000), expand = c(0,0), trans = "log10") +
  scale_y_continuous(name = "NOx ELV (g/kWh)", limits = c(0,8), expand = c(0,0)) +
  geom_errorbarh(aes(xmin = power_min, xmax = power_max), height =0.05) +
  #geom_label_repel(aes(label = product))  +
  theme_lucy() +
  theme(legend.position = "right")


# --- 2SG --- 

two_2SG_products <- product_data_long |> 
  filter(fuel == "2SG") 

r <- ggplot(two_2SG_products, aes(x = avg_power, y = weighted_mean_nox)) +
  geom_area(data = filter(complete_df, fuel_engine_type == "max_nox_2SG"), aes(x = power, y = max_nox, fill = engine_category), alpha = 0.8) +
  geom_point() +
  #scale_color_brewer(palette = "Set2") +
  scale_x_continuous(name = "Power (kW)", limits = c(-10,40), expand = c(0,0)) +
  scale_y_continuous(name = "NOx ELV (g/kWh)", limits = c(0,2), expand = c(0,0)) +
  geom_errorbarh(aes(xmin = power_min, xmax = power_max)) +
  #geom_label_repel(aes(label = product))  +
  theme_lucy() +
  theme(legend.position = "right")

# Note there is one outlying 2SG value. this is due to the product falling into the category NRSh-1. 
# This has an emission limit values of 1.543845 

ggplotly(r)

# --- 4SG --- 

four_4SG_products <- product_data_long |> 
  filter(fuel == "4SG")

ggplot(four_4SG_products, aes(x = avg_power, y = weighted_mean_nox)) +
  geom_area(data = filter(complete_df, fuel_engine_type == "max_nox_4SG"), aes(x = power, y = max_nox, fill = engine_category), alpha = 0.8) +
  geom_point() +
  #scale_color_brewer(palette = "Set2") +
  scale_x_continuous(name = "Power (kW)", limits = c(0,50), expand = c(0,0)) +
  scale_y_continuous(name = "NOx ELV (g/kWh)", limits = c(0,8), expand = c(0,0)) +
  geom_errorbarh(aes(xmin = power_min, xmax = power_max)) +
  #geom_label_repel(aes(label = product))  +
  theme_lucy() +
  theme(legend.position = "right")


# converting tables to latex format to be used in overleaf


product_data <- product_data_long |> 
  select(c(category, product, fuel:power_max, avg_power, weighted_mean_nox)) |> 
  filter(is.na(weighted_mean_nox) == FALSE) 

kable(ELV_df, "latex")


ggplot(product_data) +
  geom_point(aes(x=avg_power, y = weighted_mean_nox * 1000, colour = fuel, shape = fuel), size = 2.2) +
  scale_x_continuous(name = "Power (kW)", trans = "log10") +
  scale_y_continuous(name = expression(bold("NOx (mg kWh"^"-1"*")")), limits = c(0,7000)) +
  scale_color_brewer(name = "Type of Engine", palette = "Set2") +
  scale_shape(name = "Type of Engine") +
  geom_text_repel(aes(x=avg_power, y = weighted_mean_nox * 1000,label = product), min.segment.length = 0.01) +
  theme_lucy()



# And now making it so this can be added to the collated dataset..... 

collated_data <- product_data |> 
  select(c(product, fuel, avg_power, weighted_mean_nox)) |> 
  mutate(Directive = "NRMM", weighted_mean_nox_mg_kwh = weighted_mean_nox * 1000) |> 
  rename(Product = product, `Power (kW)` = avg_power , Fuel = fuel,  `NOx (mg/kWh)` = weighted_mean_nox_mg_kwh ) |> 
  select(-c(weighted_mean_nox)) |> 
  rbind(collated_data) |> 
  distinct()





train_data_raw <- read.csv("data/train_data.csv") |> 
  mutate(stage_v_emission_limits = ifelse(class == "66", 3.7, 2),
         extra_text = ifelse(class == "66", "g HC + NOx", "g NOx")) #Adding in the stage V emission limits 


max_power_values_for_plot <- train_data_raw |> 
  group_by(class) |> 
  summarise(max_plot_value = max(power_kw_inc_aux_loads)) # Calculating the max power for that class - this is to help with plotting 

train_data_with_plot_info <- train_data_raw |> 
  left_join(max_power_values_for_plot, by = "class") # Joining the two datasets together 


class_names <- c(`158` = "Class 158", `66` = "Class 66", `170` = "Class 170", `220_221` = "Class 220/221") # Class Names written nicely for plot 


ggplot(train_data_with_plot_info) +
  geom_hline(aes(yintercept = stage_v_emission_limits), linetype = 2, linewidth = 0.75) +
  geom_point(aes(x=power_kw_inc_aux_loads, y = nox_g_kwh, colour = class), size = 1.5) +
  geom_smooth(aes(x=power_kw_inc_aux_loads, y = nox_g_kwh, colour = class, fill = class), alpha = 0.2, linetype = 2, linewidth = 0.6) +
  #geom_hline(yintercept = 4, linetype = 2) +
  scale_x_continuous(name = "Power (kW)", expand = c(0,10), limits = c(0,NA)) +
  scale_y_continuous(name = expression(bold("NO"["x"]~"Emissions (g kWh"^{-1}*")")), expand = c(0,0.1), limits = c(0,20), breaks = seq(0,20, by = 10)) +
  # geom_label(aes(label = paste0("Stage V Limit "), 
  #                y = (stage_v_emission_limits - (2* stage_v_emission_limits/3)), 
  #                x = (max_plot_value - (max_plot_value/5))), size = 2, label.size = NA, colour = "black") +
  rsc_theme_full_spacing() +
  theme(legend.position = "none", 
        axis.line = element_line(linewidth = 0.7), 
        plot.margin = margin(0,0.5,0,0, unit = "cm"), 
        strip.background = element_rect(fill = "white"), 
        strip.text = element_text(face = "bold")) +
  facet_wrap(~class, scales = "free", labeller = as_labeller(class_names), ncol = 2)

ggsave(filename = "plots/paper_plots/train_plots.png", device = "png", height = 10, width = 17.1, units = "cm") 




train_emission_factors <- read_csv("data/train_emission_factors.csv") |> 
  mutate(`Power (kW)` = (`Min Power (kW)` + `Max Power (kW)`)/ 2, 
         `Emission Limit Nox (mg/kwh)` = `Emission Limit Nox (g/kwh)` * 1000) |> 
  rename("NOx (mg/kWh)" = `Emission Limit Nox (mg/kwh)`, Product = `Engine`) |> 
  mutate(Fuel = "Diesel", Directive = "NRMM", Product = ifelse(Product == "RLR", "Railcar", "Locomotive")) |> 
  select(c(Fuel, Directive, Product,`Power (kW)`, `NOx (mg/kWh)`))

# And producing the data for the collated dataset.... 

collated_data <- train_emission_factors |> 
  rbind(collated_data) |> 
  distinct()

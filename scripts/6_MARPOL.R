# Looking at the MARPOL emissions

# The emission limit values vary as a function of engine rate speed, ship construction date and place of emissions (emission control areas)

# Tier I - ships constructed after 01/01/2000
# Tier II - ships constructed on or after 01/01/2011
# Tier III - ships constructed on or after 01/01/2016 (North American and US Caribbean ECAs) or 01/01/2021 (Baltic and North Sea ECAs)


# Create a dataframe to store the output values

marpol_data <- data.frame(n = c(1:5000)) |> 
  mutate(tier1 = ifelse(n < 130, 17, 
                        ifelse((n >= 130 & n < 2000), 
                               (45*(n^-0.2)),
                               9.8))) |> 
  mutate(tier2 = ifelse(n < 130, 14.4, 
                        ifelse((n >= 130 & n < 2000), 
                               (44*(n^-0.23)),
                               7.7))) |> 
  mutate(tier3 = ifelse(n < 130, 3.4, 
                        ifelse((n >= 130 & n < 2000), 
                               (9*(n^-0.2)),
                               2))) 

marpol_data_long <- marpol_data |> 
  pivot_longer(cols = c("tier1", "tier2", "tier3"), names_to = "tier", values_to = "nox_limits")


ggplot(marpol_data_long) + 
  geom_line(aes(x = n, y = nox_limits, colour = tier), linewidth = 0.5) +
  scale_y_continuous(name = expression(bold("NO"["x"]~"Emission Limit (g kWh"^{-1}*")")), breaks = seq(0,20, by = 2), limits = c(0,18), expand = c(0,0)) +
  scale_x_continuous(name = "Engine Rate Speed (rpm)", expand = c(0,0)) +
  scale_color_discrete(name = "Tier", type = c("#7570B3", "#E7298A" ,"#66A61E")) +
  annotate(geom = "label", x = 2200, y = 11.75, label = str_wrap("Tier I - Ships constructed on or after 01/01/2000", 25), size = 1.5, colour = "#7570B3") +
  annotate(geom = "label", x = 2200, y = 6.25, label = str_wrap("Tier II - Ships constructed on or after 01/01/2011", 25), size = 1.5, colour = "#E7298A") +
  annotate(geom = "label", x = 1750, y = 3.55, label = str_wrap("Tier III - Ships constructed on or after 01/01/2016 operating in the 
                                                               North America or the US Caribbean ECA or 01/01/2021 for ships operating in the Baltic Sea or North Sea", 60),
           size = 1.5, colour = "#66A61E") +
  mypackage::theme_lucy() +
  theme(panel.grid.major.y = element_line(colour = "lightgrey"), 
        plot.title = element_text(size = 12, face = "bold"),
        legend.position = "none", 
        axis.text = element_text(size = 6), 
        plot.margin = margin(0.5,0.5,0,0, "cm"), 
        axis.title = element_text(face = "bold", size = 6)) 

ggsave(filename = "plots/paper_plots/MARPOL_data.png", device = "png", height = 8.3, width = 8.3, units = "cm")



# building a calculator that will take in the engines speed and give the outputs for the tiers... 

engine_rate_speed <- 2200

filter(marpol_data, n == engine_rate_speed)



# And lets get some real ship data... values have been added to the spreadsheet based upon the max engine rate speed and using this calculator. 
# A power rating has also been added from the engine spec sheets


shipping_data <- read_csv("data/shipping_emissions.csv") 



collated_data <- shipping_data |> 
  mutate(Directive = "MARPOL", `NOx (mg/kWh)` = `Tier III` * 1000, Fuel = "Marine Fuel") |> 
  select(c(Product, `Power (kW)`, `NOx (mg/kWh)`, Fuel, Directive )) |> 
  rbind(collated_data) |> 
  distinct()



write_csv(collated_data, "data/collated_dataset.csv")









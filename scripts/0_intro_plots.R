

## These are plots for the introduction 


# Firstly we have the emissions each year 

yearly_emissions_total <- read.csv("data/intro_plots/nox_trends_1977_2022.csv") |> 
  mutate(year = as.Date(paste0(year, "-01-01 00:00"))) |> 
  mutate(year = year(year)) |> 
  rename(nitrogen_oxide_emissions_million_tonnes = `emissions..million.tonnes.`) |> 
  filter(series == "Nitrogen oxides as NO2")


ggplot(yearly_emissions_total) +
  geom_line(aes(x = year, y = `nitrogen_oxide_emissions_million_tonnes`), colour = "#7570B3", linewidth = 1.2) +
  #geom_line(data = select(yearly_emissions_by_sector, year, road_transport), aes(x = year, y = road_transport/1000)) +
  #geom_line(aes(x = year, y = necr_commitment), colour = "green", linetype = 1) +
  #geom_line(aes(x = year, y = clrtap_commitment)) +
  scale_y_continuous(name = "Nitrogen Oxide Emissions (Tg) ", expand = c(0,0), limits = c(0, 3.5)) +
  scale_x_continuous(name = "Year", limits = c(1970, 2021), 
                     breaks = seq(1970, 2020, by = 5), expand = c(0,0),) +
  #ggtitle("UK Nitrogen Oxide Emissions") +
  mypackage::theme_lucy() +
  theme(axis.text.x = element_text(vjust = -1)) +
  theme(panel.grid.minor.y = element_line(linewidth = 0.5, colour = "grey"), 
        panel.grid.major.y = element_line(linewidth = 0.5, colour = "grey"), 
        panel.grid.major.x = element_blank(), 
        plot.margin = margin(unit = "cm", r = 0.5, l = 0.5)) 

ggsave(filename = "plots/intro_plots/overall_nox_trend_1970_2022.png", device = "png", height = 6.27, width = 11.69, units = "in")



yearly_emissions_by_sector <- read.csv("data/intro_plots/nox_by_sector_1990_2021.csv") |> 
  mutate(year = as.Date(paste0(year, "-01-01 00:00"))) |> 
  mutate(year = year(year))


# Use to look at how one sector has changed, can add in important stages on timeline 
ggplot(yearly_emissions_by_sector) +
  geom_line(aes(x = year, y = energy), colour = "skyblue", linewidth = 1.2) +
  scale_y_continuous(name = "Nitrogen Oxide Emissions (Tg) ", expand = c(0,0), limits = c(0,NA)) +
  scale_x_continuous(name = "Year", limits = c(1990, 2022), 
                     breaks = seq(1990, 2020, by = 5), expand = c(0,0)) +
  geom_vline(aes(xintercept = 2003), linewidth = 0.65, colour = "lightgrey", linetype = 6) +
  geom_richtext(aes(x = 2004, y = 120, label = "IED Article 30(3)"), fill = "white", angle = 90, label.size = NA) +
  geom_richtext(aes(x = 2002, y = 120, label = "IED Article 30(2)"), fill = "white", angle = 90, label.size = NA) +
  theme_classic() +
  theme(panel.grid.minor.y = element_line(linewidth = 0.5, colour = "grey"), 
        panel.grid.major.y = element_line(linewidth = 0.5, colour = "grey")) +
  theme(axis.title = element_text(face = "bold", size = 12), 
        axis.text = element_text(face = "bold") ,
        title = element_text(size = 16, face = "bold"))

ggplot(data = select(yearly_emissions_by_sector, year, road_transport)) +
  geom_line(aes(x = year, y = road_transport/1000), colour = "#7570B3", linewidth = 1) +
  #geom_line(aes(x = year, y = necr_commitment), colour = "green", linetype = 1) +
  #geom_line(aes(x = year, y = clrtap_commitment)) +
  scale_y_continuous(name = "Nitrogen Oxide Emissions (Tg) ", expand = c(0,0), limits = c(0, 1.5)) +
  scale_x_continuous(name = "Year", limits = c(1990, 2022), 
                     breaks = seq(1970, 2020, by = 5), expand = c(0,0),) +
  geom_vline(aes(xintercept = 1993), linewidth = 0.65, colour = "#66A61E", linetype = 6) +
  geom_vline(aes(xintercept = 1997), linewidth = 0.65, colour = "#66A61E", linetype = 6) +
  geom_vline(aes(xintercept = 2001), linewidth = 0.65, colour = "#66A61E", linetype = 6) +
  geom_vline(aes(xintercept = 2006), linewidth = 0.65, colour = "#66A61E", linetype = 6) +
  geom_vline(aes(xintercept = 2011), linewidth = 0.65, colour = "#66A61E", linetype = 6) +
  geom_vline(aes(xintercept = 2015), linewidth = 0.65, colour = "#66A61E", linetype = 6) +
  ggtitle("UK Road Transport Nitrogen Oxide Emissions 1990 - 2021") +
  geom_richtext(aes(x = 1993, y = 0.18, label = "Euro 1"), fill = "white", angle = 90, label.size = NA) +
  geom_richtext(aes(x = 1997, y = 0.18, label = "Euro 2"), fill = "white", angle = 90, label.size = NA) +
  geom_richtext(aes(x = 2001, y = 0.18, label = "Euro 3"), fill = "white", angle = 90, label.size = NA) +
  geom_richtext(aes(x = 2006, y = 0.18, label = "Euro 4"), fill = "white", angle = 90, label.size = NA) +
  geom_richtext(aes(x = 2011, y = 0.18, label = "Euro 5"), fill = "white", angle = 90, label.size = NA) +
  geom_richtext(aes(x = 2015, y = 0.18, label = "Euro 6"), fill = "white", angle = 90, label.size = NA) +
  theme_classic() +
  theme(panel.grid.minor.y = element_line(linewidth = 0.45, colour = "grey"), 
        panel.grid.major.y = element_line(linewidth = 0.45, colour = "grey")) +
  theme(axis.title = element_text(face = "bold", size = 12), 
        axis.text = element_text(face = "bold") ,
        title = element_text(size = 16, face = "bold"))



# Now make the data long so can plot all at once.... 


yearly_emissions_by_sector_long <- yearly_emissions_by_sector |> 
  select(!total) |> 
  pivot_longer(!year, names_to = "sector", values_to = "nox_emissions" )



ggplot(yearly_emissions_by_sector_long) +
  geom_line(aes(x = year, y = nox_emissions, colour = sector)) +
  theme_classic() +
  theme(legend.position = c(1, 1), legend.justification = c("right", "top"), legend.text = element_text(size = 6)) +
  theme(panel.grid.minor.y = element_line(linewidth = 0.5, colour = "grey"), 
        panel.grid.major.y = element_line(linewidth = 0.5, colour = "grey")) +
  theme(axis.title = element_text(face = "bold", size = 12), 
        axis.text = element_text(face = "bold") ,
        title = element_text(size = 12, face = "bold")) 

# This is an overwhelming amount of sectors, and it would just be easier to look at the top 5 
# The top 4 are: 
# Road Transport
# Energy 
# Manufacturing Industries and Construction 
# Non road transport 

# combine the rest into other - easiest to do with the wide data 

yearly_emissions_by_sector_combined <- yearly_emissions_by_sector |> 
  mutate(other_total = other_small_stationary_combustion_and_non_road_mobile_sources_and_machinery 
         + fugitive_emissions 
         + industrial_processes
         + agriculture 
         + waste
         + other)  |> 
  select(year, road_transport, energy, manufacturing_industries_and_construction, non_road_transport, other_total)


# And now make back into long....

yearly_emissions_by_sector_combined_long <- yearly_emissions_by_sector_combined |> 
  pivot_longer(!year, names_to = "sector", values_to = "nox_emissions" ) |> 
  group_by(sector)


# Line and area plot - work out the discrepancy that exists between the total line and the sum of the other sectors 


ggplot(yearly_emissions_by_sector_combined_long) +
  geom_line(aes(x = year, y = nox_emissions, colour = sector), position = "stack") +
  geom_area(aes(x = year, y = nox_emissions, colour = sector, fill = sector), alpha = 0.5) +
  geom_line(data = yearly_emissions_total, aes(x = year, y = (nitrogen_oxide_emissions_million_tonnes * 1000)), linewidth = 1.1) + # Total Line
  scale_y_continuous(name = "NOx Emissions (kilotons)", expand = c(0,0)) +
  scale_x_continuous(name = "Year", limits = c(1990,2021), expand = c(0,0), breaks = seq(1990,2020, by = 5)) +
  theme_classic() +
  theme(legend.position = c(0.97, 0.97), 
        legend.justification = c("right", "top"), 
        legend.text = element_text(size = 7), 
        legend.title = element_text(face = "bold")) +
  theme(panel.grid.minor.y = element_line(linewidth = 0.5, colour = "grey"), 
        panel.grid.major.y = element_line(linewidth = 0.5, colour = "grey")) +
  theme(axis.title = element_text(face = "bold", size = 12), 
        axis.text = element_text(face = "bold") ,
        title = element_text(size = 16, face = "bold")) +
  labs( fill = "Sector", colour = "Sector")



# Just the line plot without the area shading 

ggplot(yearly_emissions_by_sector_combined_long) +
  geom_line(aes(x = year, y = nox_emissions, colour = sector)) +
  scale_y_continuous(name = "NOx Emissions (kilotons)", expand = c(0,0)) +
  scale_x_continuous(name = "Year", limits = c(1990,2021), expand = c(0,0)) +
  theme_classic() +
  theme(legend.position = c(0.97, 0.97), 
        legend.justification = c("right", "top"), 
        legend.text = element_text(size = 7), 
        legend.title = element_text(face = "bold")) +
  theme(panel.grid.minor.y = element_line(linewidth = 0.5, colour = "grey"), 
        panel.grid.major.y = element_line(linewidth = 0.5, colour = "grey")) +
  theme(axis.title = element_text(face = "bold", size = 12), 
        axis.text = element_text(face = "bold") ,
        title = element_text(size = 16, face = "bold")) +
  labs( fill = "Sector", colour = "Sector")


# Now look at the % contribution to the NOx emissions from each sector
# Again this is data from https://www.gov.uk/government/statistics/emissions-of-air-pollutants


# Lets just get the main sectors....

yearly_emissions_by_sector_as_percentage <- read.csv("data/intro_plots/nox_by_sector_1990_2021_percentage.csv") |> 
  mutate(year = as.Date(paste0(year, "-01-01 00:00"))) |> 
  mutate(year = year(year)) |> 
  mutate(other_total = other_small_stationary_combustion_and_non_road_mobile_sources_and_machinery 
         + fugitive_emissions 
         + industrial_processes
         + agriculture 
         + waste
         + other)  |> 
  select(year, road_transport, energy, manufacturing_industries_and_construction, non_road_transport, other_total)


# Make the data long format 
yearly_emissions_by_sector_as_percentage_long <- yearly_emissions_by_sector_as_percentage |> 
  pivot_longer(!year, names_to = "sector", values_to = "nox_emissions" ) 


ggplot(yearly_emissions_by_sector_as_percentage_long, aes(x = year, y = (nox_emissions * 100), fill = sector, colour = sector)) +
  geom_line() +
  geom_point() +
  #geom_smooth(fill= "grey") +
  scale_y_continuous(name = "% of Total Emissions", limits = c(0, 50), expand = c(0,0)) +
  scale_x_continuous(name = "Year", limits = c(1990,2022), expand = c(0,0), breaks = seq(1990,2020, by = 5)) +
  scale_colour_brewer(name = "Sector", palette = "Dark2", labels = c("Energy", "Manufacturing and Construction", "Non Road Transport", "Other Total", "Road Transport")) +
  scale_fill_brewer(name = "Sector", palette = "Dark2", labels = c("Energy", "Manufacturing and Construction", "Non Road Transport", "Other Total", "Road Transport")) +
  #ggtitle("Change in emissions contributions from the Top 4 Polluting Sectors") +
  #labs(subtitle = "% of Total Emissions measured annually from 1990 to 2021") +
  mypackage::theme_lucy() + 
  theme(legend.position = c(0.99, 0.99), 
        legend.justification = c("right", "top"),
        legend.background = element_rect(fill = "white"), 
        legend.text = element_text(size = 14), 
        legend.title = element_text(face = "bold")) +
  theme(panel.grid.minor.y = element_line(linewidth = 0.5, colour = "lightgrey"), 
        panel.grid.major.y = element_line(linewidth = 0.5, colour = "lightgrey"), 
        panel.grid.major.x = element_blank(), 
        plot.margin = margin(l = 0.25, r = 0.5, t = 0.25, unit = "cm")) 

ggsave(filename = "plots/intro_plots/percentage_changes.png", device = "png", height = 8.27, width = 11.69, units = "in")


# Would also be good to look at which are the biggest movers when comparing 1990 to 2021
# Produce a bar chart of percentage shares 1990 and 2021 

# This dataset is just retaining the figures for the years 1990 and 2021 - Do 2019 as pre-pandemic? 

data_1990_2021 <- read.csv("data/intro_plots/nox_by_sector_1990_2021_percentage.csv") |> 
  mutate(year = as.Date(paste0(year, "-01-01 00:00"))) |> 
  mutate(year = year(year)) |> 
  filter(year == 1990 | year == 2021) |> # And then make it longer 
  mutate(other_total = other_small_stationary_combustion_and_non_road_mobile_sources_and_machinery 
         + fugitive_emissions 
         + industrial_processes
         + agriculture 
         + waste
         + other)  |> 
  select(year, road_transport, energy, manufacturing_industries_and_construction, non_road_transport, other_total) |> 
  pivot_longer(!year, names_to = "sector", values_to = "nox_emissions") 


ggplot(data_1990_2021) +
  geom_col(aes(x = year, y = (nox_emissions*100), fill = sector), position = "dodge") +
  theme(legend.position = "bottom") +
  scale_fill_brewer(palette= "Pastel2") +
  scale_x_continuous(name = "Year", labels = c("1990", "2021"), breaks = c(1990, 2021), expand = c(0,0)) +
  scale_y_continuous(name = "Percentage contribution to total NOx emissions", expand = c(0,0), limits = c(NA, 45)) +
  theme_classic() +
  theme(legend.position = c(0.98, 0.98), 
        legend.justification = c("right", "top"), 
        legend.text = element_text(size = 8), 
        legend.title = element_text(face = "bold"), 
        panel.grid.minor.y = element_line(linewidth = 0.5, colour = "grey"),
        panel.grid.major.y = element_line(linewidth = 0.5, colour = "grey"))


data_1990_2021_test <- data_1990_2021 |> 
  mutate(nox_emissions = ifelse(year == 1990, nox_emissions * 1, nox_emissions)) |> 
  mutate(sector = as.factor(sector))


ggplot(data_1990_2021_test) +
  geom_col(aes(x = fct_reorder(sector, - nox_emissions), y = (nox_emissions*100), fill = sector, alpha = year), position = "dodge") +
  scale_alpha_binned(range= c(0.4, 0.8), guide = FALSE) +
  scale_y_continuous(name = "% of Total NOx Emissions", limits = c(-50, 50), expand = c(0,0), 
                     breaks = seq(-50, 50, by = 10), 
                     labels = c(50,40,30,20,10,0,10,20,30,40,50)) +
  geom_hline(yintercept = 0, linewidth = 0.5) +
  geom_text(aes(x = sector, y = nox_emissions, label = abs(round(as.numeric(nox_emissions*100), 1))), vjust = (-65  * data_1990_2021_test$nox_emissions)) + 
  theme_classic() +
  annotate(geom = "label", x = 3, y = -40, label = "1990", size = 6) +
  annotate(geom = "label", x = 3, y = 40, label = "2021", size = 6) +
  theme(axis.ticks.x = element_blank()) +
  theme(axis.text.x = element_blank(), axis.title.x = element_blank()) +
  theme(axis.title.y = element_text(face = "bold"))



# ------ Absolute Values of Emission Changes ----- 

test_b <- yearly_emissions_by_sector |> 
  filter(year == 1990 | year == 2021) |> # Filter for the years required
  mutate(other_total = other_small_stationary_combustion_and_non_road_mobile_sources_and_machinery 
         + fugitive_emissions 
         + industrial_processes
         + agriculture 
         + waste
         + other)  |>  # Create an other total category
  select(year, road_transport, energy, manufacturing_industries_and_construction, non_road_transport, other_total) |> # Select what to keep 
  pivot_longer(!year, names_to = "sector", values_to = "nox_emissions") |> 
  mutate(sector = as.factor(sector)) # Convert sector to factor variable



ggplot(test_b) +
  geom_col(aes(x = fct_reorder(sector, - nox_emissions), y = as.numeric(nox_emissions), fill = as.factor(year), group = year), position = "dodge") +
  scale_fill_brewer(name = "Year", palette = "Accent") +
  scale_x_discrete(name = "Sector", labels = str_wrap(c("Road Transport", "Energy", "Manufacturing and Construction", "Other", "Non-Road Transport"), 20 ))  +
  scale_y_continuous(name = "NOx Emissions (Tg)", expand = c(0,0), limits = c(0, 1200)) +
  ggtitle("NOx Emissions from the Top 5 Polluting Sectors") +
  labs(subtitle = "Tg of NOx Emissions for 1990 and 2021 - Source: GOV.UK") +
  theme_lucy()

ggsave(filename = "plots/intro_plots/absolute_contribution_total_nox.png", device = "png", height = 8.27, width = 11.69, units = "in")


ggplot(data_1990_2021_test) +
  geom_col(aes(x = fct_reorder(sector, - nox_emissions), y = (as.numeric(nox_emissions) * 100), fill = as.factor(year), group = year), position = "dodge") +
  scale_fill_brewer(name = "Year", palette = "Accent") +
  scale_x_discrete(name = "Sector", labels = str_wrap(c("Road Transport", "Energy", "Manufacturing and Construction", "Other", "Non-Road Transport"), 20 ))  +
  scale_y_continuous(name = "NOx Emissions (% of total)", expand = c(0,0), limits = c(0, 50)) +
  ggtitle(" Relative contributions to NOx Emissions") +
  labs(subtitle = "% of total share for 1990 and 2021 from the top 4 sectors, with other sources combined into 'Other'") +
  theme_lucy()

ggsave(filename = "plots/intro_plots/percent_contribution_total_nox.png", device = "png", height = 8.27, width = 11.69, units = "in")

# ------ Plot of Vehicle emission versus number of registered vehicles -----

# Obtain the road transport data 

road_transport_emissions <- yearly_emissions_by_sector_long |> 
  filter(sector == "road_transport")


# Now read in the number of vehicles 

number_of_vehicles <- read_csv("data/intro_plots/number_of_vehicles.csv") |> 
  select("Location","Year", "Total") |> 
  filter(Location == "England, Scotland and Wales") |> 
  rename(year = Year, total = Total) |> 
  mutate(total = total * 1000)


road_transport_summary <- left_join(road_transport_emissions, number_of_vehicles, by = "year") |> 
  mutate(nox_emissions_tg = nox_emissions / 1000 * 29) |> # The 29 here acts as the correction factor 
  mutate(number_of_vehicles_million = total / 1000000) |> 
  pivot_longer(cols = c(number_of_vehicles_million, nox_emissions_tg), names_to = "variable_names", values_to = "value") 


ggplot(road_transport_summary, aes(x = year)) +
  geom_line(aes(y = value, colour = variable_names), linewidth = 0.8) +
  #geom_point(aes(y = value, colour = variable_names)) +
  scale_color_manual(values = c("#D81E5B","darkblue"), labels = c("NOx Emissions", "Licensed Vehicles")) +
  scale_x_continuous(name = "Year", breaks = seq(1990,2020, by = 5)) +
  scale_y_continuous(name = "Millions of licensed vehicles in the UK", limits = c(0,40), expand = c(0,0),  
                     breaks = seq(0,40, by = 10), 
                     sec.axis = sec_axis(transform =~. / 30, name="Road Transport NOx Emissions (Tg)")) +
  rsc_theme_half_spacing() +
  theme(axis.title = element_text(size = 6), 
        legend.position = "top") +
  theme(axis.title.y.right = element_text(hjust = 0.65, vjust = 2))



ggsave(filename = "road_transport_plot.png", path = "plots/intro_plots/", device = "png", height = 83, width = 83, units = "mm")



# And the emissions by sector data from DEFRA..... 




yearly_emissions_by_sector_latest_update <- read_csv("data/intro_plots/yearly_nox_emissions_from_DEFRA_2022_update.csv") |>
  pivot_longer(cols = c("1990":"2022"), names_to = "year", values_to = "emission") |>
  mutate(Source = ifelse(Source %in% c("Fugitive Emissions from Fuels", "Waste", "Industrial Processes and Product Use", "Military Aircraft and Naval Shipping"), "Other", Source)) |> # Combining these other factors into Other
  group_by(year,Source) |> 
  summarise(emission = sum(emission)) |> # Summarise the new categories
  mutate(
    year = as.numeric(year),
    source = reorder(`Source`, -emission, sum))    # Reordering the factor levels based on emissions



ggplot(yearly_emissions_by_sector_latest_update) +
  geom_line(aes(x = year, y = emission, colour = source), linewidth = 0.75) +
  geom_point(aes(x = year, y = emission, colour = source), size = 0.5) +
  scale_y_continuous(name = "Relative NOx Emissions (%)", expand = c(0,0), limits = c(0,45)) +
  scale_x_continuous(name = "Year", expand = c(0,0.25), breaks = seq(1990,2020, by = 5)) +
  scale_colour_manual(name = "Emission Source", values = c(road_transport_colour, ied_colour, mcp_colour, NRMM_colour, "purple", ecodesign_colour, "darkgreen", ship_colour)) +
  #scale_colour_viridis_d(name = "Emissions Source") +
  rsc_theme_full_spacing() +
  theme(legend.margin = margin(t = -10), 
        plot.margin = margin(0,0.1,0,0, unit = "cm"))


ggsave(filename = "plots/intro_plots/overall_nox_trend_1970_2022_by_sectors.png", device = "png", height = 10.7, width = 17.1, units = "cm")




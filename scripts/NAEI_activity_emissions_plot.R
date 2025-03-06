# NAEI Plots 

# Data has been taken from the NAEI Emissions data selector - https://naei.energysecurity.gov.uk/data/data-selector?view=air-pollutants

raw_data <- read_csv("data/NAEI Activity Emissions/nox_by_NFR_code_with_activity.csv") |> 
  rename(pollutant_or_activity = "Gas", NFR_group = "NFR/CRF Group") |> 
  pivot_longer(cols = "1990":"2022", names_to = "year", values_to = "value") |> 
  mutate(year = as.Date(paste0(year, "-01-01")), value = as.numeric(value), overarching_sector = substr(NFR_group, 1, 3)) |> 
  group_by(NFR_group, `Source`, `Activity` ) |> 
  pivot_wider(names_from = pollutant_or_activity, values_from = c(value, Units))  |> 
  rename(nox_emissions_kilotonnes = `value_Nitrogen oxides (NOx expressed as NO2)`, activity_TJ = `value_Activity Data` ) |> 
  mutate(g_kwh = (nox_emissions_kilotonnes * 1000000000) / (activity_TJ * 277800)) # Performing conversion from grams per giga joule to grams per kwh


# Now looking at just data from 2022 (the most recent available)....


data_from_2022 <- raw_data |> 
  filter(year == "2022-01-01") |> 
  #filter(substr(overarching_sector, 1 ,2) == "1A") |> 
  mutate(kilotonnes_per_TJ = nox_emissions_kilotonnes/activity_TJ)


# There are lots of different source catergories which makes it tricky to understand! 
ggplot(data_from_2022) +
  geom_point(aes(y = g_kwh, x = activity_TJ,  size = nox_emissions_kilotonnes)) +
  scale_x_continuous(name = "Activity (TJ)", trans = "log10") +
  scale_y_continuous(name = "NOx (g kwh-1)") +
  theme_minimal() +
  theme(legend.position = "none") 




# Now sort out the grouping of the sectors to reduce the number of points..... 
# Have assigned groupings manually in excel

new_groupings <- read_csv("data/NAEI Activity Emissions/alternative_NFR_groupings.csv")



joined_data <- raw_data |> 
  left_join(new_groupings, by = "Source", relationship = "many-to-many")


write.csv(joined_data, "data/NAEI Activity Emissions/joined_data.csv")



regrouped_data <- joined_data |> 
  filter(year == "2022-01-01") |> 
  #filter(activity_TJ != 0) |> # Lots of sources where the activity is quoted as 0
  #filter(is.na(nox_emissions_kilotonnes) == F) |> # Also sources where we have activity but no Emissions 
  group_by(major_group) |> 
  summarise(total_nox = sum(nox_emissions_kilotonnes, na.rm = T), total_activity = sum(activity_TJ, na.rm = T)) |> 
  mutate(overall_g_kwh = (total_nox * 1000000000) / (total_activity * 277800)) |> 
  # Now will remove if inf or NA 
  filter(total_activity != 0) |> 
  filter(total_nox != 0)



regrouped_data_long <- regrouped_data |> 
  mutate(total_activity_PJ= total_activity / 1000) |> 
  mutate(total_nox = total_nox * -10) |> 
  mutate(major_group = fct_reorder(major_group, total_activity, .desc = F)) |> 
  pivot_longer(cols = c(total_nox, total_activity_PJ), names_to = "y_variable", values_to = "y_values") |>  
  mutate(y_value_labels = ifelse(sign(y_values) == -1, y_values/10, y_values)) 



# Comparing the Activity to the Emissions 


b <- ggplot(regrouped_data_long) +
  geom_col(aes(x = major_group, y=y_values, fill = y_variable, colour = y_variable)) +
  geom_text(aes(x = major_group, y=y_values, label = signif(abs(y_value_labels), 2), hjust = -1. * sign(y_values) + 0.5),  colour = "black", size = 2) +
  scale_x_discrete(name = "", labels = label_wrap(30), position = "top") +
  scale_y_continuous(name = "Total Activity (PJ)", 
                     sec.axis = sec_axis(~. / -10, 
                                         name = "Total Emission of NOx (kilotonnes)",
                                         breaks = seq(0,300, by = 50)), 
                     limits = c(-3000,2200), breaks = seq(0,2500, by = 500)) +
  scale_colour_manual(name = "", values = c("#9AD2CB", "#9E768F"), labels = c("Activity", "Emissions")) +
  scale_fill_manual(name = "", values = c("#9AD2CB", "#9E768F"), labels = c("Activity", "Emissions")) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, size = 5),
        axis.title.x = element_text(hjust = 0.5, size = 8),
        axis.text.y = element_blank(),
        panel.grid.major.y = element_line(colour = "lightgrey", linewidth = 0.5)) +
  theme(legend.position= c(0.2,0.5), 
        legend.title = element_blank(), 
        plot.margin = margin(r=-5,l = 2, unit = "mm"))



ggsave(filename = "plots/paper_plots/bar_plot_activity_emissions_ordered_by_emissions.png",  device = "png", height = 150, width = 171, units = "mm")



# And now producing a plot that includes the nox in g_kwh 



data_for_SI_plot <- regrouped_data |> 
  mutate(major_group = fct_reorder(major_group, total_nox, .desc = F))

mycolours = c(RColorBrewer::brewer.pal(name="Accent", n = 8), RColorBrewer::brewer.pal(name="Set2", n = 6))

a <- ggplot(data_for_SI_plot) +
  geom_col(aes(x=major_group, y = overall_g_kwh, fill = major_group)) +
  geom_text(aes(x = major_group, y=overall_g_kwh, label = signif(abs(overall_g_kwh), 2), hjust = overall_g_kwh - (1.2 * overall_g_kwh)), colour = "black", size = 2) +
  scale_x_discrete(name = "", labels = label_wrap(20)) +
  scale_y_continuous( name = expression("NOx (g kWh"^{-1}*")"), expand = c(0,0), limits = c(0,8), breaks = seq(0,8, by = 1)) +
  scale_fill_manual(values = mycolours) +
  coord_flip() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, size = 5),
        axis.text.y = element_text(angle = 0, hjust = 0.5, size = 6),
        axis.title = element_text(hjust = 0.5, size = 8) ,
        panel.grid.major.y = element_line(colour = "lightgrey", linewidth = 0.5)) +
  theme(legend.position= "none", 
        legend.title = element_blank(), 
        plot.margin = margin(r=5, l = 0, unit = "mm"))



ggsave(filename = "plots/supp_info/bar_plot_activity_emissions_by_g_kwh.png",  device = "png", height = 150, width = 171, units = "mm")


# And making into a panel plot to  show both things



panel_plot <- ggarrange(b,a, nrow = 1, labels = c("a", "b"), widths = c(2,1), label.args = list(gp = grid::gpar(font = 1, cex = 1.2)))



ggsave(plot = panel_plot, filename = "plots/paper_plots/bar_plot_activity_emissions_combined.png",  device = "png", height = 160, width = 266, units = "mm")




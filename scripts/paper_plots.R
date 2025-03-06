grouped_data <- collated_data |> 
  filter(Directive != "NA") |>
  group_by(`Fuel`) 

# ----- Plot the data -------


ggplot(grouped_data) +
  geom_hline(yintercept = 0.01, linewidth = 0.2, colour = "#636363") +
  geom_hline(yintercept = 0.1, linewidth = 0.2, colour = "#636363") +
  geom_hline(yintercept = 1, linewidth = 0.2, colour = "#636363") +
  geom_hline(yintercept = 10, linewidth = 0.2, colour = "#636363") +
  geom_point(aes(x = `Power (kW)`, y = `NOx (mg/kWh)`/ 1000, colour = `Directive`, shape = `Directive`, Product = `Product`, Fuel = `Fuel`), size = 1.5) +
  scale_x_continuous(name = "Power", trans= 'log10', 
                     labels = c("1 kW", "10 kW", "100 kW", "1 MW", "10 MW", "100 MW", "1 GW"),
                     breaks = c(1, 10,100,1000,10000,100000,1000000)) +
  scale_colour_manual(values = c(ecodesign_colour, road_transport_colour, plane_colour, ied_colour, ship_colour, mcp_colour, NRMM_colour)) +
  scale_y_continuous(trans = "log10",  breaks = c(0.01, 0.02, 0.03, 0.05, 0.1, 0.2, 0.3, 0.5, 1,2,3, 5, 10), limits = c(0.01,10), 
                     name = expression(bold("NOx (g kWh"^{-1}*")"))) +
  scale_shape_manual(values = c(15,16,3,17,4,18,8)) +
  ggrepel::geom_text_repel(aes(x = `Power (kW)`, 
                               y = `NOx (mg/kWh)`/ 1000, 
                               label = str_wrap(str_replace_all(`Product`, "/", ",\n"), 10)), 
                           max.overlaps = 12, 
                           min.segment.length = 0.01, size = 1.65, 
                           segment.size = 0.2, segment.curvature = 0.15) +
  geom_point(aes(x = `Power (kW)`, y = `NOx (mg/kWh)`/ 1000, colour = `Directive`, shape = `Directive`, Product = `Product`, Fuel = `Fuel`), size = 1.5) +
  rsc_theme_full_spacing() +
  theme(legend.margin = margin(t = -10, r = 0, b = 0), 
        legend.direction = "horizontal", 
        legend.text = element_text(size = 14), 
        panel.grid.minor.y = element_blank(), 
        panel.grid.major.x = element_line(linewidth = 0.1, colour = "lightgrey")) +
  guides(colour = guide_legend(override.aes = list(size=4), nrow = 1),  shape = guide_legend(nrow = 1)) +
  annotation_logticks()



ggsave(filename = "full_graph_log_y.png", path = "plots/paper_plots/", device = "png", height = 171, width = 266, units = "mm")
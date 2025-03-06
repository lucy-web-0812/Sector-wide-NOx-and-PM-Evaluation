# Initialisation Script 


library(tidyverse)
library(readxl)
library(lubridate)
library(plotly)
library(ggtext)
library(readxl)
library(scales)
library(ggsci)
library(knitr)
library(ggrepel)
library(ggpubr)
library(janitor)


source("scripts/rsc_theme.R")
source("scripts/function_spiel_remover.R")

# Vague colour scheme that sticking to....


ecodesign_colour <-"#EEC643"
NRMM_colour <- "#44AF69"
plane_colour <- "#2AB7CA"
road_transport_colour <- "#D81E5B"
mcp_colour <- "#3A606E"
ied_colour <- "#593C8F"
ship_colour <- "#FF579F"

# Another theme that is used

theme_lucy <- function ()
{
  
  ggplot2::theme(plot.title = ggplot2::element_text(size = 22, face = "bold", color = "#222222"),
                 plot.subtitle = ggplot2::element_text(size = 16,
                                                       margin = ggplot2::margin(9, 0, 9, 0)),
                 plot.caption = ggplot2::element_blank(),
                 legend.position = "top",
                 legend.text.align = 0,
                 legend.background = ggplot2::element_blank(),
                 legend.title = ggplot2::element_blank(),
                 legend.key = ggplot2::element_blank(),
                 legend.text = ggplot2::element_text(size = 18, color = "#222222"),
                 axis.line = ggplot2::element_line(linewidth = 1, color = "#222222"),
                 axis.title = ggplot2::element_text(size = 18, color = "#222222", face = "bold"),
                 axis.text = ggplot2::element_text(size = 16, color = "#222222"),
                 axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, b = 10)),
                 axis.ticks = ggplot2::element_line(colour = "black"),
                 panel.grid.minor.x = ggplot2::element_blank(),
                 panel.grid.minor.y = ggplot2::element_line(color = "#cbcbcb"),
                 panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"),
                 panel.grid.major.x = ggplot2::element_line(color = "#cbcbcb"),
                 panel.background = ggplot2::element_blank(),
                 strip.background = ggplot2::element_rect(fill = "white"),
                 strip.text = ggplot2::element_text(size = 22, hjust = 0))
}


# Initialising the dataframe where the collated data will be kept...... 


collated_data <- data.frame()


collated_data$Product <- as.character()
collated_data$Directive <- as.character()
collated_data$Fuel <- as.character()
collated_data$`Power (kW)` <- as.numeric()
collated_data$`NOx (mg/kWh)` <- as.numeric()

 
source("scripts/1_boilers.R")
source("scripts/2_euro_emissions.R")
source("scripts/3_ICAO_aviation.R")
source("scripts/4_IED_and_MCP.R")
source("scripts/5_1_NRMM.R")
source("scripts/5_2_NRMM_NRE_standards.R")
source("scripts/5_3_trains.R")
source("scripts/6_MARPOL.R")

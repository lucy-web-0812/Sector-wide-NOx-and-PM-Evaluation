
rsc_theme_half_spacing <- function() {
  
  theme(axis.line = ggplot2::element_line(linewidth = 1, color = "#222222"),
        axis.title = ggplot2::element_text(size = 12, color = "#222222", face = "bold"),
        axis.text = ggplot2::element_text(size = 7, color = "#222222"),
        axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, b = 10)),
        axis.ticks = ggplot2::element_line(colour = "black"),
        panel.grid.minor.x = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_line(color = "#cbcbcb"),
        panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(), 
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = ggplot2::element_blank(),
        legend.text = ggplot2::element_text(size = 8, color = "#222222"),
  )
}
  
  

rsc_theme_full_spacing <- function(){
  theme(axis.line = ggplot2::element_line(linewidth = 1, color = "#222222"),
        axis.title = ggplot2::element_text(size = 12, color = "#222222", face = "bold"),
        axis.text = ggplot2::element_text(size = 12, color = "#222222"),
        axis.text.x = ggplot2::element_text(margin = ggplot2::margin(5, b = 10)),
        axis.ticks = ggplot2::element_line(colour = "black"),
        panel.grid.minor.x = ggplot2::element_blank(),
        panel.grid.minor.y = ggplot2::element_line(color = "#cbcbcb"),
        panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb"),
        panel.grid.major.x = ggplot2::element_blank(),
        panel.background = ggplot2::element_blank(), 
        legend.title = element_blank(),
        legend.position = "bottom",
        legend.background = ggplot2::element_blank(),
        legend.text = ggplot2::element_text(size = 8, color = "#222222"),
        
        
  )
}
  


  
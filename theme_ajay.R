theme_ajay <- function () {
  theme_minimal(base_size=12, base_family="Avenir") %+replace% 
    theme(
      plot.background = element_rect(fill ="#B1D4E0", color = "#B1D4E0"),
      panel.grid = element_blank(),
      plot.title = element_text(hjust = 0.5, size = 17, color = "#145DA0", face = "bold", margin=margin(2,0,3,0)),
      axis.text = element_text(colour = "#145DA0"),
      legend.position = "right",
      legend.background = element_rect(fill = '#B1D4E0', color = '#B1D4E0'),
      legend.box.background = element_rect(fill = '#B1D4E0', color = '#B1D4E0'),
      #legend.box.margin = margin(6, 6, 6, 6),
      plot.subtitle = element_text(hjust = 0.5, color = "#0C2D48", margin=margin(0,0,3,0), size = 11),
      plot.caption = element_text(color = "#0C2D48", hjust = 1, size = 11),
      legend.text = element_text(color = "#145DA0"),
      legend.title = element_text(color = "#145DA0"),
      axis.ticks.length = unit(0.3, "cm"),
      axis.ticks = element_line(color = "#B1D4E0"),
      axis.text.x = element_text(angle = 0),
      axis.title = element_text(hjust = 0.5, size = 14, color = "#145DA0", face = "bold"),
      panel.border = element_rect(colour = "#B1D4E0", fill=NA, size=1.5),
      strip.text.x = element_text(colour = "#145DA0", face = "bold"),
      panel.spacing = unit(1, "lines"),
      strip.background = element_rect(fill = "#B1D4E0", size = 1),
    )
}

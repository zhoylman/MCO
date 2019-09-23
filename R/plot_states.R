library(sf)
library(dplyr)
library(ggplot2)

states = st_read("/home/zhoylman/MCO/shp/states.shp")

states_conus = states %>%
  filter(!(STATE_ABBR %in% c("AK", "HI")))

states_data = states %>%
  filter(STATE_ABBR %in% c("CO", "IL", "IN", "IA", "KS", "KY",
                           "MI", "MN", "MO", "NE", "NC", "ND", 
                           "OH", "PA", "SD", "TN", "TX", "WI"))

states_data$test = as.numeric(states_data$STATE_FIPS)

plot_states = function(fill_data, variable, color_ramp, domain, color_label, title){
  plot = ggplot(data = states_conus)+
    geom_sf()+
    geom_sf(data = fill_data, aes_string(fill = variable))+
    geom_sf_text(data = fill_data, aes_string(label = variable))+
    scale_fill_gradientn(color_label, colours = color_ramp, limits = domain)+
    theme_bw(base_size = 16)+
    ggtitle(title)+
    theme(plot.title = element_text(hjust = 0.5))
    
  return(plot)
} 

color_ramp = c("#ff0000", "#ffff00", "#ffffff", "#00ff00", "#0000ff")
color_label = "Test"
range = c(0,100)
title = "This is the Title"
plot_states(states_data, "test", color_ramp, range, color_label, title)
ggsave("/home/zhoylman/MCO/plots/test_plot.png", width = 6, height = 6, units = "in", dpi = 300)

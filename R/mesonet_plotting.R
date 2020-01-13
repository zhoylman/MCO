library(RCurl)
library(dplyr)
library(tidyverse)
library(tictoc)
library(plotly)
library(data.table)

test = getURL("https://mesonet.climate.umt.edu/api/observations?latest=false&start_time=2020-01-06&end_time=2020-01-10&tz=US%2FMountain&wide=false&type=csv")

tic()
get_current = getURL("https://mesonet.climate.umt.edu/api/latest?tz=US%2FMountain&wide=false&type=csv") %>%
  read_csv()
toc()

data = get_current %>%
  dplyr::filter(station_key == "arskeogh")

plots = list()

names_str = c("sol_radi", "air_temp", "rel_humi", "wind_spd")
col = c('red', 'green', 'black', "darkgrey")
ylab = c("Solar Radiation\n(W/m2)", "Air Temperature\n(°C)", "Relative Humidity\n(%)", "Wind Speed\n(m/s)")

simple_plotly = function(data,name_str,col,ylab){
  data %>%
    select(name, value, datetime, units) %>%
    dplyr::filter(name == name_str) %>%
    transform(id = as.integer(factor(name))) %>%
    plot_ly(x = ~datetime, y = ~value, color = ~name, colors = col, showlegend=F, 
            yaxis = ~paste0("y", id)) %>%
    layout(yaxis = list(
      title = paste0(ylab)))%>%
    add_lines()
}

stations = unique(get_current$station_key)

for(i in 1:length(names_str)){
  plots[[i]] = simple_plotly(data,names_str[i], col[i], ylab[i])
}

vwc = data %>%
  dplyr::filter(name %like% "soilwc") %>%
  plot_ly(x = ~datetime, y = ~value,  colors = "black", name = ~name, type = 'scatter', mode = 'lines', showlegend=T) %>%
  layout(yaxis = list(
    title = paste0("Volumetric Water Content\n(m3/m3)")))%>%
  add_lines() 

# annotations
a <- list(
  text = "arskeogh",
  xref = "paper",
  yref = "paper",
  yanchor = "bottom",
  xanchor = "center",
  align = "center",
  x = 0.5,
  y = 1,
  showarrow = FALSE
)

final = subplot(plots[[1]], plots[[2]], plots[[3]], plots[[4]], vwc, nrows = 5, shareX = T, titleY = T, titleX = T) %>%
  layout(annotations = a)%>%
  layout(legend = list(x = 100, y = 0.1))

# 
# for(i in 1:10){
#   for(i in 1:length(names_str)){
#     plots[[i]] = simple_plotly(data,names_str[i], col[i], ylab[i])
#   }
#   
#   vwc = data %>%
#     dplyr::filter(name %like% "soilwc") %>%
#     plot_ly(x = ~datetime, y = ~value,  colors = "black", name = ~name, type = 'scatter', mode = 'lines', showlegend=T) %>%
#     layout(yaxis = list(
#       title = paste0("Volumetric Water Content\n(m3/m3)")))%>%
#     add_lines() 
#   
#   # annotations
#   a <- list(
#     text = "arskeogh",
#     xref = "paper",
#     yref = "paper",
#     yanchor = "bottom",
#     xanchor = "center",
#     align = "center",
#     x = 0.5,
#     y = 1,
#     showarrow = FALSE
#   )
#   
#   final = subplot(plots[[1]], plots[[2]], plots[[3]], plots[[4]], vwc, nrows = 5, shareX = T, titleY = T, titleX = T) %>%
#     layout(annotations = a)%>%
#     layout(legend = list(x = 100, y = 0.1))
#   
#   htmlwidgets::saveWidget(final, paste0("~/MCO/data/mesonet/",stations[i],"_test.html"), selfcontained = F, libdir = "~/MCO/data/mesonet/libs")
# }

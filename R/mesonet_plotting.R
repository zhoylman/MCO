library(RCurl)
library(dplyr)
library(tidyverse)
library(tictoc)
library(plotly)
library(data.table)

#get current frame to plot
time = data.frame(current = Sys.time() %>%
                            as.Date()) %>%
  mutate(start = current - 14)

#retrieve the curent station list
stations = getURL("https://mesonet.climate.umt.edu/api/stations?type=csv&clean=true") %>%
  read_csv()

#define simple plotting fuctions
simple_plotly = function(data,name_str,col,ylab,conversion_func){
  data %>%
    dplyr::filter(name == name_str) %>%
    mutate(value = conversion_func(value)) %>%
    transform(id = as.integer(factor(name))) %>%
    plot_ly(x = ~datetime, y = ~value, color = ~name, colors = col, showlegend=F, 
            yaxis = ~paste0("y", id)) %>%
    layout(yaxis = list(
      title = paste0(ylab)))%>%
    add_lines()
}

#define inputs
names_str = c("sol_radi", "air_temp", "rel_humi", "wind_spd")
col = c('red', 'green', 'black', "darkgrey")
ylab = c("Solar Radiation\n(W/m<sup>2</sup>)", "Air Temperature\n(°F)", "Relative Humidity\n(%)", "Wind Speed\n(ft/s)")
conversion_func = list(function(x){return(x)}, 
                       function(x){return((x * 9/5)+32)},
                       function(x){return(x)}, 
                       function(x){return(x * 3.28084)})

#loop though stations
for(s in 1:11){
  tic()
  url = paste0("https://cfcmesonet.cfc.umt.edu/api/observations?stations=",stations$`Station ID`[s], "&latest=false&start_time=",
               time$start, "&end_time=", time$current+1, "&tz=US%2FMountain&wide=false&type=csv")
  
  data = getURL(url) %>%
    read_csv() %>%
    mutate(datetime = datetime %>%
             lubridate::with_tz("America/Denver")) %>%
    select(name, value, datetime, units)
  
  plots = list()
  
  for(i in 1:length(names_str)){
    plots[[i]] = simple_plotly(data, names_str[i], col[i], ylab[i], conversion_func[[i]])
  }
  
  precip = data %>%
    dplyr::filter(name %like% "precipit") %>%
    plot_ly(x = ~datetime, y = ~value,  colors = "black", name = ~name, type = 'bar', showlegend=F) %>%
    layout(yaxis = list(
      title = paste0("Precipitation\n(mm)")))
  
  vwc = data %>%
    dplyr::filter(name %like% "soilwc") %>%
    mutate(name = name %>%
             str_extract(., "(\\d)+") %>%
             as.numeric() %>%
             paste0(., " in")) %>%
    mutate(value = value * 100) %>%
    plot_ly(x = ~datetime, y = ~value,  colors = "black", name = ~name, type = 'scatter', mode = 'lines', showlegend=T) %>%
    layout(yaxis = list(
      title = paste0("Volumetric Water Content\n(%)")))
  
  # annotations
  a <- list(
    text = paste0(stations$`Station name`[s], " (", round((stations$`Elevation (masl)`[s] * 3.28084),0), " ft elevation)"),
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
    layout(legend = list(x = 100, y = 0.1),
           xaxis = list(
             title = "Time"
           ))
  
  htmlwidgets::saveWidget(final, paste0("~/MCO/data/mesonet/",stations$`Station ID`[s],"_current_data.html"), selfcontained = F, libdir = "~/MCO/data/mesonet/libs")
  print(s)
  toc()
}

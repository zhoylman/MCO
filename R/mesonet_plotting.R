library(RCurl)
library(dplyr)
library(tidyverse)
library(tictoc)
library(plotly)
library(data.table)

tic()
get_current = getURL("https://mesonet.climate.umt.edu/api/observations?latest=false&start_time=2020-01-06&end_time=2020-01-10&tz=US%2FMountain&wide=false&type=csv") %>%
  read_csv()
toc()

test = get_current %>%
  dplyr::filter(station_key == "arskeogh")

p0 = test %>%
  select(name, value, datetime) %>%
  dplyr::filter(name == "sol_radi") %>%
  transform(id = as.integer(factor(name))) %>%
  plot_ly(x = ~datetime, y = ~value, color = ~name, colors = "orange",
          yaxis = ~paste0("y", id)) %>%
  add_lines() 

p1 = test %>%
  select(name, value, datetime) %>%
  dplyr::filter(name == "air_temp") %>%
  transform(id = as.integer(factor(name))) %>%
  plot_ly(x = ~datetime, y = ~value, color = ~name, colors = "red",
          yaxis = ~paste0("y", id)) %>%
  add_lines() 

p2 = test %>%
  select(name, value, datetime) %>%
  dplyr::filter(name == "rel_humi") %>%
  transform(id = as.integer(factor(name))) %>%
  plot_ly(x = ~datetime, y = ~value, color = ~name, colors = "purple",
          yaxis = ~paste0("y", id)) %>%
  add_lines() 

p3 = test %>%
  select(name, value, datetime) %>%
  dplyr::filter(name == "wind_spd") %>%
  transform(id = as.integer(factor(name))) %>%
  plot_ly(x = ~datetime, y = ~value, color = ~name, colors = "blue",
          yaxis = ~paste0("y", id)) %>%
  add_lines() 

p4 = test %>%
  dplyr::filter(name %like% "soilwc") %>%
  plot_ly(x = ~datetime, y = ~value,  colors = "black", name = ~name, type = 'scatter', mode = 'lines') %>%
  add_lines() 



subplot(p0,p1,p2,p3,p4, nrows = 5, shareX = T)

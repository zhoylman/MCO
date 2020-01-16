library(RCurl)
library(dplyr)
library(tidyverse)
library(tictoc)
library(plotly)
library(data.table)
library(doParallel) 
library(htmltools)
library(htmlwidgets)
library(knitr)
library(kableExtra)

# httr::GET(url = "https://cfcmesonet.cfc.umt.edu/api/latest", 
#           query = list(stations=c("arskeogh"),type = "csv")) %>%
#   httr::content()

#get current frame to plot
time = data.frame(current = Sys.time() %>% as.Date()) %>%
  mutate(start = current - 14)

#retrieve the curent station list
stations = getURL("https://cfcmesonet.cfc.umt.edu/api/stations?type=csv&clean=true") %>%
  read_csv()

#retrieve the lastest data (last time signiture)
latest = getURL("https://cfcmesonet.cfc.umt.edu/api/latest?tz=US%2FMountain&wide=false&type=csv")%>%
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
col = c('red', 'green', 'black', "orange")
ylab = c("Solar Radiation\n(W/m<sup>2</sup>)", "Air Temperature\n(°F)", "Relative Humidity\n(%)", "Wind Speed\n(ft/s)")
conversion_func = list(function(x){return(x)}, 
                       function(x){return((x * 9/5)+32)},
                       function(x){return(x)}, 
                       function(x){return(x * 3.28084)})

#loop though stations
cl = makeCluster(detectCores()-1)
registerDoParallel(cl)

#
clusterCall(cl, function() {lapply(c("RCurl", "dplyr", "tidyverse", "plotly",
                                     "data.table", "tidyverse", "htmltools",
                                     "htmlwidgets", "knitr", "kableExtra"), library, character.only = TRUE)})

start = Sys.time()
foreach(s=1:length(stations$`Station ID`)) %dopar% {
  source('/home/zhoylman/MCO/R/mesonet_dynamic_rmd.R')
  
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
    plot_ly(x = ~datetime, y = ~value, name = ~name, color = ~name, showlegend=T, colors = "Set2") %>%
    layout(yaxis = list(
      title = paste0("Soil Moisture\n(%)"))) %>%
    add_lines()
  
  temp = data %>%
    dplyr::filter(name %like% "soilt") %>%
    mutate(name = name %>%
             str_extract(., "(\\d)+") %>%
             as.numeric() %>%
             paste0(., " in")) %>%
    mutate(value = conversion_func[[2]](value)) %>%
    plot_ly(x = ~datetime, y = ~value, name = ~name, showlegend=F, color = ~name, colors = "Set2") %>%
    layout(yaxis = list(
      title = paste0("Soil Temperature\n(°F)"))) %>%
    add_lines()
  
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
  
  final = subplot(plots[[1]], plots[[2]], plots[[3]], plots[[4]], vwc, temp, nrows = 6, shareX = T, titleY = T, titleX = T) %>%
    layout(annotations = a)%>%
    layout(height = 1500) %>%
    layout(legend = list(x = 100, y = 0.1),
           xaxis = list(
             title = "Time"
           )) %>%
    saveWidget(., paste0("~/MCO/data/mesonet/station_page/current_plots/",stations$`Station ID`[s],"_current_data.html"), selfcontained = F, libdir = "./libs")
  
  ## current conditions
  latest_time = latest %>%
    filter(station_key == stations$`Station ID`[s]) %>%
    select(datetime)%>%
    mutate(datetime = datetime %>%
             lubridate::with_tz("America/Denver"))%>%
    head(1)
  
  table_current = latest %>%
    filter(station_key == stations$`Station ID`[s]) %>% 
    mutate(datetime = datetime %>%
             lubridate::with_tz("America/Denver")) %>%
    select("name", "value", "units") %>%
    rename("Name" = name,  "Value" = value, "Units" = units)%>%
    kable(., "html", caption = paste0("Latest observation was at ", latest_time[1]$datetime %>% as.character()))%>%
    kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"))%>%
    save_kable(file = paste0("~/MCO/data/mesonet/station_page/latest_table/",stations$`Station ID`[s],"_current_table.html"),  selfcontained = F)
  
  #write out final page from RMD
  mesonet_dynamic_rmd(stations$Latitude[s], stations$Longitude[s], stations$`Station ID`[s], stations$`Station name`[s])
  
  ## mobile Sandbox
  
  # data_mobile = data %>%
  #   dplyr::filter(datetime > time$current - 3)
  # 
  # mobile = plot_ly(data_mobile) %>%
  #   add_lines(x = ~datetime[name == "air_temp"], y = ~value[name == "air_temp"], name = "Air Temp", visible = T, color=I("green"), showlegend=F) %>%
  #   add_lines(x = ~datetime[name == "sol_radi"], y = ~value[name == "sol_radi"], name = "Solar Radiation", visible = F, color=I("red"), showlegend=F) %>%
  #   add_lines(x = ~datetime[name == "rel_humi"], y = ~value[name == "rel_humi"], name = "Relitive Humidity", visible = F, color=I("orange"), showlegend=F) %>%
  #   layout(autosize = T,
  #     yaxis = list(title = "y"),
  #     updatemenus = list(
  #       list(
  #         y = 1.1,
  #         x = 0.1,
  #         buttons = list(
  #           list(method = "restyle",
  #                args = list("visible", list(TRUE, FALSE, FALSE)),
  #                label = "Air Temp"),
  #           list(method = "restyle",
  #                args = list("visible", list(FALSE, TRUE, FALSE)),
  #                label = "Solar Radiation"),
  #           list(method = "restyle",
  #                args = list("visible", list(FALSE, FALSE, TRUE)),
  #                label = "Relitive Humidity")))
  #     )
  #   ) %>%
  #   htmlwidgets::saveWidget(., paste0("~/MCO/data/mesonet/station_page/current_plots/mobile_test.html"), selfcontained = F, libdir = "./libs")
}

Sys.time() - start

stopCluster(cl)

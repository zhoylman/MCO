library(leaflet)

stations = getURL("https://mesonet.climate.umt.edu/api/stations?type=csv&clean=true") %>%
  read_csv()

source("~/MCO/R/base_map.R")

map = base_map() %>% addCircleMarkers(data = stations, lat = ~Latitude, lng = ~Longitude, stroke = TRUE,
                                fillColor = "blue", fillOpacity = 0.8, color = "black", opacity = 0.8, radius = 10,
  popup = paste0('<font size="3"> ' ,stations$`Station name`,"<br> <a href='https://mco.cfc.umt.edu/mesonet_data/station_page/",stations$`Station ID`,".html'>Current Data</a> </font>"))

htmlwidgets::saveWidget(map, paste0("~/MCO/data/mesonet/station_page/simple_mesonet_map.html"), selfcontained = F, libdir = "./libs")

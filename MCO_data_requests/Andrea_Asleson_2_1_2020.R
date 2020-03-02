library(raster)
library(leaflet)
library(htmlwidgets)
library(htmltools)

mt = read_sf("~/mt-climate-data/shp/states/states.shp")%>%
  filter(STATE_ABBR == "MT")

co = read_sf("~/mt-climate-data/shp/states/states.shp")%>%
  filter(STATE_ABBR == "CO")

rh_mt = raster("/home/zhoylman/MCO/data_output/rh_mean_mt.tif") %>%
  mask(.,mt)

rh_co = raster("/home/zhoylman/MCO/data_output/rh_mean_co.tif") %>%
  mask(., co)

rh_co[rh_co >= 65] = 65
rh_co[rh_co <= 40] = 40

rh_mt[rh_mt >= 65] = 65
rh_mt[rh_mt <= 40] = 40

source("/home/zhoylman/mesonet-dashboard/R/base_map.R")

pal <- colorNumeric((c("#8b0000", "#ff0000", "#ffff00", "#ffffff", "#00ffff", "#0000ff", "#000d66")), 39:66, na.color = "transparent")

map = base_map() %>%
  addRasterImage(rh_mt, colors = pal, opacity = 0.8, project = TRUE )%>%
  addRasterImage(rh_co, colors = pal, opacity = 0.8, project = TRUE ) %>%
  addLegend(pal = pal, values = 40:65,
            title = paste0("Relative Humidity<br>Average (%)"),
            position = "bottomleft") %>%
  htmlwidgets::prependContent(tags$head(tags$meta(HTML('<meta name="viewport" content="width=device-width, initial-scale=1.0, maximum-scale=1.0, user-scalable=no" />'))))


htmlwidgets::saveWidget(map, "/home/zhoylman/MCO/data_output/rh_map.html", selfcontained = T)  
  

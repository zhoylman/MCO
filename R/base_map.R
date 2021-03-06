#define base map information as a function used for all leaflet maps
#load base map dependent data
library(rgdal)
library(dplyr)
library(leaflet)
library(leaflet.extras)
states = rgdal::readOGR("~/MCO/shp/states.shp")

#define basemap function
base_map = function(x){
  leaflet::leaflet(options = leafletOptions(zoomControl = FALSE)) %>%
    leaflet::addTiles("https://maps.tilehosting.com/data/hillshades/{z}/{x}/{y}.png?key=KZO7rAv96Alr8UVUrd4a") %>%
    leaflet::addProviderTiles("Stamen.TonerLines") %>%
    leaflet::addProviderTiles("Stamen.TonerLabels") %>%
    leaflet::setView(lng = -109.5, lat = 47, zoom = 5) %>%
    leaflet::addPolygons(data = states, group = "States", fillColor = "transparent", weight = 2, color = "black", opacity = 1)
}

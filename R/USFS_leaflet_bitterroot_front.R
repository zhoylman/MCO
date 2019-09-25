library(raster)
library(leaflet)
library(leaflet.opacity)
library(leaflet.esri)
eco_sens = raster("~/MCO/data/USFS/Ecosystem_Sensitivity_Bitteroot_Front.tif")

values(eco_sens)[values(eco_sens) > 0.4] = 0.4
values(eco_sens)[values(eco_sens) < -0.4] = -0.4

base_map = function(x){
  leaflet::leaflet(options = leaflet::tileOptions()) %>%
    leaflet::addTiles("https://maps.tilehosting.com/data/hillshades/{z}/{x}/{y}.png?key=KZO7rAv96Alr8UVUrd4a") %>%
    leaflet::addProviderTiles("Stamen.TonerLines") %>%
    leaflet::addProviderTiles("Stamen.TonerLabels") %>%
    leaflet::setView(lng = -114.232269, lat = 46.132008, zoom = 14) 
}


ramp = c('#FF4933', '#F0FF33','#33C1FF')

pal1 <- colorNumeric(ramp, domain = c(-0.4,0.4) ,na.color = "transparent")

map = base_map() %>%
  addEsriBasemapLayer(esriBasemapLayers$Imagery)%>%
  addRasterImage(eco_sens, colors = pal1, layerId = "Ecosystem Sensitivity", group = "Ecosystem Sensitivity", project = TRUE) %>%
  leaflet::addLegend(pal = pal1,
                     values = seq(-0.4,0.4,0.01),
                     title = "Ecosystem Sensitivity",
                     position = "bottomleft") %>%
  addOpacitySlider(layerId = "Ecosystem Sensitivity")

htmlwidgets::saveWidget(map, "~/MCO/data_output/USFS/ecosystem_sensitivity.html")

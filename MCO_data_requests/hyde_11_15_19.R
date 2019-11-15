library(raster)
library(ggplot2)
library(rgdal)
library(sf)
library(dplyr)

montana = st_read("/home/zhoylman/drought_indicators/shp_kml/Montana_Outline.shp")

percentiles = raster("/home/zhoylman/drought_indicators/precipitation/maps/current_percentile_90.tif") %>%
  mask(., readOGR("/home/zhoylman/drought_indicators/shp_kml/Montana_Outline.shp")) %>%
  rasterToPoints() %>%
  data.frame() %>%
  mutate(bin = .bincode(current_percentile_90, breaks = seq(0,100,10)))

map = ggplot()+
  #geom_sf(data = crop)+
  geom_tile(data = percentiles, aes(x = x, y = y, fill = current_percentile_90))+
  geom_sf(data = montana, fill = "transparent", size = 0.5, color = "black")+ 
  theme_bw(base_size = 16)+
  xlab("")+
  ylab("")+
  ggtitle("90 Day Precipitation Percentile \n 8/15/2019 - 11/13/2019")+
  scale_fill_gradientn(colours = c("darkred", "red", "yellow", "white", "cyan", "blue", "darkblue"),
                       breaks = c(seq(0,100,25)), limits = c(0,100), name = "")+
  theme(plot.title = element_text(hjust = 0.5))

map

ggsave("./plots/precip_percentiles.png", map, dpi = 599)

dev.off()

soil_moisture = raster("/home/zhoylman/Downloads/SM_perc.ens.web.tif") %>%
  mask(., readOGR("/home/zhoylman/drought_indicators/shp_kml/Montana_Outline.shp")) %>%
  rasterToPoints() %>%
  data.frame() %>%
  mutate(SM_perc.ens.web=replace(SM_perc.ens.web, SM_perc.ens.web>100, 100))
  

soil_moisture_map = ggplot()+
  #geom_sf(data = crop)+
  geom_tile(data = soil_moisture, aes(x = x, y = y, fill = SM_perc.ens.web))+
  geom_sf(data = montana, fill = "transparent", size = 0.5, color = "black")+ 
  theme_bw(base_size = 16)+
  xlab("")+
  ylab("")+
  ggtitle("Soil Moisture Percentile \n 11/14/2019")+
  scale_fill_gradientn(colours = c("darkred", "red", "yellow", "white", "cyan", "blue", "darkblue"),
                       breaks = c(seq(0,100,25)), limits = c(0,100), name = "")+
  theme(plot.title = element_text(hjust = 0.5))

soil_moisture_map

ggsave("./plots/soil_moisture_percentiles.png", soil_moisture_map, dpi = 599)

library(ncdf4)
test = download.file("https://www.cpc.ncep.noaa.gov//products/Drought/Figures/index/SM_perc.ens.web.tif", 
                     destfile = "/home/zhoylman/temp/test.tif")

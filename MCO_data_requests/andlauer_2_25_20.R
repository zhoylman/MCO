library(dplyr)
library(RCurl)
library(ggplot2)
library(sf)
library(tidyverse)
library(raster)

#api request:
#https://mesonet.climate.umt.edu/api/observations?elements=soilwc08&latest=false&start_time=2019-01-01&end_time=2019-12-31&tz=US%2FMountain&wide=false&simple_datetime=true&type=csv
# sm = getURL("https://mesonet.climate.umt.edu/api/observations?elements=soilwc08&latest=false&start_time=2019-01-01&end_time=2019-12-31&tz=US%2FMountain&wide=false&simple_datetime=true&type=csv") %>%
#   read_csv()
sm = read_csv('/home/zhoylman/MCO/MCO_data_requests/data/response_1582663990740.csv') 

stations = getURL("https://cfcmesonet.cfc.umt.edu/api/stations?type=csv&clean=true") %>%
  read_csv() %>% # read csv
  arrange(`Station ID`) # arrange the data by name

montana = read_sf('/home/zhoylman/MCO/MCO_data_requests/data/states/states.shp') %>%
  filter(STATE_ABBR %in% 'MT')

mean_sm = sm %>%
  group_by(station_key) %>% # segment dataframe into groups by station name
  summarise(mean = mean(value)) %>% # summerized values within each group
  arrange(station_key) %>% # arrange by station name
  mutate(mean = mean * 100) # modify an existing collumn

counties = read_sf('/home/zhoylman/MCO/MCO_data_requests/data/counties/county_umrb.shp') %>%
  filter(STATE_NAME == "Montana")

cities = read_sf('/home/zhoylman/MCO/MCO_data_requests/data/cities.gdb') %>%
  filter(ST == "MT")

cities$x_nudge = c(0, 0, 0, 0, 0, -0.3, 0)
cities$y_nudge = c(-0.3, -0.3, -0.3, -0.3, -0.3, 0, 0.3)

import_raster = function(x){
  temp = raster(x) %>%
    rasterToPoints() %>%
    data.frame()
  
  colnames(temp) = c("x", "y", "z")
  return(temp)
}

hillshade = import_raster("/home/zhoylman/MCO/MCO_data_requests/data/montana_hillshade.tif")

map = ggplot() + 
  geom_raster(data = hillshade,aes(y=y, x=x, alpha = -1*(z)), show.legend = F)+
  scale_alpha_continuous(range = c(0, 0.6))+
  scale_fill_gradientn(colours = c('#654321', "lightblue", "blue", 'darkblue'), name = 'VWC (%)')+
  geom_sf(data = counties,   fill = 'transparent')+
  geom_sf(data = montana, fill = 'transparent', size = 2) + 
  geom_sf(data = cities)+
  geom_sf_text(data = cities, aes(label = NAME), position = position_nudge(x = cities$x_nudge, y = cities$y_nudge),  size = 3)+
  geom_point(data = stations, aes(x = Longitude, y = Latitude, fill = mean_sm$mean), size = 4, pch = 21) +
  theme_bw(base_size = 16) +
  ggtitle('MT Mesonet Mean Soil Moisture (%)')+
  theme(legend.position = c(0.05,0.15),
        axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        plot.title = element_text(hjust = 0.5))
  
map

ggsave("/home/zhoylman/MCO/MCO_data_requests/data/mesonet_map.png", map, dpi = 300, width = 12, height = 8, units = 'in')

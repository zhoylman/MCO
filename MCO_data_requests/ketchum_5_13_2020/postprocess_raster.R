library(tidyverse)
library(spdplyr)
library(raster)
library(sf)

west = st_read('/home/zhoylman/MCO/MCO_data_requests/ketchum_5_13_2020/data/states.shp') %>%
  filter(STATE_ABBR %in% c('WA', 'ID', 'MT', 'OR', 'WY', 'CA', 'UT', 'NV', 'CO',
                           'AZ', 'NM'))

precip = '/home/zhoylman/MCO/MCO_data_requests/ketchum_5_13_2020/data/annual_precip_mean_annual_1981-2010.tif'
eto = '/home/zhoylman/MCO/MCO_data_requests/ketchum_5_13_2020/data/annual_eto_mean_annual_1981-2010.tif'

extact_min_max = function(x){
  data = raster(x) %>%
    mask(.,west) %>%
    crop(., west) %>%
    rasterToPoints() %>%
    as.data.frame()
  colnames(data) = c('lon','lat', 'z')
  max_index = which(data$z == max(data$z, na.rm = T))
  max_data = data[max_index,]
  
  min_index = which(data$z == min(data$z, na.rm = T))
  min_data = data[min_index,]
  
  return(rbind(max_data, min_data))
}

precip_min_max = extact_min_max(precip)
eto_min_max = extact_min_max(eto)


plot(data)
points(eto_min_max$lon, eto_min_max$lat)

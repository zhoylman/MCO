library(reticulate)
library(rgee)
library(cptcity)
library(raster)
library(stars)
library(sf)
library(tidyverse)

use_condaenv("gee-base", conda = "auto",required = TRUE)
ee = import("ee")
ee_Initialize(email = 'zhoylman@gmail.com', drive = TRUE)

# Define a region of interest with sf
ee_roi = st_read("/home/zhoylman/mesonet-dashboard/data/shp/states.shp") %>%
  dplyr::filter(STATE_ABBR %in% c('CA')) %>%
  st_geometry() %>%
  sf_as_ee()

gps = read_csv('/home/zhoylman/Downloads/gps_locations_RR_JPL_extend.txt') %>%
  st_as_sf(., coords = c(x = 'long', y = 'lat')) %>%
  st_set_crs(4326) %>%
  sf_as_ee()

et_ic = ee$ImageCollection('MODIS/NTSG/MOD16A2/105')$select('ET')

training = gps$sampleRegion(
  reducer = ee$Reducer$mean(),
  collection = et_ic,
  scale = 1000
)

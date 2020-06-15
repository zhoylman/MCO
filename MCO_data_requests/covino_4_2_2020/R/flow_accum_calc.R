library(RSAGA)
library(rgdal)
library(raster)
library(tictoc)
library(sf)
library(dplyr)
library(spdplyr)

#Working Directory
setwd("/home/zhoylman/MCO/MCO_data_requests/covino_4_2_2020/data/")

#Sets enviormental variables for SAGA
env <- rsaga.env(path = '/usr/bin/',
                 modules = '/usr/lib/x86_64-linux-gnu/saga/',
                 parallel = TRUE, cmd = "saga_cmd", cores = 7)

#convert WGS84 to UTM zone 13N (EPSG 32613)
source('../R/gdal_warp_custom.R')
gdal_warp(file_in = "./wgs84/raw_dem_10m_wgs84.tif",
         file_out = "./utm/temp/raw_dem_10m_UTM_raw.tif",
         proj_in = "EPSG:4326", proj_out = "EPSG:32613", res = 10)

#import outline reproject for cropping of gdalwarp 
outline = st_read('../shp/fef/fefshed.shp') %>%
  st_transform(., 32613)

# import dem from gdal warp and clip to outline of waterhsed
# gdal warp will try to interpolate values around the edges and
# outside of the watershed boundries (NAs and 0s)
dem = raster('./utm/temp/raw_dem_10m_UTM_raw.tif') %>%
  mask(., outline)

#write out dataset
writeRaster(dem, './utm/dem_10m_UTM.tif', overwrite = T)
writeRaster(dem, './utm/dem_10m_UTM.sgrd', fromat = 'SAGA', overwrite = T)

#Preprocess raster for sink removal
rsaga.geoprocessor(lib="ta_preprocessor", module = "Sink Removal", env = env,
                   param=list(DEM='./utm/dem_10m_UTM.sgrd', 
                              DEM_PREPROC = './utm/dem_10m_UTM_sinks_filled.sgrd',
                              METHOD = 1)) # fill sinks 

#Calculate Flow Accumulation accorsing to Seibert, J. / McGlynn, B. (2007) (Triangular Multiple Flow Direction)
rsaga.geoprocessor(lib="ta_hydrology", module = "Flow Accumulation (Top-Down)", env = env,
                     param=list(ELEVATION	= './utm/dem_10m_UTM_sinks_filled.sgrd', 
                                FLOW = './utm/flow_accumulation_10m_UTM.sgrd',
                                METHOD = 5))
#Export sgrd as tif
rsaga.geoprocessor(lib="io_gdal", module=2, param=list(GRIDS= './utm/flow_accumulation_10m_UTM.sgrd', 
                                                         FILE= './utm/flow_accumulation_10m_UTM.tif'), env = env)

#reproject and overwrite (crs definition is lost during saga processing)
flow_accum = raster('./utm/flow_accumulation_10m_UTM.tif') %>%
  mask(., outline) 
crs(flow_accum) = crs(dem)
writeRaster(flow_accum, './utm/flow_accumulation_10m_UTM.tif', overwrite = T)

# clip sub watersheds and reproject
reproj_sheds = function(x){
  y = st_transform(x, 32613)
  return(y)
}
watersheds = list(st_read('../shp/subwatershed/deadhorse/deadhorsewatershed.shp'),
                  st_read('../shp/subwatershed/eslc/eslcwatershed.shp'),
                  st_read('../shp/subwatershed/fool/foolshed.shp'),
                  st_read('../shp/subwatershed/lex/lexshed.shp')) %>%
  lapply(., reproj_sheds)

names = c('deadhorse', 'eslc', 'fool', 'lex')
for(i in 1:4){
  data_dem = dem %>%
    mask(., watersheds[[i]]) %>%
    crop(., extent(watersheds[[i]]))
  writeRaster(data_dem, paste0('../subwatershed_outputs/', names[i], '_dem_utm_10m.tif'), overwrite = T)
  writeRaster(data_dem, paste0('../subwatershed_outputs/', names[i], '_dem_utm_10m.asc'), format = 'ascii', overwrite = T)
  
  data_flow = flow_accum %>%
    mask(., watersheds[[i]]) %>%
    crop(., extent(watersheds[[i]]))
  writeRaster(data_flow, paste0('../subwatershed_outputs/', names[i], '_flow_accumulation_utm_10m.tif'), overwrite = T)
  writeRaster(data_flow, paste0('../subwatershed_outputs/', names[i], '_flow_accumulation_utm_10m.asc'), format = 'ascii', overwrite = T)
}


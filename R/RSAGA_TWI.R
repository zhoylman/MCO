rm(list = ls())

library(RSAGA)
library(rgdal)
library(raster)
library(tictoc)

#Working Directory
working.dir = "/home/zhoylman/location/of/tiffs"

#Sets enviormental variables for SAGA (left these paths to guide you to where the saga cmd files are on your comp) 
#This are also allows you to designate how many cores to use in parallel
env <- rsaga.env(path = 'C:\\Users\\zachary.hoylman.UM\\Downloads\\saga-7.0.0_x64\\saga-7.0.0_x64',
                 modules = 'C:\\Users\\zachary.hoylman.UM\\Downloads\\saga-7.0.0_x64\\saga-7.0.0_x64\\tools',
                 parallel = TRUE, cmd = "saga_cmd.exe", cores = 8)

#Extracts files form working directory to work though
files = list.files(paste0(working.dir,"/extra_path"), pattern = ".tif$", full.names = T)

#Run files not already processed
for(f in 1:length(files)){
  tic("run time")
  #Convert tif to sgrd
  rsaga.geoprocessor(lib="io_gdal", module=0, param=list(GRIDS=paste(getwd(),paste(files[f], ".sgrd", sep = ""),sep="/"), 
                                                         FILES=paste(getwd(),paste(files[f], ".tif", sep = ""),sep="/")), env = env)

  # This is where I calcualted TWI, this is where you insert the module you want. For instance, flow direction)
  # Calculate TWI
  rsaga.geoprocessor(lib="terrain_analysis", module = "Topographic Wetness Index (One Step)", env = env,
                     param=list(DEM=paste(getwd(),paste(files[f], ".sgrd", sep = ""),sep="/"), 
                                TWI=paste(getwd(),paste(files[f],"_TWI.sgrd", sep = ""),sep="/"),
                                FLOW_METHOD = "Multiple Triangular Flow Direction"))
  #Export sgrd as tif
  rsaga.geoprocessor(lib="io_gdal", module=2, param=list(GRIDS=paste(getwd(),paste(files[f],"_TWI.sgrd", sep = ""),sep="/"), 
                                                         FILE=paste(getwd(),paste(files[f],"_TWI.tif", sep = ""),sep="/")), env = env)
  
  print(paste("------------",f, " out of ", length(files),"------------"), sep = "")
  toc()
}

# Generally I loose the geospatial metadata in the tiff _. sgrid -> tiff steps so I redefine the CRS here with a template tiff

example_crs = raster("/home/zhoylman/path/to/raster/with/crs/here.tif")

# reproject
# figure out how to parse out the files you want to reproject. for me it was just grping through strings to find "TWI.tif"
TWI_Rasters = substr(list.files(pattern = "TWI.tif", full.names = F),1,nchar(list.files(pattern = c("TWI.tif"), full.names = F))-4)

for(f in 1:length(TWI_Rasters)){
  tic()
  setwd("H:\\Region_1_RSAGA_TWI_HUC8_SDAT")
  temp_raster = raster(paste(TWI_Rasters[f],".tif", sep = ""))
  crs(temp_raster) = crs(example_crs)
  setwd("H:\\Region_1_RSAGA_TWI_HUC8_SDAT_Reproject")
  writeRaster(temp_raster, (paste(TWI_Rasters[f],".tif", sep = "")), format = "GTiff")
  toc()
  print(paste("------------",f, " out of ", length(TWI_Rasters),"------------"), sep = "")
}



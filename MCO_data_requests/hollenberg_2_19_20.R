library(rgdal)
library(dplyr)
library(leaflet)
library(leaflet.extras)
library(raster)

gdalUtils::gdalwarp(srcfile = "/home/zhoylman/Downloads/ecorast.tif", 
                    dstfile = "/home/zhoylman/Downloads/ecorast_reproj.tif", overwrite = T, 
                    s_srs = "EPSG:4326", t_srs= "EPSG:3857", of = "GTiff", tr = c(4000, 4000))

test = raster::raster("/home/zhoylman/Downloads/ecorast_reproj.tif") 

writeRaster(test, "/home/zhoylman/Downloads/ecorast_reproj_compressed.tif")

new = raster::raster("/home/zhoylman/Downloads/ecorast_reproj_compressed.tif") 

temp = extent(new)

temp[3] = -8000000
temp[4] = 19000000

new = crop(new, temp)

html = leaflet() %>%
  addRasterImage(new)

htmlwidgets::saveWidget(html, "/home/zhoylman/Downloads/ecorast.html")

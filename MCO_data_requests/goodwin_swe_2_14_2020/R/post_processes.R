library(raster)

start = brick("/home/zhoylman/goodwin_swe/data/raw/April_1_SWE.tif")
end = brick("/home/zhoylman/goodwin_swe/data/raw/June_1_SWE.tif")
time = c(1980:2018)

for(i in 1:nbands(start)){
  temp = start[[i]]
  out_name = paste0("/home/zhoylman/goodwin_swe/data/annual/April_1/April_1_SWE_", time[i], ".tif")
  writeRaster(temp, file = out_name)
}

for(i in 1:nbands(start)){
  temp = end[[i]]
  out_name = paste0("/home/zhoylman/goodwin_swe/data/annual/June_1/June_1_SWE_", time[i], ".tif")
  writeRaster(temp, file = out_name)
}

for(i in 1:nbands(start)){
  temp = (end[[i]] / start[[i]])*100
  out_name = paste0("/home/zhoylman/goodwin_swe/data/annual/difference/difference_SWE_", time[i], ".tif")
  writeRaster(temp, file = out_name)
}


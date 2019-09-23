# Calculate quantiles for preciptation for Montana for
# a given time period. 

## LOAD THE REQUIRED LIBRARYS
library(ncdf4) # if running on windows, need opendap ncdf4 build https://github.com/pmjherman/r-ncdf4-build-opendap-windows
library(lubridate)
library(dplyr)
library(zoo)
library(plyr)
library(rowr)
library(precintcon)
library(gridExtra)
library(raster)
library(MASS)
library(tictoc)
library(doParallel)
library(foreach)
library(rgdal)
library(rgeos)
library(stringr)
library(spdplyr)

## DEFINE OUR VARIABLE NAME 
var="precipitation_amount"

raster_precip = brick("http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_pr_1979_CurrentYear_CONUS.nc", var= var)
#proj4string(raster_precip) = CRS("+init=EPSG:4326")

#import UMRB outline for clipping and watershed for aggregating
states = rgdal::readOGR("~/MCO/shp/states.shp")
montana = states %>%
  filter(STATE_NAME == "Montana")

#clip precip grids to the extent of UMRB, to reduce dataset and bring grids into memory
raster_precip_spatial_clip = crop(raster_precip, extent(montana))

time = data.frame(datetime = as.Date(as.numeric(substring(names(raster_precip_spatial_clip),2)), origin="1900-01-01"))
time$day = strftime(time$datetime,"%m-%d")

#compute indexes for time breaks
second_date_breaks = which(time$day == "05-01")
first_date_breaks = which(time$day == "07-31")


#create slice vectors and group by vectors
for(j in 1:length(first_date_breaks)){
  if(j == 1){
    slice_vec = seq(second_date_breaks[j],first_date_breaks[j], by = 1)
    group_by_vec = rep(j,(first_date_breaks[j] - second_date_breaks[j]+1))
  }
  else{
    slice_vec = append(slice_vec, seq(second_date_breaks[j],first_date_breaks[j], by = 1))
    group_by_vec = append(group_by_vec, rep(j,(first_date_breaks[j] - second_date_breaks[j]+1)))
  }
}

#start cluster for parellel computing
cl = makeCluster(detectCores()-1)
registerDoParallel(cl)

#sum and mask precip in parellel
raster_precip_clipped = foreach(i=unique(group_by_vec)) %dopar% {
  library(raster)
  temp = sum(raster_precip_spatial_clip[[slice_vec[group_by_vec == i]]])
  mask(temp, montana)
}

#calucalte time integrated precip sum
integrated_precip = data.frame(matrix(nrow = length(values(raster_precip_clipped[[1]])), ncol = length(unique(group_by_vec))))
for(i in 1:length(unique(group_by_vec))){
  integrated_precip[,i] = values(raster_precip_clipped[[i]])
}

quantile_30 = function(x){
  temp = quantile(x,0.3, na.rm = T)
  return(temp)
}

quantile_50 = function(x){
  temp = quantile(x,0.5, na.rm = T)
  return(temp)
}

#calcualte precipitation qunatiles
precip_quantiles_30 = parApply(cl,integrated_precip, 1, FUN = quantile_30)
precip_quantiles_50 = parApply(cl,integrated_precip, 1, FUN = quantile_50)

#stop parellel cluster
stopCluster(cl)

############################################
############## RASTER FILE #################
############################################

#create spatial template for quantile values
precip_30 = raster_precip_clipped[[1]]
precip_50 = raster_precip_clipped[[1]]

#allocate quantile values to spatial template
values(precip_30) = precip_quantiles_30
values(precip_50) = precip_quantiles_50

#convert to in
precip_30 = precip_30/25.4
precip_50 = precip_50/25.4

#write GeoTiff
writeRaster(precip_30, "~/MCO/data_output/montana_30_percent_propability.tif", format = "GTiff", overwrite = T)
writeRaster(precip_50, "~/MCO/data_output/montana_50_percent_propability.tif", format = "GTiff", overwrite = T)

#set upper bounds
values(precip_30)[values(precip_30) > 10] = 10
values(precip_50)[values(precip_50) > 10] = 10

#compute color ramp for visualization
color_ramp = colorRampPalette(c("darkred","red", "white", "blue", "darkblue"))

#plot map
plot(precip_30, col = color_ramp(100), 
     main = "30% Quantile Precip Sum (05/01 - 07/31)")

plot(precip_50, col = color_ramp(100), 
     main = "50% Quantile Precip Sum (05/01 - 07/31)")

source("~/MCO/R/base_map.R")

ramp = c('#d73027','#f46d43','#fdae61','#fee090','#ffffbf','#e0f3f8','#abd9e9','#74add1','#4575b4')

pal1 <- leaflet::colorBin(ramp, 
                         domain = NULL,
                         bins = c(2:9,20),
                         na.color = "transparent")

names = c("30th Percentile [in]", "50th Percentile (median) [in]")

map = base_map()%>%
  leaflet::addRasterImage(precip_30, colors = pal1, opacity = 0.8, group = names[1], project = TRUE)%>%
  leaflet::addRasterImage(precip_50, colors = pal1, opacity = 0.8, group = names[2], project = TRUE)%>%
  
  
  leaflet::addLegend(group = names[1], pal = pal1,
            title = paste0(names[1], "<br>May 1 - July 31<br>(1979-2019)"),
            values = c(2:9,20),
            position = "bottomleft")%>%
  
  leaflet::addLegend(group = names[2], pal = pal1,
                     title = paste0(names[2], "<br>May 1 - July 31<br>(1979-2019)"),
                     values = c(2:9,20),
                     position = "bottomleft")%>%
  
  leaflet::addLayersControl(position = "topleft",
                            overlayGroups = names,
                            baseGroups = c("States"),
                            options = leaflet::layersControlOptions(collapsed = FALSE))%>%
  leaflet::hideGroup(names[2])

map

htmlwidgets::saveWidget(map, "~/MCO/data_output/precip_probs.html", selfcontained = T)


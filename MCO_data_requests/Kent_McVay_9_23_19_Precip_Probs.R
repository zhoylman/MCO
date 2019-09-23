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

 # quantile fucntions
{
  quantile_10 = function(x){
    temp = quantile(x,0.1, na.rm = T)
    return(temp)
  }
  
  quantile_30 = function(x){
    temp = quantile(x,0.3, na.rm = T)
    return(temp)
  }
  
  quantile_50 = function(x){
    temp = quantile(x,0.5, na.rm = T)
    return(temp)
  }
  
  quantile_70 = function(x){
    temp = quantile(x,0.7, na.rm = T)
    return(temp)
  }
  
  quantile_90 = function(x){
    temp = quantile(x,0.9, na.rm = T)
    return(temp)
  } 
}

#calcualte precipitation qunatiles
functions = c("quantile_10","quantile_30","quantile_50",
              "quantile_70","quantile_90")

precip_quantiles = list()
for(i in 1:length(functions)){
  precip_quantiles[[i]] = parApply(cl,integrated_precip, 1, FUN = functions[i])
}


#stop parellel cluster
stopCluster(cl)

############################################
############## RASTER FILE #################
############################################

precip = list()

for(i in 1:length(functions)){
  #create spatial template for quantile values
  precip[[i]] = raster_precip_clipped[[1]]
  #allocate quantile values to spatial template
  values(precip[[i]]) = precip_quantiles[[i]]
  #convert to in
  precip[[i]] = precip[[i]]/25.4
  #write GeoTiff
  writeRaster(precip[[i]], paste0("~/MCO/data_output/montana_", functions[i],".tif"), format = "GTiff", overwrite = T)
  #set max values
  #values(precip[[i]])[values(precip[[i]]) > 15] = 15
}

#plot leaflet
source("~/MCO/R/base_map.R")

#import counties 
counties = rgdal::readOGR("~/MCO/shp/mt_counties.shp")

counties_simple = rgeos::gSimplify(counties, tol = 0.001, topologyPreserve = TRUE)

ramp = c('#d73027','#f46d43','#fdae61','#fee090','#ffffbf','#e0f3f8','#abd9e9','#74add1','#4575b4', "#00008b", "#2E0854")

pal1 <- leaflet::colorBin(ramp, 
                         domain = NULL,
                         bins = c(seq(1,9,1), seq(11,15,2),c(20,25,30)),
                         na.color = "transparent")

names = c("10th Percentile [in] (very dry year)","30th Percentile [in] (dry year)", 
          "50th Percentile [in] (average year)", "70th Percentile [in] (wet year)", "90th Percentile [in] (very wet year)")

map = base_map()

for(i in 1: length(names)){
  map = map %>% 
        leaflet::addRasterImage(precip[[i]], colors = pal1, opacity = 0.8, group = names[i], project = TRUE)
}
  
map = map %>% 
  leaflet::addLegend(pal = pal1,
            title = "May 1 - July 31<br>(1979-2019)",
            values = c(seq(1,9,1), seq(11,15,2),c(20,25,30)),
            position = "bottomleft")%>%
  leaflet::addPolygons(data = counties_simple, group = "Counties", fillColor = "transparent", weight = 2, color = "black", opacity = 1)%>%
  leaflet::addLayersControl(position = "topleft",
                            baseGroups = names,
                            overlayGroups = c("States", "Counties"),
                            options = leaflet::layersControlOptions(collapsed = FALSE))%>%
  leaflet::hideGroup(names[c(1,2,4,5)])


map

htmlwidgets::saveWidget(map, "~/MCO/data_output/precip_probs.html", selfcontained = T)


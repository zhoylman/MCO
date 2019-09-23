library(ncdf4) 
library(dplyr)
library(lubridate)

gridmet_extract_point = function(lat_in, lon_in, variable_url, var){ 
  lat_of_interest = lat_in
  lon_of_interest = lon_in
  url = variable_url
  var = var 
  
  #pull data from northwest knowledge threads server (netcdf)
  nc <- nc_open(url)
  
  #file dimensions are lon,lat,time
  v3 = nc$var[[1]]
  lonsize = v3$varsize[1]
  latsize = v3$varsize[2]
  endcount = v3$varsize[3] 
  
  ### define point of interest
  lon_matrix = nc$var[[1]]$dim[[1]]$vals
  lat_matrix = nc$var[[1]]$dim[[2]]$vals
  
  #find lat long that corispond (minimize difference)
  lon=which(abs(lon_matrix-lon_of_interest)==min(abs(lon_matrix-lon_of_interest)))  
  lat=which(abs(lat_matrix-lat_of_interest)==min(abs(lat_matrix-lat_of_interest))) 
  
  ## READ THE DATA VARIABLE 
  data <- ncvar_get(nc, var, start=c(lon,lat,1),count=c(1,1,endcount))
  ## READ THE TIME VARIABLE
  time <- ncvar_get(nc, "day", start=c(1),count=c(endcount))
  ## CONVERT TIME FROM "days since 1900-01-01" TO YYYY-MM-DD
  time=as.Date(time, origin="1900-01-01") 
  # PUT EVERYTHING INTO A DATA FRAME
  dataset <- data.frame(time,data)
  
  ## CLOSE THE FILE
  nc_close(nc)
  
  #return dataset
  return(dataset)
}

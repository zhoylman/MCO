library(dplyr)
library(tidyr)
library(data.table)

nc = raster::brick("http://thredds-prod.nkn.uidaho.edu:8080/thredds/dodsC/agg_terraclimate_aet_1958_CurrentYear_GLOBE.nc", var="aet")

nc_time = names(nc)

nutnet = read.csv("/home/zhoylman/temp/NutNet_Data_141019.csv")

nutnet_unique = nutnet %>%
  select(site_code, latitude, longitude)%>%
  distinct()

points = data.frame(longitude=nutnet_unique$longitude, latitude = nutnet_unique$latitude) %>%
  drop_na() %>%
  sp::SpatialPoints()

tictoc::tic()
data = t(data.frame(raster::extract(nc, points)))
colnames(data) = nutnet_unique$site_code
tictoc::toc()

write.csv(data, "./data_output/NutNet_AET.csv")

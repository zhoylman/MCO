
library(rnoaa) 
library(tidyverse)
library(lubridate)
library(magrittr)
library(lmomco)
library(doParallel)
library(ggplot2)
library(foreach)
library(doParallel)
library(sf)
library(automap)
library(gstat)
library(spdplyr)
library(automap)
options(dplyr.summarise.inform = FALSE)

# define base parameters 
# ID to define time scale, months of interest and minimum
# number of records, coorisponding to "complete data"
time_scale_id = 1
time_scale = list(30,60,90)

months_of_interest = list(c(5,6,7,8),
                          c(4,5,6,7,8),
                          c(3,4,5,6,7,8))
n_minimum = list(123,153,184)

# define nice special
`%notin%` = Negate(`%in%`)

# import states to filter and for plotting
states = st_read('/home/zhoylman/drought-year-sensitivity/data/shp/conus_states.shp') %>%
  st_geometry()

#read in dataframe of valid stations
valid_stations = readRDS('/home/zhoylman/drought-year-sensitivity/data/valid_stations_70year_summer_baseline.RDS')

ggplot()+
  geom_sf(data = states)+
  geom_sf(data = valid_stations %>% st_geometry(), color = 'black', fill = 'cyan', shape = 21)+
  theme_bw(base_size = 16)

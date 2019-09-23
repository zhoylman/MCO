# Download data for sntoel site #657 (North Fork Elk Creek, Near Potomac MT)
# Site is in Lubrecht at 6250 feet Elevation
# This script can be modified to download any NRCS station

library(RNRCS)
library(dplyr)
library(data.table)

#define site information
site_num = 657
network_name = "SNTL"
temporal_resolution = "daily"

# get meta data for station, here we will use the start date to download data
meta = grabNRCS.meta(network_name)[[network_name]] %>%
  dplyr::filter(site_id == paste0(network_name,":",site_num)) 

# extract year from string
year = as.numeric(gsub("([0-9]+).*$", "\\1", meta$start))

# download full record of data
data = grabNRCS.data(network_name, site_num , timescale = temporal_resolution, DayBgn = paste0(year,"-10-01"),
                DayEnd = as.Date(Sys.time()))

# filter for columnames for portions of string that match variables of interest 
data_filtered = data[colnames(data) %like% "Date" | colnames(data) %like% "Temperature" |
                     colnames(data) %like% "Precipitation" ]

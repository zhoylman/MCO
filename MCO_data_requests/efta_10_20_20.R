library(RNRCS) 
library(tidyverse)
library(snotelr)

#define SNOTEL Site ID
id = 365

#Pull down Meta Data
site_info = grabNRCS.meta('SNTL')[[1]] %>%
  filter(., site_id %in% paste0('SNTL:', id))

#retrieve raw data from server
raw_data = grabNRCS.data(network = site_info$ntwk[1], 
                         site_id = id, 
                         timescale = "hourly", 
                         DayBgn = "1999-10-01", 
                         DayEnd = "2100-01-01")


precip = raw_data %>% 
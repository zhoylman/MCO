library(tidyverse)
library(ggplot2)
library(sf)
library(scales)

#make spatial dataset
sites = read_csv('/home/zhoylman/MCO/data/BLM/blm_stations.csv') %>%
  select(station_key, lat, lon, qaqc) %>% 
  mutate(id = seq(1:17)) #%>%
  #sf::st_as_sf(., coords = c("lon", "lat"), crs = sp::CRS("+proj=longlat +datum=WGS84")) 

#st_write(sites, '/home/zhoylman/MCO/data/BLM/blm_stations.shp')

data = read_csv('/home/zhoylman/MCO/data/BLM/ndvi_record.csv') %>%
  t() %>%
  as.data.frame()

ids = data[which(rownames(data) == 'id'),] %>%
  t() %>%
  as.data.frame() %>%
  mutate(id = as.character(id) %>%
           as.numeric()) %>%
  dplyr::left_join(., sites, by = 'id')

colnames(data) = ids$station_key
data$rownames = rownames(data)
rownames(data) = NULL

data_clean = data %>%
  mutate(time = gsub("[^0-9.]", "",  rownames) %>%
           as.character()%>%
           as.Date(., format = '%Y%m%d'),
         var = gsub("[^[:alpha:]]", "",  rownames) %>%
           as.character())%>%
  tidyr::gather(key, value, -time, -var) %>%
  mutate(value = as.numeric(value)) %>%
  as.tibble() %>%
  drop_na() 

station_select = c('blm1arge', 'blm3mcca', 'blmplevn', 'blmpumpk', 'blmround')

station_statistics = data_clean %>%
  dplyr::filter(time > as.Date('2003-01-01')) %>%
  mutate(week = lubridate::week(time))%>%
  group_by(key, week, var) %>%
  summarize(median = median(value),
            q_lower = quantile(value, 0.01),
            q_upper = quantile(value, 0.99)) %>%
  filter(key %in% station_select) %>%
  mutate(time_alt = lubridate::ymd( "2019-01-01" ) + lubridate::weeks(week - 1),
         time_alt = as.POSIXct(time_alt))

modis_2019 = data_clean %>%
  dplyr::filter(time > as.Date('2019-01-01') & time < as.Date('2019-12-31')) %>%
  filter(key %in% station_select) %>%
  mutate(time = as.POSIXct(time))

station_ndvi = readxl::read_excel('/home/zhoylman/MCO/data/BLM/ndvi/ndvi/mesonet_ndvi.xlsx') %>%
  as.tibble() %>%
  filter(Date > as.Date('2019-01-01')) %>%
  filter(`NDVI — Aggregated` < .99) %>%
  mutate(key = Station) %>%
  filter(key %in% station_select)

# %>%
#   mutate(week = lubridate::week(Date)) %>%
#   drop_na() %>%
#   mutate(key = Station) %>%
#   group_by(key, week) %>%
#   summarize(median = median(`NDVI — Aggregated`),
#             q_lower = quantile(`NDVI — Aggregated`, 0.10),
#             q_upper = quantile(`NDVI — Aggregated`, 0.90)) %>%
#   filter(key %in% station_select) %>%
#   mutate(time_alt = lubridate::ymd( "2019-01-01" ) + lubridate::weeks(week - 1),
#          time_alt = as.POSIXct(time_alt))


vars <- c("Station Data (2019)"="red", "MODIS Climatology"="black", 'MODIS (2019)' = 'blue')

plots = ggplot(data = station_statistics %>% filter(var == "LANDSATNDVI"), aes(x = time_alt, y = median, ymin = q_lower, ymax = q_upper)) + 
  #geom_ribbon(fill = "grey70") +
  #geom_line() +
  geom_ribbon(data = station_statistics %>% filter(var == "MODISNDVI"), aes(x = time_alt, y = median, ymin = q_lower, ymax = q_upper),
              fill = "grey70", alpha = 0.3) +
  geom_line(data = station_statistics %>% filter(var == "MODISNDVI"), aes(x = time_alt, y = median, color = 'MODIS Climatology')) +
  geom_point(data = station_ndvi, aes(color = 'Station Data (2019)', x = Date, y = `NDVI — Aggregated`, ymax = NULL, ymin = NULL), size = 0.5) +
  geom_point(data = modis_2019, aes(color = 'MODIS (2019)', x = time, y = value, ymax = NULL, ymin = NULL), size = 1) +
  facet_wrap(~key, scales = "free") + 
  theme_bw() +
  ylab('NDVI')+
  xlab(NULL)+ 
  scale_x_datetime(labels = date_format("%b-%Y"), date_breaks = '3 months')+
  theme(strip.background = element_blank(), strip.placement = "outside")+
  scale_colour_manual(name = 'Legend', values=vars, breaks = c("MODIS Climatology", "Station Data (2019)", 'MODIS (2019)')) +
  theme(legend.position= c(0.85,0.25),
        legend.box.background = element_rect(colour = "black"),
        legend.title.align=0.5)+ 
  xlim(as.POSIXct(c(as.Date('2019-04-01'), as.Date('2019-10-01'))))

plots
ggsave(plots, file = '/home/zhoylman/MCO/data/BLM/plot_sample.png', units = 'in', width = 10, height = 6)

station_ndvi_full = readxl::read_excel('/home/zhoylman/MCO/data/BLM/ndvi/ndvi/mesonet_ndvi.xlsx') %>%
  as.tibble() 


full_data = ggplot(data = station_ndvi_full, aes(x = Date, y = `NDVI — Aggregated`))+
  geom_point()+
  theme_bw()+
  facet_wrap(~Station)


full_data
ggsave(full_data, file = '/home/zhoylman/MCO/data/BLM/raw_station_data.png')























station_statistics = data_clean %>%
  mutate(week = lubridate::week(time))%>%
  group_by(key, week) %>%
  summarize(median = median(value),
            q_lower = quantile(value, 0.10),
            q_upper = quantile(value, 0.90))

plots_combined = ggplot(data = station_statistics, aes(x = week, y = median, ymin = q_lower, ymax = q_upper)) + 
  geom_ribbon(fill = "grey70") +
  geom_line() +
  facet_wrap(~key, scales = "free") + 
  theme_bw() +
  ylab('NDVI')+
  xlab('Week ID')
  

plots_combined
ggsave(plots_combined, file = '/home/zhoylman/MCO/data/BLM/plot_sample_combined.png')



ggsave(plots, file = '/home/zhoylman/MCO/data/BLM/plot_sample.png')

station = read_csv('/home/zhoylman/MCO/data/BLM/NDVI blm3mcca 2018-19 Apr-Sep.csv') %>%
  mutate(Date = as.Date(Date, format = '%d-%b-%y'))

test = data_clean %>%
  dplyr::filter(var == 'NDVI') %>%
  dplyr::filter(time > as.Date('2016-01-01'))

plot(station$Date, station$NDVI, xlab = 'Time', ylab = 'NDVI', ylim = c(0,0.6))
points(test$time, test$value, col = 'red')
lines(test$time, test$value, col = 'red')
legend(as.Date('2019-07-01'), 0.6, legend=c("Station", "MODIS"),
       col=c("black", "red"), lty=1:1, cex=0.8)

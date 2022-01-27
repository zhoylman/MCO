library(tidyverse)

data = RCurl::getURL(paste0('https://waterdata.usgs.gov/nwis/dv?referred_module=sw&search_site_no=12359800&search_site_no_match_type=exact&site_tp_cd=OC&site_tp_cd=OC-CO&site_tp_cd=ES&site_tp_cd=LK&site_tp_cd=ST&site_tp_cd=ST-CA&site_tp_cd=ST-DCH&site_tp_cd=ST-TS&group_key=NONE&sitefile_output_format=html_table&column_name=agency_cd&column_name=site_no&column_name=station_nm&range_selection=date_range&begin_date=1996-10-01&end_date=',
                     Sys.Date()+1, '&format=rdb&date_format=YYYY-MM-DD&rdb_compression=value&list_of_search_criteria=search_site_no%2Csite_tp_cd%2Crealtime_parameter_selection')) %>%
  read_tsv(skip = 29)
  
data = data[-1,] %>%
  mutate(time = as.Date(datetime),
         q = as.numeric(`82054_00060_00003`),
         day = lubridate::yday(time)) %>%
  dplyr::select(time, q, day)
      
stats = data %>%
  drop_na() %>%
  group_by(day) %>%
  summarise(median = median(q),
            q5 = quantile(q, 0.05),
            q25 = quantile(q, 0.25),
            q75 = quantile(q, 0.75),
            q95 = quantile(q, 0.95)) %>%
  mutate(time = as.Date(paste0('2018-', day), format = '%Y-%j') %>%
           as.POSIXct())

data_2018 = data %>%
  filter(time > as.Date('2018-01-01') & time < as.Date('2019-01-01')) %>%
  mutate(time = as.POSIXct(time))

data_2017 = data %>%
  filter(time > as.Date('2017-01-01') & time < as.Date('2018-01-01')) %>%
  mutate(time = as.POSIXct(time)) %>%
  mutate(time = as.POSIXct(time),
         day = lubridate::yday(time),
         time_alt = as.Date(paste0('2018-', day+1), format = '%Y-%j') %>%
           as.POSIXct())

data_2020 = data %>%
  filter(time > as.Date('2020-01-01') & time < as.Date('2021-01-01')) %>%
  mutate(time = as.POSIXct(time),
         day = lubridate::yday(time),
         time_alt = as.Date(paste0('2018-', day+1), format = '%Y-%j') %>%
           as.POSIXct())

cols = c('Average' = 'black','2018' = 'red', '2020' = 'blue', '2017' = 'orange')

q = ggplot(data = stats, aes(x = time, y = median, ymax = q95, ymin = q5)) +
  geom_ribbon(fill = 'grey', alpha = 0.4)+
  geom_line(aes(color = 'Average')) + 
  theme_bw() +
  geom_line(data = data_2018, aes(x = time, y = q, ymax = NULL, ymin = NULL, color = '2018')) +
  geom_line(data = data_2017, aes(x = time_alt, y = q, ymax = NULL, ymin = NULL, color = '2017')) +
  geom_line(data = data_2020, aes(x = time_alt, y = q, ymax = NULL, ymin = NULL, color = '2020'), size = 1.2)+
  ylab('Q (ft^3/s)')+
  xlab('Time')+
  scale_colour_manual(name="Discharge",values=cols, breaks = c('Average', '2017','2018', '2020')) +
  xlim(c(as.POSIXct('2018-04-01'), as.POSIXct('2018-08-01'))) +
  geom_point(data = NULL, aes(x = data_2018$time[c(200)], y = data_2018$q[c(200)]))+
  geom_point(data = NULL, aes(x = data_2018$time[c(203)], y = data_2018$q[c(203)]))+
  geom_point(data = NULL, aes(x = stats$time[106], y = stats$median[106]), shape = 3)+
  geom_point(data = NULL, aes(x = stats$time[110], y = stats$median[110]), shape = 3)+
  coord_trans(y = "log10")+
  geom_line(data = stats, aes(x = time, y = q75), linetype = 'dashed')

q
ggsave(q, file = '/home/zhoylman/temp/south_fork_q.png')
  
  

  
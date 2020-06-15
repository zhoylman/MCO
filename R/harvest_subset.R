library(tidyverse)
response = read_csv('/home/zhoylman/MCO/data/USFS/harvest_stats_rG.csv')
response$`system:index` = NULL

response_years = c('clim',paste0(c(0,seq(1:35)), '_NDVI'))

ndvi = response[,response_years] %>%
  as.tibble() %>%
  mutate(site = 1:200) %>%
  gather("key", 'value', -clim, -site) %>%
  mutate(xtile = statar::xtile(clim, 3) %>%
           as.factor()) %>%
  mutate(xtile = plyr::revalue(xtile, c(`1` = "Wet",
                `2` = "Moderate",
                `3` = "Dry"))) %>%
  group_by(xtile, key) %>%
  summarise(median = median(value))%>%
  mutate(time = gsub("[^0-9.]", "",  key) %>%
           as.numeric() + 1984) %>%
  filter(time < 2019)

models = ndvi %>%
  filter(time > 1991) %>%
  group_by(xtile) %>%
  do(linearFit = lm(median ~ time, data = .)) %>%
  mutate(slope = coef(linearFit)[2] ,
         r2 = (summary(linearFit)$r.squared))

plots = ggplot(data = ndvi, aes(x = time, y = median))+
  geom_smooth(data = ndvi %>% filter(time > 1991), method = 'lm') + 
  geom_point() +
  geom_vline(aes(xintercept = 1990))+
  theme_bw(base_size = 16) +
  geom_text(data = models, aes(x = 1997, y = 1.4, label = paste0('Slope = ', round(slope, 4))))+
  geom_text(data = models, aes(x = 1995, y = 1.3, label = paste0('r2 = ', round(r2, 3))))+
  xlab('Year') + 
  ylab('Relavtive NDVI')+
  facet_wrap(~xtile, labeller = labeller(c("1" = "Wet",
                                           "2" = "Moderate",
                                           "3" = "Dry")))+
  theme(strip.background = element_blank(), strip.placement = "outside")
  
plots
ggsave(plots, file = '/home/zhoylman/MCO/data/USFS/harvest_plot.png', units = 'in', width = 10, height = 4)


## NPP

response_npp = read_csv('/home/zhoylman/MCO/data/USFS/harvest_stats_rNPP.csv')
response_npp$`system:index` = NULL

response_years = c('clim',paste0(1986:2019, '_annualNPP'))

npp = response_npp[,response_years] %>%
  as.tibble() %>%
  mutate(site = 1:200) %>%
  gather("key", 'value', -clim, -site) %>%
  mutate(xtile = statar::xtile(clim, 3) %>%
           as.factor()) %>%
  mutate(xtile = plyr::revalue(xtile, c(`1` = "Wet",
                                        `2` = "Moderate",
                                        `3` = "Dry"))) %>%
  group_by(xtile, key) %>%
  summarise(median = median(value))%>%
  mutate(time = gsub("[^0-9.]", "",  key) %>%
           as.numeric()) 

models_npp = npp %>%
  filter(time > 1992) %>%
  group_by(xtile) %>%
  do(linearFit = lm(median ~ time, data = .)) %>%
  mutate(slope = coef(linearFit)[2] ,
         r2 = (summary(linearFit)$r.squared))


plots_npp = ggplot(data = npp, aes(x = time, y = median))+
  geom_smooth(data = npp %>% filter(time > 1992), method = 'lm') + 
  geom_point() +
  geom_vline(aes(xintercept = 1990))+
  theme_bw(base_size = 16) +
  geom_text(data = models_npp, aes(x = 1997, y = 1.15, label = paste0('Slope = ', round(slope, 4))))+
  geom_text(data = models_npp, aes(x = 1995, y = 1.10, label = paste0('r2 = ', round(r2, 3))))+
  xlab('Year') + 
  ylab('Relavtive NPP')+
  facet_wrap(~xtile, labeller = labeller(c("1" = "Wet",
                                           "2" = "Moderate",
                                           "3" = "Dry")))+
  theme(strip.background = element_blank(), strip.placement = "outside")

plots_npp
ggsave(plots_npp, file = '/home/zhoylman/MCO/data/USFS/harvest_plot_npp.png', units = 'in', width = 10, height = 4)















  plot(ndvi_stats)
abline(v = 7)

ndvi_t = t(ndvi) %>%
  as.tibble() %>%
  mutate(time = 1984:2019) %>%
  gather("key", 'value', -time) 

ggplot(data = ndvi_t, aes(x = time, y = value, color = key))+
  geom_point(guide = F)+
  theme(legend.position = 'none')


#subset
test = st_read('/home/zhoylman/Downloads/R1_timberharvest_dividewest/R1_timberharvest_dividewest.shp')
test$FY_COMPLET = as.character(test$FY_COMPLET) %>%
  as.numeric()

subset = test %>%
  filter(FY_COMPLET == 1992)

index = sample(1:length(subset$FY_COMPLET),200, replace = F)

subset = subset[index,]

st_write(subset, "/home/zhoylman/Downloads/R1_timberharvest_dividewest/R1_timberharvest_dividewest_1990_200.shp")

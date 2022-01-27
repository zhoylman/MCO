library(snotelr)
library(tidyverse)
library(RCurl)
#683 = Parker Peak = 9400 feet // WY
#806 = Sylvan Lake = 8420 feet // WY
#577 = Lewis Lake Divide = 7850 feet // WY
#807 = Sylvan Road = 7120 feet // WY
#924 = west yellowstone = 6700 feet //MT

sites = c(350, 683, 806, 577, 807, 924)
states = c('WY', 'WY', 'WY', 'WY', 'WY', 'MT')

meta = snotelr::snotel_info() %>%
  as_tibble() %>%
  filter(site_id %in% sites) %>% 
  arrange(-elev)

plots = list()

for(i in 1:6){
  base_url <- paste0(
    "https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customSingleStationReport,metric/daily/",
    sites[i], ":",
    states[i], ":",
    'SNTL',
    "%7Cid=%22%22%7Cname/",'1981-01-01',",",'2020-12-31',"/WTEQ::value"
  )
  
  data_raw_api = getURL(base_url) %>%
    read_csv(skip = 58) 
  
  processed = data_raw_api %>%
    mutate(year = lubridate::year(Date)) %>%
    group_by(year) %>%
    summarise(max_swe = max(`Snow Water Equivalent (mm) Start of Day Values`, na.rm = T)) 
  
  plots[[i]] = ggplot()+
    geom_point(data = processed, aes(x = year, y = max_swe/25.4))+
    theme_bw(base_size = 14)+
    ggtitle(paste0('SNOTEL Site: ' ,str_to_title(meta$site_name[i]), '(', meta$elev[i]*3.28084 %>% round(., 1), 'ft)'))+
    if(i == 3){
      labs(x = 'Year', y = 'Annual Max SWE (in)')
    }
    else{
      labs(x = 'Year', y = ' ')
    }
  
}

grid = cowplot::plot_grid(plots[[1]], plots[[2]], plots[[3]], plots[[4]], plots[[5]], plots[[6]], ncol = 2)

title_gg <- ggplot() + 
  labs(title = "Maximum Annual Snow Water Equivalent (SWE) in Yellowstone NP", subtitle = "Produced by the Montana Climate Office")+
  theme_minimal(base_size = 16)+
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

final = cowplot::plot_grid(title_gg, grid, ncol = 1, rel_heights = c(0.1, 1))

ggsave(final, file = '/home/zhoylman/temp/snotel.png', width = 10.5, height = 10)

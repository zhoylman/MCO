library(tidyverse)

# data is from https://waterdata.usgs.gov/nwis/dv?cb_00060=on&format=rdb&site_no=06029000&referred_module=sw&period=&begin_date=1949-01-01&end_date=1969-01-01

streamflow = read_tsv('/home/zhoylman/Desktop/usgs_data_06029000') %>%
  filter(`80743_00060_00003_cd` == "A") %>%
  dplyr::select(datetime, cfs) %>%
  mutate(Month = lubridate::month(datetime),
         day = lubridate::yday(datetime))

average_monthly_flow = streamflow %>%
  group_by(Month) %>%
  summarise(`Median Monthly Discharge (cfs)` = median(cfs, na.rm = T),
            `Number of Observations` = length(cfs))

table_export = average_monthly_flow %>%
  flextable::flextable()%>%
  flextable::autofit() %>%
  flextable::hline(., part="all")

flextable::save_as_image(table_export, '/home/zhoylman/MCO/MCO_data_requests/coppes_06_23_2020/median_flow_table.png')


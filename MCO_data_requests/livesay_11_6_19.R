library(dplyr)
library(lubridate)

# api call
# https://mesonet.climate.umt.edu/api/observations?stations=blm1arge&elements=atmos_pr&latest=false&tz=US%2FMountain&wide=true&type=csv

data = read.csv("/home/zhoylman/Downloads/observations") %>%
  mutate(day = yday(as.Date(datetime))) %>%
  mutate(year = year(as.Date(datetime))) %>%
  mutate(hour = substr(datetime,12,13))%>%
  group_by(hour,day, year)%>%
  summarise(mean(Atmospheric.Pressure..kPa.)) %>%
  mutate(time = as.POSIXct(paste(day,year,hour,sep = "-"), format = "%j-%Y-%H")) %>%
  arrange(time)
  
colnames(data) = c("hour","day","year","mean_pressure_Kpa", "time")

write.csv(data, "/home/zhoylman/MCO/data_output/hourly_pressure_blm1arge.csv", row.names = F)

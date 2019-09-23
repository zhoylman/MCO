source("/home/zhoylman/MCO/R/gridmet_extract_point.R")

url = "http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_pr_1979_CurrentYear_CONUS.nc"

data = gridmet_extract_point(45.743694, -107.587139, url, "precipitation_amount")

data_summary = data %>%
  dplyr::filter(time >= as.Date("1998-01-01") & time <= as.Date("1999-12-31"))%>%
  dplyr::mutate(.,month_year = zoo::as.yearmon(time)) %>%
  dplyr::group_by(month_year)%>%
  dplyr::summarise(sum = (sum(data))*0.0393701)%>% # cums monthly precip and converts to in
  dplyr::rename(month_year = month_year, monthly_precipitation_total_inches = sum)
  
write.csv(data_summary, "/home/zhoylman/MCO/MCO_data_requests/Mark_Majerus_8_30_19.csv", row.names = F)

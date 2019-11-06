library(curl)

curl_download('https://mesonet.climate.umt.edu/api/observations?latest=false&start_time=2019-09-01&tz=US%2FMountain&wide=true&type=csv',
              destfile = "/home/zhoylman/temp/mesonet_data.csv")

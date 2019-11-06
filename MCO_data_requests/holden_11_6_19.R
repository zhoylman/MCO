library(curl)

curl_download('http://cfcmesonet-test.cfc.umt.edu/api/observations?latest=false&start_time=2019-10-06&type=csv',
              destfile = "/home/zhoylman/temp/mesonet_data.csv")

library(knitr)
library(dplyr)
library(RCurl)

# stations = getURL("https://mesonet.climate.umt.edu/api/stations?type=csv&clean=true") %>%
#   read_csv()

mesonet_dynamic_rmd = function(lat, long, station_key, station_name){
  weather_iframe = paste0('<iframe src="https://mobile.weather.gov/index.php?lat=',lat,'&lon=',long,'" height="680px" width="100%" frameborder="0"></iframe>')
  plotly_iframe = paste0('<iframe width="100%" height="100%" allowfullscreen="allowfullscreen" allowvr="yes" frameborder="0" mozallowfullscreen="mozallowfullscreen" scrolling="yes" src="https://mco.cfc.umt.edu/mesonet_data/station_page/',
                         station_key,'_current_data.html" webkitallowfullscreen="webkitallowfullscreen"></iframe>')
  
  plotly_mobile = paste0('<iframe width="100%" height="100%" allowfullscreen="allowfullscreen" allowvr="yes" frameborder="0" mozallowfullscreen="mozallowfullscreen" scrolling="no" src="https://mco.cfc.umt.edu/mesonet_data/station_page/mobile_test.html" webkitallowfullscreen="webkitallowfullscreen"></iframe>')
  
  writeLines(paste0('---
title: "Montana Mesonet - ', station_name,'"
self_contained: true
output: 
  flexdashboard::flex_dashboard:
    self_contained: false
    lib_dir: "./libs"
    theme: spacelab
    navbar:
      - { title: "MCO GitHub", href: "https://github.com/mt-climate-office", align: right }
    orientation: rows
---
  
Column {.sidebar data-width=350}
-------------------------------------

### {.no-mobile}
  
<img src="https://climate.umt.edu/imx/MCO_logo.svg" width="100%">
  
### Stations  {.no-mobile}
  
<iframe width="100%" height="300" allowfullscreen="allowfullscreen" target="_parent" allowvr="yes" frameborder="0" mozallowfullscreen="mozallowfullscreen" scrolling="no" src="https://mco.cfc.umt.edu/mesonet_data/station_page/simple_mesonet_map.html" webkitallowfullscreen="webkitallowfullscreen"></iframe>
  
***
  
### Weather & Forecast {.no-mobile}

  ', weather_iframe,
  '

Column {.tabset .tabset-fade}
-------------------------------------
### Data {.no-mobile}
',
  plotly_iframe,
  '
### Data {.mobile}
', plotly_mobile),
'
### Stations {.mobile}
  
<iframe width="100%" height="300" allowfullscreen="allowfullscreen" target="_parent" allowvr="yes" frameborder="0" mozallowfullscreen="mozallowfullscreen" scrolling="no" src="https://mco.cfc.umt.edu/mesonet_data/station_page/simple_mesonet_map.html" webkitallowfullscreen="webkitallowfullscreen"></iframe>
  ',
                             con = "~/MCO/data/mesonet/station_page/temp.Rmd")  
  rmarkdown::render("~/MCO/data/mesonet/station_page/temp.Rmd", output_file = paste0("~/MCO/data/mesonet/station_page/", station_key, ".html"), quiet=TRUE)
}

mesonet_dynamic_rmd(stations$Latitude[s], stations$Longitude[s], stations$`Station ID`[s], stations$`Station name`[s])

# for(s in 1:length(stations$`Station name`)){
#   mesonet_dynamic_rmd(stations$Latitude[s], stations$Longitude[s], stations$`Station ID`[s], stations$`Station name`[s])
# }




# mesonet_dynamic_rmd = function(lat, long, station_key, station_name){
#   weather_iframe = paste0('<iframe src="https://mobile.weather.gov/index.php?lat=',lat,'&lon=',long,'" height="680px" width="100%" frameborder="0"></iframe>')
#   plotly_iframe = paste0('<iframe width="100%" height="100%" allowfullscreen="allowfullscreen" allowvr="yes" frameborder="0" mozallowfullscreen="mozallowfullscreen" scrolling="no" src="https://mco.cfc.umt.edu/mesonet_data/station_page/',
#                          station_key,'_current_data.html" webkitallowfullscreen="webkitallowfullscreen"></iframe>')
#   
#   writeLines(paste0('---
# title: "Montana Mesonet Data Explorer - ', station_name,'"
# self_contained: true
# output: 
#   flexdashboard::flex_dashboard:
#     self_contained: false
#     lib_dir: "./libs"
#     theme: spacelab
#     navbar:
#       - { title: "MCO GitHub", href: "https://github.com/mt-climate-office", align: right }
#     orientation: rows
# ---
#   
#   Inputs {.sidebar data-width=350}
# -------------------------------------
#   
#   ***
#   
#   <img src="https://climate.umt.edu/imx/MCO_logo.svg" width="100%">
#   
#   ***
#   
#   <center>
#   <font size="5">
#   **Mesonet Stations**
#   </font>
#   </center>
#   
#   <iframe width="100%" height="300" allowfullscreen="allowfullscreen" target="_parent" allowvr="yes" frameborder="0" mozallowfullscreen="mozallowfullscreen" scrolling="no" src="https://mco.cfc.umt.edu/mesonet_data/station_page/simple_mesonet_map.html" webkitallowfullscreen="webkitallowfullscreen"></iframe>
#   
#   ***
#   
#   <center>
#   <font size="5">
#   **Current Weather & Forecast**
#   </font>
#   </center>
# 
#   ', weather_iframe,
#                     '
# 
# Row {.tabset .tabset-fade data-height=1600}
# -------------------------------------
# ### Data {.no-mobile}
# ',
#                     plotly_iframe,
#                     '
# ### Data {.mobile}
# ', plotly_iframe),
#              con = "~/MCO/data/mesonet/station_page/temp.Rmd")  
#   rmarkdown::render("~/MCO/data/mesonet/station_page/temp.Rmd", output_file = paste0("~/MCO/data/mesonet/station_page/", station_key, ".html"), quiet=TRUE)
# }
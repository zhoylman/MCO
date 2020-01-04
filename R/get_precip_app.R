library(ncdf4)
library(lubridate)
library(dplyr)
library(zoo)
library(ggplot2)
library(scales)
library(shiny)
library(leaflet)
library(leaflet.extras)

get_precip = function(lat_in, lon_in){
  #Define URL to net cdf 
  urltotal = "http://thredds.northwestknowledge.net:8080/thredds/dodsC/agg_met_pr_1979_CurrentYear_CONUS.nc"
  # OPEN THE FILE
  nc = nc_open(urltotal)
  # find length of time variable for extraction
  endcount = nc$var[[1]]$varsize[3] 
  # Querry the lat lon matrix
  lon_matrix = nc$var[[1]]$dim[[1]]$vals
  lat_matrix = nc$var[[1]]$dim[[2]]$vals
  # find lat long that corispond
  lon=which(abs(lon_matrix-lon_in)==min(abs(lon_matrix-lon_in)))  
  lat=which(abs(lat_matrix-lat_in)==min(abs(lat_matrix-lat_in))) 
  # define variable name
  var="precipitation_amount"
  # read data and time and extract useful time information
  data = data.frame(data = ncvar_get(nc, var, start=c(lon,lat,1),count=c(1,1,endcount))) %>%
    mutate(time = as.Date(ncvar_get(nc, "day", start=c(1),count=c(endcount)), origin="1900-01-01")) %>%
    mutate(day = yday(time)) %>%
    mutate(year = year(time)) %>%
    mutate(month = month(time, label = T, abbr = F))
  # close file
  nc_close(nc)
  
  #monthly data
  monthly_data = data %>%
    group_by(month, year) %>%
    dplyr::summarise(sum = sum(data)) %>%
    mutate(time = as.POSIXct(as.Date(as.yearmon(paste(year, month, sep = "-"),
                                                '%Y-%b')))) %>%
    arrange(time) 
  # define ggplot function to display 3 years of data
  plot_function = function(data){
    precip_plot = ggplot(data = data, aes(x = time, y = sum))+
      geom_bar(stat = 'identity', fill = "blue")+
      xlab("")+
      ylab("Precipitation (mm/month)")+
      theme_bw(base_size = 16)+
      ggtitle("")+
      theme(legend.position="none",
            axis.text.x = element_text(angle = 60, vjust = 0.5))+
      scale_x_datetime(breaks = date_breaks("3 month"), labels=date_format("%b / %Y"),
                       limits= as.POSIXct(c(data$time[length(data$time)-36], 
                                            data$time[length(data$time)]))) 
    return(precip_plot)
  }
  # return a list of 3 things, the plot (using the function above), daily daya and monthly data
  return(list(final_plot = plot_function(monthly_data), 
              daily_data = data.frame(time = data$time, precipitation_mm = data$data),
              monthly_data = data.frame(month = monthly_data$month, 
                                        year = monthly_data$year,
                                        precipitation_mm = monthly_data$sum)))
}


shinyApp(ui <- fluidPage(
  # build our UI defining that we want a vertical layout
  verticalLayout(),
  # first we want to display the map
  leafletOutput("mymap", height = 600),
  # add in a conditional message for when calculations are running. 
  conditionalPanel(condition="$('html').hasClass('shiny-busy')",
                   tags$div("Calculating Climatology...",
                            id="loadmessage")),
  # display our precip plot
  plotOutput("plot", width = "100%", height = "300px"),
  # set up download buttons for the user to download data
  downloadButton("downloadDaily", "Download Daily Data (1979 - Present)"),
  downloadButton("downloadMonthly", "Download Monthly Data (1979 - Present)")
),
# now on to the server
server <- function(input, output, session) {
  # this is our map that we will display
  output$mymap <- renderLeaflet({
    leaflet() %>%
      # this is the base map  
      leaflet::addProviderTiles("Stamen.Toner") %>%
      # terrain tiles
      leaflet::addTiles("https://maps.tilehosting.com/data/hillshades/{z}/{x}/{y}.png?key=KZO7rAv96Alr8UVUrd4a") %>%
      # lines and labels
      leaflet::addProviderTiles("Stamen.TonerLines") %>%
      leaflet::addProviderTiles("Stamen.TonerLabels") %>%
      # set default viewing location and zoom
      leaflet::setView(lng = -97.307564, lat = 40.368971, zoom = 4) %>%
      # modify some parameters (what tools are displayed with the map)
      leaflet.extras::addDrawToolbar(markerOptions = drawMarkerOptions(),
                                     polylineOptions = FALSE, polygonOptions = FALSE,
                                     circleOptions = FALSE, rectangleOptions = FALSE,
                                     circleMarkerOptions = FALSE, editOptions = FALSE,
                                     singleFeature = FALSE, targetGroup='draw')
  })
  # Now for our reactive portion which is when the user drops a pin on the map
  observeEvent(input$mymap_draw_new_feature,{
    # create a variable "feature" that will be overwritten when pin drops
    feature = input$mymap_draw_new_feature
    # call our precip function and store the outputs as a variable
    function_out = get_precip(feature$geometry$coordinates[[2]],
                              feature$geometry$coordinates[[1]])
    # render the plot from our function output
    output$plot <- renderPlot({
      function_out[[1]]
    })
    # render the daily data output from our function to a csv for download 
    # with a reactive name (lat long)
    output$downloadDaily <- downloadHandler(
      filename = function() {
        paste("daily_precip_",round(feature$geometry$coordinates[[2]],4),"_",
              round(feature$geometry$coordinates[[1]],4),".csv", sep = "")
      },
      content = function(file) {
        write.csv(function_out$daily_data, file, row.names = FALSE)
      }
    )
    # render the monthly data output again with a reactive name
    output$downloadMonthly <- downloadHandler(
      filename = function() {
        paste("monthly_sum_precip_",round(feature$geometry$coordinates[[2]],4),"_",
              round(feature$geometry$coordinates[[1]],4),".csv", sep = "")
      },
      content = function(file) {
        write.csv(function_out$monthly_data, file, row.names = FALSE)
      }
    )
  })
})

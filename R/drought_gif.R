library(raster)
library(dplyr)
library(spdplyr)
library(RColorBrewer)
library(ggplot2)

#define some functions for clipping
crop_to_umrb = function(x){
  crop(x, extent(UMRB)) %>%
    mask(., UMRB)
}

set_min_max = function(x){
  raster::values(x)[raster::values(x) > 3] = 3 
  raster::values(x)[raster::values(x) < -3] = -3
  return(x)
}

source("/home/zhoylman/drought_indicators/zoran/R/fdates.R")

UMRB = rgdal::readOGR("/home/zhoylman/drought_indicators/shp_kml/larger_extent/outline_umrb.shp")
states = rgdal::readOGR("/home/zhoylman/drought_indicators/shp_kml/states.shp") %>%
  dplyr::filter(STATE_ABBR %in% c("MT","WY","ID","SD","ND"))


time = list.files("/home/zhoylman/temp/spi/archive", pattern = "_60_day") %>%
  fdates() %>%
  as.Date(., format = "%Y%m%d")

#process maps
maps = list.files("/home/zhoylman/temp/spi/archive", pattern = "_60_day", full.names = T) %>%
  lapply(.,raster)%>%
  lapply(., crop_to_umrb) %>%
  lapply(., set_min_max)

color_ramp = c("#8b0000", "#ff0000", "#ffffff", "#0000ff", "#00008b")

# for(i in 1:length(maps)){
#   png(paste0("/home/zhoylman/temp/spi/plots/", i,".png"),width = 600, height = 480, units = "px",)
#   plot(maps[[i]], col = brewer.pal(n = 11, name = "RdYlBu"), zlim = c(-3,3), main = paste0("Standardized Precipitation Index\n",time[i]),
#        xaxt='n', yaxt='n',  frame.plot = FALSE)
#   lines(states)
#   dev.off()
#   print(i)
# }

library(doParallel)
library(foreach)

cl = makeCluster(detectCores()-1)
registerDoParallel(cl)

foreach(i= 1:length(maps)) %dopar% {
  library(dplyr)
  library(raster)
  library(ggplot2)
  
  data = maps[[i]] %>%
    rasterToPoints()%>%
    as.data.frame() %>%
    rename(spi = 3)
  
  plot = ggplot()+
    geom_tile(data = data, aes(x = x, y = y, fill = spi))+
    theme_bw(base_size = 14) +
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          panel.border = element_blank(),
          plot.title = element_text(hjust = 0.5))+
    geom_path(data = states, aes(x = long, y = lat, group = group), fill = "transparent")+
    scale_fill_gradientn("SPI", colours = color_ramp, limits = c(-3,3))+
    ggtitle(paste0("Standardized Precipitation Index\n",time[i]))
  
  ggsave(paste0("/home/zhoylman/temp/spi/plots/", i,".png"), plot, width = 5, height = 3, units = "in", dpi = 68)
  }

stopCluster(cl)

library(purrr)
library(magick)
library(gtools)
setwd("/home/zhoylman/temp/spi/gif")

list.files(path = "/home/zhoylman/temp/spi/plots", pattern = "*.png", full.names = T) %>% 
  mixedsort() %>%
  map(image_read) %>% # reads each path file
  image_join() %>% # joins image
  image_animate(fps=10) %>% # animates, can opt for number of loops
  image_write("spi_gif.gif") # write to current dir

require(rgdal)
require(raster)
library(ggplot2)
library(dplyr)

# The input file geodatabase
fgdb <- "/home/zhoylman/Downloads/mudcreek_1.gdb"

# List all feature classes in a file geodatabase
subset(ogrDrivers(), grepl("GDB", name))
fc_list <- ogrListLayers(fgdb)
print(fc_list)

# Read the feature class
fc <- readOGR(dsn=fgdb,layer="Boundary")
fc_transformed = spTransform(fc, CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"))



sensativity = raster("/home/zhoylman/Downloads/standardized_slope_west_fork (1).tif")
plot(sensativity)
plot(fc_transformed, add = T)

#### ggplot maps #####
plot_raster = function(raster, color_ramp, variable_str){
  raster[raster > quantile(values(raster), 0.99, na.rm = T)] = quantile(values(raster), 0.99, na.rm = T)
  raster[raster < quantile(values(raster), 0.01, na.rm = T)] = quantile(values(raster), 0.01, na.rm = T)
  
  raster.p = data.frame(rasterToPoints(raster))
  colnames(raster.p) = c("x","y","z")
  plot = ggplot(data = raster.p, aes(x = x, y = y, fill = z))+
    ggtitle(variable_str)+
    geom_raster()+
    geom_polygon(data = fc_transformed, aes(x = long, y = lat, group = group), fill = "transparent", colour = "black", size = 0.5)+
    scale_fill_gradientn(expression(),
                         colours=color_ramp)+
    theme_bw(base_size = 16)+
    scale_colour_manual(name = "", values = "black")+
    xlab("Longitude")+
    ylab("Latitude")
  return(plot)
}

#### ggplot maps #####
plot_raster_zoom = function(raster, color_ramp, variable_str){
  raster = raster::mask(raster, fc_transformed)%>%
     raster::crop(.,fc_transformed)
  
  raster[raster > quantile(values(raster), 0.99, na.rm = T)] = quantile(values(raster), 0.99, na.rm = T)
  raster[raster < quantile(values(raster), 0.01, na.rm = T)] = quantile(values(raster), 0.01, na.rm = T)
  
  raster.p = data.frame(rasterToPoints(raster))
  colnames(raster.p) = c("x","y","z")
  plot = ggplot(data = raster.p, aes(x = x, y = y, fill = z))+
    ggtitle(variable_str)+
    geom_raster()+
    geom_polygon(data = fc_transformed, aes(x = long, y = lat, group = group), fill = "transparent", colour = "black", size = 0.5)+
    scale_fill_gradientn(expression(),
                         colours=color_ramp)+
    theme_bw(base_size = 16)+
    scale_colour_manual(name = "", values = "black")+
    xlab("Longitude")+
    ylab("Latitude")
  return(plot)
}


#Plot
color_ramp = c("#0000FFFF", "#00ffff", "#00ff00","#ffff00", "#FFA500" ,"#FF0000FF", "#800000", "#8b0000")
variable_str = "Mean Annual Deficit 1986 - 2015 (mm)"
def_plot = plot_raster((raster("/home/zhoylman/Downloads/def_mean_west_fork.tif")*1000), color_ramp, variable_str)
png(filename="/home/zhoylman/MCO/plots/1.png", width = 10, height = 6, units = "in", res = 300)
def_plot
dev.off()

def_plot_zoom = plot_raster_zoom((raster("/home/zhoylman/Downloads/def_mean_west_fork.tif")*1000), color_ramp, variable_str)
png(filename="/home/zhoylman/MCO/plots/2.png", width = 10, height = 6, units = "in", res = 300)
def_plot_zoom
dev.off()


#sensativity
color_ramp = rev(c("#013220","#00ff00","#ffff00", "#FFA500" ,"#FF0000FF", "#800000", "#8b0000", "#4B0082"))
variable_str = "Ecosystem Sensitivity"
eco_sens_plot = plot_raster(raster("/home/zhoylman/Downloads/standardized_slope_west_fork (1).tif"), color_ramp, variable_str)
png(filename="/home/zhoylman/MCO/plots/5.png", width = 10, height = 6, units = "in", res = 300)
eco_sens_plot
dev.off()

color_ramp = rev(c("#00ff00","#ffff00", "#FFA500" ,"#FF0000FF", "#800000", "#8b0000", "#4B0082"))
eco_sens_plot_zoom = plot_raster_zoom(raster("/home/zhoylman/Downloads/standardized_slope_west_fork (1).tif"), color_ramp, variable_str)
png(filename="/home/zhoylman/MCO/plots/6.png", width = 10, height = 6, units = "in", res = 300)
eco_sens_plot_zoom
dev.off()

#NPP
color_ramp = rev(c("#0000FFFF", "#00ffff", "#00ff00","#ffff00", "#FFA500" ,"#FF0000FF", "#800000", "#8b0000"))
variable_str = "Mean Annual NPP 1986 - 2015 (gC/m2/y)"
npp_plot = plot_raster((raster("/home/zhoylman/Downloads/npp_mean_west_fork.tif")), color_ramp, variable_str)
png(filename="/home/zhoylman/MCO/plots/3.png", width = 10, height = 6, units = "in", res = 300)
npp_plot
dev.off()

npp_plot_zoom = plot_raster_zoom((raster("/home/zhoylman/Downloads/npp_mean_west_fork.tif")), color_ramp, variable_str)
png(filename="/home/zhoylman/MCO/plots/4.png", width = 10, height = 6, units = "in", res = 300)
npp_plot_zoom
dev.off()

#NPP Trend
color_ramp = rev(c("#0000FFFF", "#00ffff", "#00ff00","#ffff00", "#FFA500" ,"#FF0000FF", "#800000", "#8b0000"))
variable_str = "Annual NPP Trend 1986 - 2015 (gC/m2/y2)"
npp_plot = plot_raster((raster("/home/zhoylman/Downloads/npp_trend_mm_west_fork.tif")), color_ramp, variable_str)
png(filename="/home/zhoylman/MCO/plots/9.png", width = 10, height = 6, units = "in", res = 300)
npp_plot
dev.off()

npp_plot_zoom = plot_raster_zoom((raster("/home/zhoylman/Downloads/npp_trend_mm_west_fork.tif")), color_ramp, variable_str)
png(filename="/home/zhoylman/MCO/plots/10.png", width = 10, height = 6, units = "in", res = 300)
npp_plot_zoom
dev.off()

#DEF
color_ramp = c("#ffff00", "#FFA500" ,"#FF0000FF", "#800000", "#8b0000")
variable_str = "Annual Deficit Trend 1986 - 2015 (mm/yr)"
def_plot = plot_raster((raster("/home/zhoylman/Downloads/def_trend_mm_west_fork.tif")), color_ramp, variable_str)
png(filename="/home/zhoylman/MCO/plots/7.png", width = 10, height = 6, units = "in", res = 300)
def_plot
dev.off()

def_plot_zoom = plot_raster_zoom((raster("/home/zhoylman/Downloads/def_trend_mm_west_fork.tif")), color_ramp, variable_str)
png(filename="/home/zhoylman/MCO/plots/8.png", width = 10, height = 6, units = "in", res = 300)
def_plot_zoom
dev.off()
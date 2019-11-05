library(raster)
library(dplyr)
library(ggsn)

load("/media/zhoylman/DATAPART1/EcoHydro/SapDuino_2016/HDI_GLMM.RData")

TWI = raster(read.asciigrid("/media/zhoylman/DATAPART1/EcoHydro/SapDuino_2016/SAGA_Exports/TWI.asc"))

TWI_mask = TWI/TWI
  
april = raster(read.asciigrid("/media/zhoylman/DATAPART1/EcoHydro/SapDuino_2016/SAGA_Exports/april_10m_def_bilinear_clipped.asc")) * TWI_mask

august = raster(read.asciigrid("/media/zhoylman/DATAPART1/EcoHydro/SapDuino_2016/SAGA_Exports/august_10m_def_bilinear_clipped.asc")) * TWI_mask

Deficit = april

object = stack(TWI, Deficit)
names(object) = c("TWI","Deficit")

HDI_april = predict(object, model = HDI_GLMM, type = "response", re.form = NA) * 7

HDI_april[HDI_april < 5] = 1

HDI_april[HDI_april > 5] = NA

plot(HDI_april)



object = stack(TWI, august)
names(object) = c("TWI","Deficit")

HDI_august = predict(object, model = HDI_GLMM, type = "response", re.form = NA) * 0.11

HDI_august[HDI_august < 5] = 1

HDI_august[HDI_august > 5] = NA

plot(HDI_august)

outline = rgdal::readOGR("/media/zhoylman/DATAPART1/EcoHydro/SapDuino_2016/NFEC_Clipping/NFK_Elk_clip.shp")

library(ggplot2)

data = HDI_april %>%
  rasterToPoints() %>%
  as.data.frame()

plot1 = ggplot(data = data)+
  geom_tile(aes(x = x, y = y, fill = layer))+
  theme_bw(base_size = 16)+
  coord_fixed(ratio = 1) +
  geom_path(data = outline, aes(y = lat, x = long))+
  xlab("Easting")+
  ylab("Northing")+
  theme(legend.position = "none")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  ggtitle("Groundwater Recharge (April)")+
  north(data = NULL, location = "topright", x.min = min(data$x), x.max = max(data$x),
        y.min = min(data$y), y.max = max(data$y), symbol = 16, scale = 0.25)+
  scalebar(data = NULL, location = "bottomright", 1, height = 0.02,
           st.dist = 0.05, st.bottom = TRUE, st.size = 3,
           x.min = min(data$x), x.max = max(data$x),
           y.min = min(data$y), y.max = max(data$y), transform = F,dist_unit = "km")

plot1

data = HDI_august %>%
  rasterToPoints() %>%
  as.data.frame()

plot2 = ggplot(data = data)+
  geom_tile(aes(x = x, y = y, fill = layer))+
  theme_bw(base_size = 16)+
  coord_fixed(ratio = 1) +
  geom_path(data = outline, aes(y = lat, x = long))+
  xlab("Easting")+
  ylab("Northing")+
  theme(legend.position = "none")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank())+
  ggtitle("Groundwater Recharge (August)")


plot = cowplot::plot_grid(plot1,plot2, nrow = 1)
ggsave("/home/zhoylman/MCO/plots/sat_plot.png", plot)

library(gstat)
library(sp)
library(spacetime)
library(raster)
library(rgdal)
library(rgeos) 
library(geoR)
library(automap)
library(leaflet)
library(mapview)
library(dplyr)
library(ggplot2)
library(scales)
library(magrittr)
library(gridExtra)
library(RColorBrewer)
setwd("C:/Users/Zeeshan/Desktop/Kriging/version control") 
source("kriging.R")
source("data_read.R")
#-------------------------------Data Reading and CRS Setting-------------------------
data = data.read("avg.txt")

#-------------------------------Data Visualization-----------------------------------
# bubble(data, "X0", col="#00ff0088", maxsize = 3.5 ,main="Ozone concentrations (ppb)", xlab="Longitude", ylab="Latitude")

data %>% as.data.frame %>%
  ggplot(aes(x = Longitude, y = Latitude)) + geom_point(aes(size=X0), color="blue", alpha=2/4) +
  coord_equal() + theme_bw() +
  labs(#subtitle="Area Vs Population",
    y="Latitude",
    x="Logitude",
    title="Ozone Concentration (ppb)",
    caption = "Source: Georgia Dataset")

mapview(data, zcol = "X0", cex = 8)
mapview(data, zcol = "X0") + mapview(data_krige$Grid_SP, cex = 1)

#-------------------------------Performing Kriging-----------------------------------
data_krige = kriging(data)


#-------------------------------Variogram Visualization------------------------------
plot(data_krige$Data.Variogram, pch=19, col="black", ylab=expression("Semivariance("*gamma*")"),
     xlab="Distance (m)", main="Ozone concentrations (ppb)")

plot(data_krige$Data.Variogram, model=data_krige$Fitted_Variogram, pch=19, col="black", main="Fitted Model",
     ylab=expression("Semivariance ("*gamma*")"), xlab="Distance (m)")


#-------------------------------Kriging Visualization------------------------------
plot(data_krige$Data.Krige["var1.pred"], main="Ozone Kriging Prediction", col = rev(heat.colors(60)))
points(data_krige$Data, pch=4, cex=0.5)

plot(data_krige$Data.Krige["var1.var"], main="Kriging Variance", col = rev(heat.colors(50)))
points(data_krige$Data, pch=4, cex=0.5)
mapview(data_krige$Data.Krige["var1.pred"], alpha.regions = 0.7, col.regions = rev(heat.colors(250))) + mapview(data, zcol = "X0", cex = 1, alpha.regions = 1/4)
mapview(data_krige$Data.Krige["var1.var"], alpha.regions = 0.6, col.regions = rev(heat.colors(250))) + mapview(data, zcol = "X0", cex = 1, alpha.regions = 1/4)

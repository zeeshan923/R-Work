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
setwd("C:/Users/Zeeshan/Desktop/R Examples/version control") 
data <- read.table("avg.txt", header=T)
#dists <- dist(data[,3:4]) 
#summary(dists)
coordinates(data)=~Latitude+Longitude #make spatialpoint dataframe

proj4string(data)=CRS("+init=epsg:4326") #set coordinate reference system
showDefault(data) # show the details of data
bubble(data, "X0", col="#00ff0088", maxsize = 3.5 ,main="Ozone concentrations (ppb)", xlab="Longitude", ylab="Latitude")
mapview(data) 



#-------------------------------SETTING GRID FOR KRIGING-----------------------------

data.grid = GridTopology(c(-9541787 ,3616144), c(8000,10000), c(70,56)) #making grid
grid.sp = SpatialPoints(data.grid, proj4string = CRS("+init=epsg:3857")) #convrting to spatial point
mapview(data) + mapview(grid.sp) #overlay of grid over the stations
bound = bbox(grid.sp) #boundig box of grid spatial object
cropped_grid = crop(grid.sp, bound)
# plot(cropped_gridpoints)
croppred_grid.sp = SpatialPixels(cropped_grid)
plot(croppred_grid.sp)



#-------------------------------SETTING VARIOGRAM------------------------------------

#transforming data into Mector Projection
data = spTransform(data, CRSobj = CRS("+init=epsg:3857")) 
#calculate variogram with linear function
data.variogram=variogram(X0~1, data , width = 40000, cutoff = 220000) 
#summary(zn.vgm)
#showDefault(zn.vgm)
plot(data.variogram, pch=19, col="black", ylab=expression("Semivariance("*gamma*")"),
                    xlab="Distance (m)", main="Ozone concentrations (ppb)")
# fit by eye to get initial values
SILL=50 # y value where y levels off
RANGE=120000 # x value where y levels off
NUGGET=4 # intercept with y
variogram_model <- vgm(SILL-NUGGET, "Sph",RANGE,NUGGET) # define manual variogram model
plot(zn.vgm, model=variogram_model, pch=19, col="black", main="Manual Model",
     ylab=expression("Semivariance ("*gamma*")"), xlab="Distance (m)")


fitted_variogram <- fit.variogram(data.variogram, variogram_model) #fitting the variogram
plot(zn.vgm, model=fitted_variogram, pch=19, col="black", main="Fitted Model",
     ylab=expression("Semivariance ("*gamma*")"), xlab="Distance (m)")




#--------------------------------KRIGING--------------------------------------

lzn.krigedauto <- autoKrige((X0) ~ 1, data, croppred_grid.sp) #perform auto kriging
summary(lzn.krigedauto)
plot(lzn.krigedauto)



kr = krige((X0) ~ 1, data,  croppred_grid.sp, fitted_variogram) #perform manual kriging
summary(kr)
plot(kr, main="Ozone Kriging Prediction")



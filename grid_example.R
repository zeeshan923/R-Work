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
library(dplyr) # for "glimpse"
library(ggplot2)
library(scales) # for "comma"
library(magrittr)
setwd("C:/Users/Zeeshan/Desktop/R Examples")    
data <- read.table("avg.txt", header=T)
#dists <- dist(data[,3:4]) 
#summary(dists)
coordinates(data)=~Latitude+Longitude

proj4string(data)=CRS("+init=epsg:4326")
showDefault(data)
bubble(data, "X0", col="#00ff0088", main="Ozone concentrations (ppb)")
mapview(data)


#SETTING GRID FOR KRIGING
# ori <- SpatialPoints(cbind(-85.71533, 30.87205), proj4string =  CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
# ori_t <- spTransform(ori, CRSobj = CRS("+init=epsg:3857"))
# showDefault(ori_t)
# x_ori <- round(coordinates(ori_t)[1, 1]/100) * 100
# y_ori <- round(coordinates(ori_t)[1, 2]/100) * 100
# x_cell <- 290#290
# y_cell <- 180#180
# 
# # Define the resolution to be 1000 meters
# cell_size <- 1000
# 
# # Create the extent
# ext <- extent(x_ori, x_ori + (x_cell * cell_size), y_ori, y_ori + (y_cell * cell_size))
# # Initialize a raster layer
# ras <- raster(ext)
# 
# # Set the resolution to be
# res(ras) <- c(cell_size, cell_size)
# ras[] <- 0
# showDefault(ras)
# # Project the raster
# projection(ras) <- CRS("+init=epsg:3857")
# 
# # Create interactive map
# mapview(data) + mapview(ras)
# 
# # Save the raster layer
# writeRaster(ras, filename = "ras.tif", format="GTiff", overwrite=TRUE)
# # Convert to spatial pixel
# st_grid <- rasterToPoints(ras, spatial = TRUE)
# gridded(st_grid) <- TRUE
# st_grid <- as(st_grid, "SpatialPixels")
# showDefault(st_grid)
# plot(st_grid)

grid1 = GridTopology(c(-9541787 ,3616144), c(8000,10000), c(70,56))

grid.sp = SpatialPoints(grid1, proj4string = CRS("+init=epsg:3857"))
viewExtent(grid.sp) + mapview(data) + plot(grid.sp)
mapviewColors(data, colors = mapviewGetOption("vector.palette"), at = grid.sp,
              na.color = mapviewGetOption("na.color"))
mapview(data) + mapview(grid.sp)
showDefault(grid.sp)
plot(grid.sp)
bound = bbox(grid.sp)
cropped_gridpoints = crop(grid.sp, bound)
# plot(cropped_gridpoints)
spgrid = SpatialPixels(cropped_gridpoints)
plot(spgrid)


#SETTING VARIOGRAM
data = spTransform(data, CRSobj = CRS("+init=epsg:3857"))
zn.vgm=variogram(X0~1, data) # , width = 40000, cutoff = 220000
#summary(zn.vgm)
#showDefault(zn.vgm)
plot(zn.vgm, pch=19, col="black", ylab=expression("Semivariance("*gamma*")"), xlab="Distance (m)",
     main="log Ozone concentrations (ppb)")
# fit by eye to get initial values
SILL=50 # y value where y levels off
RANGE=120000 # x value where y levels off
NUGGET=4 # intercept with y
m <- vgm(SILL-NUGGET, "Sph",RANGE,NUGGET) # define manual model
plot(zn.vgm, model=m, pch=19, col="black", main="Manual Model",
     ylab=expression("Semivariance ("*gamma*")"), xlab="Distance (m)")


fm <- fit.variogram(zn.vgm, m)
plot(zn.vgm, model=fm, pch=19, col="black", main="Fitted Model",
     ylab=expression("Semivariance ("*gamma*")"), xlab="Distance (m)")





#KRIGING
lzn.krigedauto <- autoKrige((X0) ~ 1, data, spgrid)
summary(lzn.krigedauto)
plot(lzn.krigedauto)



kr = krige((X0) ~ 1, data,  spgrid, fm)
summary(kr)
plot(kr, main="fskdfh")

lzn.kriged <- krige(log(Av8top) ~ X+Y, data, spgrid, model=fm)

plot(lzn.kriged)

lzn.krigedauto %>% as.data.frame %>%
  ggplot(aes(x=Lon, y=Lat)) + geom_tile(aes(fill=var1.pred)) + coord_equal() +
  scale_fill_gradient(low = "yellow", high="red") +
  scale_x_continuous(labels=comma) + scale_y_continuous(labels=comma) +
  theme_bw()

# grid = GridTopology(c(-118.71284, 33.4603), c(0.0542568, 0.0286054), c(50,50), proj4string =  CRS("+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"))
# 
# ak = autoKrige(data$Av8top~1, data, grid)

# bluepal=colorRampPalette(c("azure1", "steelblue4"))
# brks=c(4.5,5,5.5,6,6.5,7,7.5,8)
# mycol=bluepal(length(brks)-1)


# max_lat = max(data$Latitude)
# min_lat = min(data$Latitude)
# max_log = max(data$Longitude)
# min_log = min(data$Longitude)
# 
# gridy = seq(max_lat,min_lat-0.22, by = -0.22 )
# gridx = seq(min_log, max_log+0.19, by = 0.19)
# 
# coordinates(meuse.grid)=~x+y
# proj4string(meuse.grid)=CRS("+init=epsg:28992")
# meuse.grid=as(meuse.grid,"SpatialPixelsDataFrame")
# plot(meuse.grid)
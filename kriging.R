
kriging = function(data)
{
  #-------------------------------SETTING GRID FOR KRIGING-----------------------------
  
  # Set the origin
  ori <- SpatialPoints(cbind(-86.25298, 31.2855), proj4string =  CRS("+init=epsg:4326")) 
  # Convert the projection of ori
  # Use EPSG: 3857 (Spherical Mercator)
  ori_t <- spTransform(ori, CRSobj = CRS("+init=epsg:3857"))

  # The origin has been rounded to the nearest 100
  x_ori <- round(coordinates(ori_t)[1, 1]/100) * 100
  y_ori <- round(coordinates(ori_t)[1, 2]/100) * 100
  
  #transforming data into Mector Projection
  data = spTransform(data, CRSobj = CRS("+init=epsg:3857"))
  
  data.grid = GridTopology(cellcentre.offset = c(x_ori ,y_ori), cellsize = c(10000,10000), cells.dim = c(57,57)) #making grid
  grid.sp = SpatialPoints(data.grid, proj4string = CRS("+init=epsg:3857")) #convrting to spatial point
  mapview(data, zcol = "X0") + mapview(grid.sp, cex = 1) #overlay of grid over the stations
  bound = bbox(grid.sp) #boundig box of grid spatial object
  cropped_grid = crop(grid.sp, bound)
  # plot(cropped_gridpoints)
  croppred_grid.sp = SpatialPixels(cropped_grid)
  # plot(croppred_grid.sp)
  
  
  
  
  #-------------------------------SETTING VARIOGRAM------------------------------------
  
  
  #calculate variogram with linear function
  data.variogram = variogram(X0 ~ 1, data, width = 40000, cutoff = 220000)
  plot(data.variogram, pch=19, col="black", ylab=expression("Semivariance("*gamma*")"),
       xlab="Distance (m)", main="Ozone concentrations (ppb)")
  # fit by eye to get initial values
  SILL=50 # y value where y levels off
  RANGE=120000 # x value where y levels off
  NUGGET=4 # intercept with y
  variogram_model <- vgm(SILL-NUGGET, "Sph",RANGE,NUGGET) # define manual variogram model
  plot(data.variogram, model=variogram_model, pch=19, col="black", main="Manual Model",
       ylab=expression("Semivariance ("*gamma*")"), xlab="Distance (m)")
  
  
  fitted_variogram <- fit.variogram(data.variogram, variogram_model) #fitting the variogram
  plot(data.variogram, model=fitted_variogram, pch=19, col="black", main="Fitted Model",
       ylab=expression("Semivariance ("*gamma*")"), xlab="Distance (m)")
  
  
  
  
  #--------------------------------KRIGING--------------------------------------
  
  data.krige = krige((X0) ~ 1, data,  croppred_grid.sp, fitted_variogram) #perform manual kriging
  # summary(data.krige)
  # plot(data.krige["var1.pred"], main="Ozone Kriging Prediction", col = rev(heat.colors(50)))
  # points(data, pch=4, cex=0.5)
  # plot(data.krige["var1.var"], main="Kriging Variance", col = rev(heat.colors(50)))
  # points(data, pch=4, cex=0.5)
  
  return_list = list(Data = data, Data.Krige = data.krige, Data.Variogram = data.variogram, Fitted_Variogram = fitted_variogram, Grid_SP = grid.sp)
  return(return_list)
  
  
  # data.auto_krige <- autoKrige((X0) ~ 1, data, croppred_grid.sp) #perform auto kriging
  # summary(data.auto_krige)
  # plot(data.auto_krige)
  
  
  # spplot(data.krige, zcol = "var1.pred", main="Ozone Kriging Prediction", col.regions = rev(heat.colors(50)))
  # spplot(data.krige, zcol = "var1.var", main="Kriging Variance", col.regions = rev(heat.colors(50)))
  # points(data, pch=4, cex=1, col = 'green')
  
  
  
  
  
  # data.krige %>% as.data.frame %>%
  #   ggplot(aes(x= s1, y=s2)) + geom_tile(aes(fill=var1.pred)) + coord_equal() +
  #   scale_fill_gradient2(low = "green", mid = "yellow", high = "red") +
  #   scale_x_continuous(labels=comma) + scale_y_continuous(labels=comma) +
  #   theme_bw()
  # points(data, pch=4, cex=0.5)
  
  
}


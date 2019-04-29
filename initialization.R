initialization = function()
{
  # Initial Homogenous Transformation
  H_initial = matrix(
    c(0.7071068, -0.7071068, 0, -9601600,
      0.7071068, 0.7071068, 0, 3669900,
      0, 0, 1, 5,
      0, 0, 0, 1),
    nrow=4, 
    ncol=4,
    byrow = TRUE)
  
  #%% Initialization of points and variables
  search_points = data.frame(x=c(),y=c())
  points_temp = data.frame(x=c(),y=c())
  del_poll = 0
  pmax = 0
  i = 1
  ttl_max = 2
  Y = data_krige$Data.Krige@data$var1.pred
  dat = data_krige$Data.Krige@coords
  plot(data_krige$Data.Krige["var1.pred"], main="Ozone Kriging Prediction", col = rev(heat.colors(60)))
  
  p1 = point(H_initial)
  search_points = data.frame(x=c(p1[1]),y=c(p1[2]))
  grid_index = Grid_Index(dat, x_value = p1[1], y_value =  p1[2])
  p1_poll = getPollution(grid_index, Y)
  # % Initial run            
  H1 = run(H_initial)
  p2 = point(H1)
  grid_index = Grid_Index(dat, x_value = p2[1], y_value =  p2[2])
  p2_poll = getPollution(grid_index, Y)
  points_temp = data.frame(x=c(p2[1]),y=c(p2[2]))
  search_points <- rbind(search_points, points_temp)
}
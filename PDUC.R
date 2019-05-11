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
library(useful)
library(shape)
library(oce)
setwd("C:/Users/Zeeshan/Desktop/Kriging/version control") 
source("kriging.R")
source("data_read.R")
#-------------------------------Data Reading and CRS Setting-------------------------
data = data.read("avg1.txt")

#-------------------------------Data Visualization-----------------------------------
# bubble(data, "X0", col="#00ff0088", maxsize = 3.5 ,main="Ozone concentrations (ppb)", xlab="Longitude", ylab="Latitude")

# data %>% as.data.frame %>%
#   ggplot(aes(x = Longitude, y = Latitude)) + geom_point(aes(size=X0), color="blue", alpha=2/4) +
#   coord_equal() + theme_bw() +
#   labs(#subtitle="Area Vs Population",
#     y="Latitude",
#     x="Logitude",
#     title="Ozone Concentration (ppb)",
#     caption = "Source: Georgia Dataset")



# -------------------------------Performing Kriging-----------------------------------
data_krige = kriging(data)

mapview(data, zcol = "X0", cex = 6)
mapview(data, zcol = "X0", cex = 6) + mapview(data_krige$Grid_SP, cex = 1)
grid_bound = bbox(data_krige$Grid_SP)
# 
# #-------------------------------Variogram Visualization------------------------------
# plot(data_krige$Data.Variogram, pch=19, col="black", ylab=expression("Semivariance("*gamma*")"),
#      xlab="Distance (m)", main="Ozone concentrations (ppb)")
# 
# plot(data_krige$Data.Variogram, model=data_krige$Fitted_Variogram, pch=19, col="black", main="Fitted Model",
#      ylab=expression("Semivariance ("*gamma*")"), xlab="Distance (m)")
# 
# #
# #-------------------------------Kriging Visualization------------------------------
plot(data_krige$Data.Krige["var1.pred"], main="Ozone Kriging Prediction", col = rev(heat.colors(60)))
points(data_krige$Data, pch=4, cex=1)

# plot(data_krige$Data.Krige["var1.var"], main="Kriging Variance", col = rev(heat.colors(50)))
# points(data_krige$Data, pch=4, cex=0.5)
# mapview(data_krige$Data.Krige["var1.pred"], alpha.regions = 0.7, col.regions = rev(heat.colors(250))) + mapview(data, zcol = "X0", cex = 1, alpha.regions = 1/4)
# mapview(data_krige$Data.Krige["var1.var"], alpha.regions = 0.6, col.regions = rev(heat.colors(250))) + mapview(data, zcol = "X0", cex = 1, alpha.regions = 1/4)
# 



source("point.R")
source("run.R")
source("tumble.R")
source("getPollution.R")
source("Grid_Index.R")
source("rotate.R")



#%% Initialization of points and variables----------------------------------------

search_points = data.frame(x=c(),y=c())
# search_points2 = data.frame(x=c(),y=c())
points_temp = data.frame(x=c(),y=c())
del_poll = 0
pmax = 0
i = 1
ttl_max = 2
Y = data_krige$Data.Krige@data$var1.pred
dat = data_krige$Data.Krige@coords
# Initial Homogenous Transformation
H_initial = matrix(
  c(0.7071068, -0.7071068, 0, grid_bound[1,1],
    0.7071068, 0.7071068, 0, grid_bound[2,1],
    0, 0, 1, 5,
    0, 0, 0, 1),
  nrow=4, 
  ncol=4,
  byrow = TRUE)
plot(data_krige$Data.Krige["var1.pred"], main="Ozone Kriging Prediction", col = rev(heat.colors(60)))


p1 = point(H_initial)
search_points = data.frame(x=c(p1[1]),y=c(p1[2]))
grid_index = Grid_Index(dat, x_value = p1[1], y_value =  p1[2])
p1_poll = getPollution(grid_index, Y)
# % Initial run            
H = run(H_initial)
p2 = point(H)
grid_index = Grid_Index(dat, x_value = p2[1], y_value =  p2[2])
p2_poll = getPollution(grid_index, Y)
points_temp = data.frame(x=c(p2[1]),y=c(p2[2]))
search_points <- rbind(search_points, points_temp)
Arrows(x0 = p1[1], y0 = p1[2], x1 = p2[1], y1 = p2[2], arr.type = "triangle", arr.length = 0.08, 
       arr.width = 0.06, arr.col = '#000080', lcol = '#000080')
# lines(search_points)+points(search_points, pch=19, cex=1)

isSearching = TRUE
main_loop_count = 1

while (main_loop_count <= 2)
{
  if(main_loop_count >= 2)
  {
    H_rotate_neg45 = matrix(c(0.7071068, 0.7071068, 0, 0,
                                -0.7071068, 0.7071068, 0, 0,
                                0, 0, 1, 0,
                                0, 0, 0, 1),
                              nrow=4,
                              ncol=4,
                              byrow = TRUE)
    H = H_explore
    p1 = point(H)
    H = H%*%H_rotate_neg45
    points_temp = data.frame(x=c(p[1]),y=c(p[2]))
    search_points <- rbind(search_points, points_temp)
    grid_index = Grid_Index(dat, x_value = p[1], y_value =  p[2])
    p1_poll = getPollution(grid_index, Y)
    H = run(H)
    p2 = point(H)
    grid_index = Grid_Index(dat, x_value = p[1], y_value =  p[2])
    p2_poll = getPollution(grid_index, Y)
    points_temp = data.frame(x=c(p[1]),y=c(p[2]))
    search_points <- rbind(search_points, points_temp)
    Arrows(x0 = p1[1], y0 = p1[2], x1 = p2[1], y1 = p2[2], arr.type = "triangle", arr.length = 0.08, arr.width = 0.06,
           arr.col = '#000080', lcol = '#000080')
    p1 = p2

    # lines(x = p1) +points(search_points2, pch=19, cex=1)
  }
 
  while (isSearching)
  {
    print('Search Phase')
    p_boundary = TRUE
    p_visited = TRUE
    
    del_poll = p2_poll - p1_poll
    p1_poll = p2_poll
    
    if (del_poll >= 0)
    {
      print('pol >= 0')
      ttl = 0
      p1 = point(H)
      H= run(H)
      p2 = point(H)
      pmax = p2
      pol_max=p2_poll
      Hmax=H
      
      
      points_temp = data.frame(x=c(p2[1]),y=c(p2[2]))
      search_points <- rbind(search_points, points_temp)
      grid_index = Grid_Index(dat, x_value = p2[1], y_value =  p2[2])
      p2_poll = getPollution(grid_index, Y)
      Arrows(x0 = p1[1], y0 = p1[2], x1 = p2[1], y1 = p2[2], arr.type = "triangle", arr.length = 0.08, arr.width = 0.06,
             arr.col = '#000080', lcol = '#000080')
      # Poll_mat[p[1]/10,p[2]/10]=p2_poll;
      # lines(search_points)+points(search_points, pch=19, cex=1)
    }else
    {
      print('Search Phase poll < 0')
      p1 = point(H)
      H = tumble(H)
      p2 = point(H)
      ttl = ttl + 1
      
      # % Get pollution at point p
      grid_index = Grid_Index(dat, x_value = p2[1], y_value =  p2[2])
      p2_poll = getPollution(grid_index, Y)
      # Poll_mat(p(1)/10,p(2)/10)=p2_poll
      points_temp = data.frame(x=c(p2[1]),y=c(p2[2]))
      search_points <- rbind(search_points, points_temp)
      Arrows(x0 = p1[1], y0 = p1[2], x1 = p2[1], y1 = p2[2], arr.type = "triangle", arr.length = 0.08, arr.width = 0.06,
             arr.col = '#000080', lcol = '#000080')
      # lines(search_points)+points(search_points, pch=19, cex=1)
    }
    # % Check if ttl is expired
    if (ttl >= ttl_max)
    {
      isSearching = FALSE
      isExploring = TRUE
    }
  }
  
  
  
  # Explore Initialization --------------------------------------------------
  if(main_loop_count <= 2)
  {
    explore_points = data.frame(x=c(),y=c())
    
    # % Initial rin in exploration phase
    p1 = point(Hmax)
    Arrows(x0 = p2[1], y0 = p2[2], x1 = p1[1], y1 = p1[2], arr.type = "triangle", arr.length = 0.08, 
           arr.width = 0.06, arr.col = 'black', lcol = 'black')
    H_explore_initial=run(Hmax)
    
    # % Homogenous Transformation at angle of 135
    theta = pi/4 #135*pi/180
    H_rotate_135 = matrix(c(cos(theta), -sin(theta), 0, 0,
                            sin(theta), cos(theta), 0, 0,
                            0, 0, 1, 0,
                            0, 0, 0, 1),
                          nrow=4, 
                          ncol=4,
                          byrow = TRUE)
    
    # % Rotate and e
    H = H_explore_initial%*%H_rotate_135
    p2=point(H)
    points_temp = data.frame(x=c(p2[1]),y=c(p2[2]))
    explore_points <- rbind(explore_points, points_temp)
    grid_index = Grid_Index(dat, x_value = p2[1], y_value =  p2[2])
    poll_ex = getPollution(grid_index, Y)
    # lines(explore_points)+points(explore_points, pch=4, cex=1)
    Arrows(x0 = p1[1], y0 = p1[2], x1 = p2[1], y1 = p2[2], arr.type = "triangle", arr.length = 0.08, 
           arr.width = 0.06, arr.col = 'black', lcol = 'black')
    H_rotate_90 = matrix(c(0, -1, 0, 0,
                           1, 0, 0, 0,
                           0, 0, 1, 0,
                           0, 0, 0, 1),
                         nrow = 4,
                         ncol = 4,
                         byrow = TRUE)
    k=0
    j=2
    count = 0
    arr = c(2,2,2,2,2,2:5, 5,5, 5, 5, 5, 5, 5, 5, 5, 5, 5)
  }
  
  # Exploration Phase -------------------------------------------------------
  isExploring = TRUE
  while(isExploring)
  {
    print('Explore Phase')
    p_boundary = TRUE
    p_explored = TRUE
    H = H%*%H_rotate_90
    k = k+1
    
    
    
    for(i in 1:j)
    {
      p1 = point(H)
      H = run(H)
      p2 = point(H)
      
      Arrows(x0 = p1[1], y0 = p1[2], x1 = p2[1], y1 = p2[2], arr.type = "triangle", arr.length = 0.08, 
             arr.width = 0.04, arr.col = '#006400', lcol = '#006400')
      points_temp = data.frame(x=c(p2[1]),y=c(p2[2]))
      explore_points <- rbind(explore_points, points_temp)
      grid_index = Grid_Index(dat, x_value = p2[1], y_value =  p2[2])
      poll_ex = getPollution(grid_index, Y)
      if(poll_ex > pol_max + 2.6)
      {
        print("exploration breaks")
        break
      }
    }
    if(k%%2==0)
    {
      count = count + 1
      j = j+arr[count]
    }
    # lines(explore_points)+points(explore_points, pch=4, cex=0.5)
    
    if(poll_ex > pol_max + 2.6)
    {
      H_explore = H
      p1_poll = poll_ex
      print("exploration breaks")
      isSearching = TRUE
      isExploring = FALSE
      break
    }
  }
  main_loop_count = main_loop_count + 1
}


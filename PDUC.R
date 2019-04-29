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



# -------------------------------Performing Kriging-----------------------------------
data_krige = kriging(data)

mapview(data, zcol = "X0", cex = 6)
mapview(data, zcol = "X0", cex = 6) + mapview(data_krige$Grid_SP, cex = 1)

#-------------------------------Variogram Visualization------------------------------
plot(data_krige$Data.Variogram, pch=19, col="black", ylab=expression("Semivariance("*gamma*")"),
     xlab="Distance (m)", main="Ozone concentrations (ppb)")

plot(data_krige$Data.Variogram, model=data_krige$Fitted_Variogram, pch=19, col="black", main="Fitted Model",
     ylab=expression("Semivariance ("*gamma*")"), xlab="Distance (m)")

#
#-------------------------------Kriging Visualization------------------------------
plot(data_krige$Data.Krige["var1.pred"], main="Ozone Kriging Prediction", col = rev(heat.colors(60)))
points(data_krige$Data, pch=4, cex=0.5)

plot(data_krige$Data.Krige["var1.var"], main="Kriging Variance", col = rev(heat.colors(50)))
points(data_krige$Data, pch=4, cex=0.5)
mapview(data_krige$Data.Krige["var1.pred"], alpha.regions = 0.7, col.regions = rev(heat.colors(250))) + mapview(data, zcol = "X0", cex = 1, alpha.regions = 1/4)
mapview(data_krige$Data.Krige["var1.var"], alpha.regions = 0.6, col.regions = rev(heat.colors(250))) + mapview(data, zcol = "X0", cex = 1, alpha.regions = 1/4)




source("point.R")
source("run.R")
source("tumble.R")
source("getPollution.R")
source("Grid_Index.R")
source("rotate.R")

# Initial Homogenous Transformation
H_initial = matrix(
  c(0.7071068, -0.7071068, 0, -9601600,
    0.7071068, 0.7071068, 0, 3669900,
    0, 0, 1, 5,
    0, 0, 0, 1),
  nrow=4, 
  ncol=4,
  byrow = TRUE)

#%% Initialization of points and variables----------------------------------------

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

lines(search_points)+points(search_points, pch=19, cex=1)

isSearching = TRUE

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
    H2= run(H1)
    p2 = point(H2)
    pmax = p2
    pol_max=p2_poll
    Hmax=H2
    H1 = H2

    
    points_temp = data.frame(x=c(p2[1]),y=c(p2[2]))
    search_points <- rbind(search_points, points_temp)
    grid_index = Grid_Index(dat, x_value = p2[1], y_value =  p2[2])
    p2_poll = getPollution(grid_index, Y)
    # Poll_mat[p2[1]/10,p2[2]/10]=p2_poll;
    lines(search_points)+points(search_points, pch=19, cex=1)
  }else
  {
    print('Search Phase poll < 0')
    H2 = tumble(H1)
    p2 = point(H2)
    ttl = ttl + 1
    H1 = H2
    
    # % Get pollution at point p2
    grid_index = Grid_Index(dat, x_value = p2[1], y_value =  p2[2])
    p2_poll = getPollution(grid_index, Y)
    # Poll_mat(p2(1)/10,p2(2)/10)=p2_poll
    points_temp = data.frame(x=c(p2[1]),y=c(p2[2]))
    search_points <- rbind(search_points, points_temp)
    lines(search_points)+points(search_points, pch=19, cex=1)
  }
  # % Check if ttl is expired
  if (ttl >= ttl_max)
  {
    isSearching = FALSE
    isExploring = TRUE
  }
}



# Explore Initialization --------------------------------------------------
explore_points = data.frame(x=c(),y=c())
# % Initial rin in exploration phase
H_explore_initial=run(Hmax);

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
p_ex=point(H)
points_temp = data.frame(x=c(p_ex[1]),y=c(p_ex[2]))
explore_points <- rbind(explore_points, points_temp)
grid_index = Grid_Index(dat, x_value = p_ex[1], y_value =  p_ex[2])
poll_ex = getPollution(grid_index, Y)
lines(explore_points)+points(explore_points, pch=4, cex=1)

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
arr = c(2,2,2,2,2:5, 5,5, 5, 5, 5, 5, 5, 5, 5, 5, 5)
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
    H = run(H)
    p_ex = point(H)
    points_temp = data.frame(x=c(p_ex[1]),y=c(p_ex[2]))
    explore_points <- rbind(explore_points, points_temp)
    grid_index = Grid_Index(dat, x_value = p_ex[1], y_value =  p_ex[2])
    poll_ex = getPollution(grid_index, Y)
    # lines(p_ex)+points(p_ex, pch=4, cex=0.5)
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
  lines(explore_points)+points(explore_points, pch=4, cex=0.5)
  
  if(poll_ex > pol_max + 2.6)
  {
    print("exploration breaks")
    break
  }


  

  # if (del_poll <= 0)
  # {
  #   for (run_c in 1:r)
  #   {
  #     p_boundary = true;
  #     p_explored = true;    
  #     H= run(H);
  #     p_ex = point(H);
  #   }
  #   # % Check if the point is already explored
  #   for (j = 1:length(ex_points2))
  #   {
  #     if (p_ex == ex_points2{j})
  #     {
  #       isExploring = false;
  #       isSearching = true;
  #     }
  #   }
  # }
}

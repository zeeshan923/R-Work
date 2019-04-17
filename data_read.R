
#-------------------------------Data Reading and CRS Setting-------------------------
data.read = function(filename)
{
  data <- read.table(file = filename, header = T)
  coordinates(data)=~Longitude+Latitude #make spatialpoint dataframe
  proj4string(data)=CRS("+init=epsg:4326") #set coordinate reference system
  # showDefault(data) # show the details of data
  return(data)
}

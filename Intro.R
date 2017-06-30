library(readr)
library(ggplot2)
library(dplyr)
library(raster)
library(sp)
library(rgdal)
library(ggmap)

hzip15 <- read_csv('./dash-austin-crime-report-2015/2014_Housing_Market_Analysis_Data_by_Zip_Code.csv')
crime15 <- read_csv('./dash-austin-crime-report-2015/Annual_Crime_Dataset_2015.csv')
dat <- read_csv('./dash-austin-crime-report-2015/Crime_Housing_Joined.csv')

amap <- get_map(location = 'Austin, Texas')

#Need to transform the coordinate reference system
#See: https://gis.stackexchange.com/questions/45263/converting-geographic-coordinate-system-in-r
d <- data.frame(lon = dat$X_Coordinate, lat = dat$Y_Coordinate)
d %>%
  filter(complete.cases(.)) -> d

coordinates(d) <- c("lon", "lat")
proj4string(d) <- CRS("+init=esri:102739")
CRS.new <- CRS("+init=epsg:4326")

d_trans <- spTransform(d, CRS.new)

dat_coords <- data.frame(lon = d_trans@coords[,1], lat = d_trans@coords[,2])

ggmap(amap)+
  geom_point(aes(lon, lat), data = dat_coords)

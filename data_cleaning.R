library(readr)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(sp)
library(ggmap)
library(corrplot)

dat <- read_csv('./dash-austin-crime-report-2015/Crime_Housing_Joined.csv',
                col_types = cols(Zip_Code_Crime = col_character()))

#Transform Coordinates
data.frame(lon = dat$X_Coordinate, lat = dat$Y_Coordinate) %>%
  filter(complete.cases(.)) -> df_coords

coordinates(df_coords) <- c("lon", "lat")
proj4string(df_coords) <- CRS("+init=esri:102739")
CRS.new <- CRS("+init=epsg:4326")

df_coords_trans <- spTransform(df_coords, CRS.new)

dat_coords <- data.frame(lon = df_coords_trans@coords[,1], lat = df_coords_trans@coords[,2])

dat %>%
  filter(!(is.na(X_Coordinate) | is.na(Y_Coordinate))) %>%
  mutate(X_Coordinate = df_coords_trans@coords[ ,1],
         Y_Coordinate = df_coords_trans@coords[ ,2]) -> df

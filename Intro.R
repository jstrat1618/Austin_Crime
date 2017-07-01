library(readr)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(sp)
library(ggmap)

hzip15 <- read_csv('./dash-austin-crime-report-2015/2014_Housing_Market_Analysis_Data_by_Zip_Code.csv')
crime15 <- read_csv('./dash-austin-crime-report-2015/Annual_Crime_Dataset_2015.csv')
dat <- read_csv('./dash-austin-crime-report-2015/Crime_Housing_Joined.csv',
                col_types = cols(Zip_Code_Crime = col_character()))

#Need to transform the coordinate reference system
#See: https://gis.stackexchange.com/questions/45263/converting-geographic-coordinate-system-in-r

data.frame(lon = dat$X_Coordinate, lat = dat$Y_Coordinate) %>%
  filter(complete.cases(.)) -> df_coords

coordinates(df_coords) <- c("lon", "lat")
proj4string(df_coords) <- CRS("+init=esri:102739")
CRS.new <- CRS("+init=epsg:4326")

df_coords_trans <- spTransform(df_coords, CRS.new)

dat_coords <- data.frame(lon = df_coords_trans@coords[,1], lat = df_coords_trans@coords[,2])

amap10 <- get_map(location = 'Austin, Texas', zoom = 10)
amap11 <- get_map(location = 'Austin, Texas', zoom = 11)

ggmap(amap11)+
  geom_point(aes(lon, lat), data = dat_coords)

dat %>%
  rename(`Zip` = Zip_Code_Crime) %>%
  group_by(Zip) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = Zip, y = Count))+
    geom_bar(stat = 'identity')


dat %>%
  rename(`Zip` = Zip_Code_Crime) %>%
  group_by(Zip) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = reorder(Zip, Count), y = Count))+
    geom_bar(stat = 'identity', width = 0.5)+
    labs(xlab('Zip'))+
    coord_flip()+
    theme_calc()


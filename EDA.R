library(readr)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(sp)
library(ggmap)
library(corrplot)

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
  filter(!(is.na(X_Coordinate) | is.na(Y_Coordinate))) %>%
  mutate(X_Coordinate = df_coords_trans@coords[ ,1],
         Y_Coordinate = df_coords_trans@coords[ ,2]) -> df

ggmap(amap11)+
  geom_point(aes(X_Coordinate, Y_Coordinate), data = df %>%
             filter(Highest_Offense_Desc == "AUTO THEFT"))


ggmap(amap10)+
  geom_point(aes(X_Coordinate, Y_Coordinate, col = Highest_NIBRS_UCR_Offense_Description),
             data = df)


ggmap(amap11)+
  geom_point(aes(X_Coordinate, Y_Coordinate, col = Highest_NIBRS_UCR_Offense_Description),
             data = df)


ggmap(amap11)+
  geom_point(aes(X_Coordinate, Y_Coordinate, col = Highest_NIBRS_UCR_Offense_Description),
             data = df %>%
               filter(!Highest_NIBRS_UCR_Offense_Description %in% c("Theft", "Burglary")))



dat %>%
  rename(`Zip` = Zip_Code_Crime) %>%
  group_by(Zip) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = Zip, y = Count))+
    geom_bar(stat = 'identity')


dat %>%
  group_by(Zip_Code_Crime) %>%
  summarise(Count = n()) %>%
  ggplot(aes(x = reorder(Zip_Code_Crime, Count), y = Count))+
    geom_bar(stat = 'identity', width = 0.5)+
    labs(x = 'Zip')+
    coord_flip()+
    theme_calc()


ggplot(dat, aes(x = Highest_NIBRS_UCR_Offense_Description, fill = Zip_Code_Crime))+
  geom_bar()+
  labs(x = "Highest NIBS UCR", y = "Count", fill = "Zip")


dat %>%
  select(-Key, -Council_District, -Highest_Offense_Desc, 
         -Highest_NIBRS_UCR_Offense_Description, -Report_Date, -Location,
         -Clearance_Status, -Clearance_Date, -District, -Zip_Code_Crime,
         -X_Coordinate, -Y_Coordinate, -Zip_Code_Housing) -> dat_tmp

my_sub <- function(x){
 x <- gsub("%", "", x)
 x <- gsub("\\$" ,"", x)
 x <- gsub(",", "", x)
 x <- as.numeric(x)
}

dat_mat <- apply(dat_tmp, 2, my_sub)

cor_mat <- cor(dat_mat, use = "pairwise.complete")
cor_mat2 <- cor_mat

colnames(cor_mat2) <- NULL
rownames(cor_mat2) <- NULL

corrplot(cor_mat2)

cor_sorted <- function(x){
  x <- cor_mat[,x]
  sort(x)
}

#Example
cor_sorted("Averagemonthlytransportationcost")

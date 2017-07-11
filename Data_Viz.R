library(readr)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(ggmap)

hzip <- read_csv('cleaned_hzip.csv',
                 col_types = cols(`Zip Code` = col_character()))
dat <- read_csv('cleaned_data.csv',
                col_types = cols(Zip_Code_Crime = col_character(),
                                 Zip_Code_Housing = col_character,
                                 Clearance_Date = col_date(format = "%d-%b-%y")))

dat %>%
  filter(!(is.na(X_Coordinate) | is.na(Y_Coordinate))) -> df_no_missing


df_no_missing %>%
  ggplot(aes(X_Coordinate, Y_Coordinate))+
    geom_point(aes(X_Coordinate, Y_Coordinate), alpha = 0.2, shape = 1)+
    geom_density2d()+
    labs(x = "Longitude", y = "Lattitude", title = 'All Crime in Austin, TX')+
    theme_base()

amap10 <- get_map(location = 'Austin, Texas', zoom = 10)
amap11 <- get_map(location = 'Austin, Texas', zoom = 11)


ggmap(amap10)+
  geom_point(aes(X_Coordinate, Y_Coordinate), data = df_no_missing,
             alpha = 0.2, shape = 1)+
  geom_density2d(aes(X_Coordinate, Y_Coordinate), data = df_no_missing)+
  labs(x = "Longitude", y = "Lattitude", title = 'All Crime in Austin, TX')

#Zoom in a little bit, but loose some observations
ggmap(amap11)+
  geom_point(aes(X_Coordinate, Y_Coordinate), data = df_no_missing,
             alpha = 0.2, shape = 1)+
  geom_density2d(aes(X_Coordinate, Y_Coordinate), data = df_no_missing)+
  labs(x = "Longitude", y = "Lattitude", title = 'All Crime in Austin, TX')

#Let's differentiate by Highest_NIBRS_UCR_Offense_Description
df_no_missing %>%
  ggplot(aes(X_Coordinate, Y_Coordinate))+
    geom_point(aes(X_Coordinate, Y_Coordinate), alpha = 0.2, shape = 1)+
    geom_density2d()+
    labs(x = "Longitude", y = "Lattitude", title = 'Crime in Austin, TX')+
    facet_wrap(~Highest_NIBRS_UCR_Offense_Description)+
    theme_base()

#Overlay this with the city
ggmap(amap11)+
  geom_point(aes(X_Coordinate, Y_Coordinate), data = df_no_missing,
             alpha = 0.2, shape = 1)+
  geom_density2d(aes(X_Coordinate, Y_Coordinate), data = df_no_missing)+
  facet_wrap(~Highest_NIBRS_UCR_Offense_Description)+
  labs(x = "Longitude", y = "Lattitude", title = 'Crime in Austin, TX')

#Let's look at Crime over time
dat$Clearance_Date



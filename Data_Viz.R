library(readr)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(lubridate)
library(ggmap)

hzip <- read_csv('cleaned_hzip.csv',
                 col_types = cols(`Zip Code` = col_character()))
dat <- read_csv('scored_crime.csv',
                col_types = cols(Zip_Code_Crime = col_character(),
                                 Zip_Code_Housing = col_character(),
                                 Clearance_Date = col_date()))

dat %>%
  filter(!(is.na(X_Coordinate) | is.na(Y_Coordinate))) -> df_no_missing


df_no_missing %>%
  ggplot(aes(X_Coordinate, Y_Coordinate))+
    geom_point(aes(X_Coordinate, Y_Coordinate), alpha = 0.2, shape = 1)+
    geom_density2d()+
    labs(x = "Longitude", y = "Lattitude", title = 'All Crime in Austin, TX')+
    theme_grey()

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


#By Clearance status
df_no_missing %>%
  ggplot(aes(X_Coordinate, Y_Coordinate))+
    geom_point(aes(X_Coordinate, Y_Coordinate), alpha = 0.2, shape = 1)+
    geom_density2d()+
    labs(x = "Longitude", y = "Lattitude", title = 'Crime in Austin, TX by Clearance Status')+
    facet_wrap(~Clearance_Status)
    theme_base()

#Overlay this with the city
ggmap(amap11)+
    geom_point(aes(X_Coordinate, Y_Coordinate), data = df_no_missing,
              alpha = 0.2, shape = 1)+
    geom_density2d(aes(X_Coordinate, Y_Coordinate), data = df_no_missing)+
    facet_wrap(~Clearance_Status)+
    labs(x = "Longitude", y = "Lattitude", title = 'Crime in Austin, TX')
    

#Let's look at Crime over time
dat %>%
  select(Clearance_Date) %>%
  filter(complete.cases(.))%>%
  group_by(Clearance_Date) %>%
  summarise(Count = n()) %>%
  ggplot(aes(Clearance_Date, Count))+
    geom_line()
#There seems to be a large downward trend after Jan 2016
#Also, there seems be be very a cyclical pattern, perhaps this is due to the weekday?
dat %>%
  select(Clearance_Date) %>%
  filter(complete.cases(.))%>%
  mutate(Weekday = wday(Clearance_Date, label = TRUE)) %>%
  group_by(Weekday) %>%
  summarise(Count = n()) %>%
  ggplot(aes(Weekday, Count))+
    geom_bar(stat = 'identity', width = 0.5)

#There is considerably less crime on the weekend. 
#Perhaps this may be due to data collection or the weekends are just that much more violent
dat %>%
  select(Clearance_Date) %>%
  filter(complete.cases(.))%>%
  mutate(Month = month(Clearance_Date, label = TRUE)) %>%
  group_by(Month) %>%
  summarise(Count = n()) %>%
  ggplot(aes(Month, Count))+
    geom_bar(stat = 'identity', width = 0.5)
#Doesn't appear to be that much variation in month 

#Let's look at the variation in type of crime
dat %>%
  select(Clearance_Date, Highest_NIBRS_UCR_Offense_Description) %>%
  rename(`Offense` = Highest_NIBRS_UCR_Offense_Description) %>%
  filter(complete.cases(.)) %>%
  group_by(Clearance_Date, Offense) %>%
  summarise(Count = n()) %>%
  ggplot(aes(Clearance_Date, Count, col = Offense))+
    geom_line()

#Let's take out Theft
dat %>%
  select(Clearance_Date, Highest_NIBRS_UCR_Offense_Description) %>%
  rename(`Offense` = Highest_NIBRS_UCR_Offense_Description) %>%
  filter(complete.cases(.) & !(Offense %in% c("Theft", "Burglary", "Auto Theft"))) %>%
  group_by(Clearance_Date, Offense) %>%
  summarise(Count = n()) %>%
  ggplot(aes(Clearance_Date, Count, col = Offense))+
    geom_line()

#Let's eliminate Theft, Burglary and Auto Theft
dat %>%
  select(Clearance_Date, Highest_NIBRS_UCR_Offense_Description) %>%
  rename(`Offense` = Highest_NIBRS_UCR_Offense_Description) %>%
  filter(complete.cases(.) & !(Offense %in% c("Theft", "Burglary", "Auto Theft"))) %>%
  group_by(Clearance_Date, Offense) %>%
  summarise(Count = n()) %>%
  ggplot(aes(Clearance_Date, Count, col = Offense))+
    geom_line()

#Let's visualize our index over time
dat %>%
  select(Clearance_Date, Crime_Score1) %>%
  group_by(Clearance_Date) %>%
  summarise(`Crime Index` = sum(Crime_Score1)) %>%
  filter(!is.na(Clearance_Date)) %>%
  ggplot(aes(Clearance_Date, `Crime Index`))+
    geom_line()+
    labs(x = 'Date', title = "Crime Index over time")

#Saturday and Sundays have much lower crime
dat %>%
  select(Clearance_Date, Crime_Score1) %>%
  group_by(Clearance_Date) %>%
  summarise(`Crime Index` = sum(Crime_Score1)) %>%
  filter(!is.na(Clearance_Date)) %>%
  ungroup()%>%
  mutate(weekday = wday(Clearance_Date, label = TRUE)) %>%
  group_by(weekday) %>%
  summarise(Mean_Index = mean(`Crime Index`))


library(readr)

hzip15 <- read_csv('./dash-austin-crime-report-2015/2014_Housing_Market_Analysis_Data_by_Zip_Code.csv')
dat <- read_csv('cleaned_data.csv',
                col_types = cols(Zip_Code_Crime = col_character(),
                                 Zip_Code_Housing = col_character()))


dat %>%
  group_by(Zip_Code_Housing) %>%
  summarise(Num_Crimes = n()) -> dat_zip

#Create housing zip code crime score
ucr <- unique(dat$Highest_NIBRS_UCR_Offense_Description)
desired <- c(4, 3, 2, 5, 1, 6, 7)
names(desired) <- ucr

desired[match(dat$Highest_NIBRS_UCR_Offense_Description, ucr)]


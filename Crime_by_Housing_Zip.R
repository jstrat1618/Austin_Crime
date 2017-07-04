library(readr)

hzip <- read_csv('cleaned_hzip.csv',
                 col_types = cols(`Zip Code` = col_character()))
dat <- read_csv('cleaned_data.csv',
                col_types = cols(Zip_Code_Crime = col_character(),
                                 Zip_Code_Housing = col_character()))

#Make sure Zip_Code_Crime and Zip_Code_Housing are always the same
all(dat$Zip_Code_Crime == dat$Zip_Code_Housing, na.rm = TRUE)

#Aside from NA's looks like they are
one <- is.na(dat$Zip_Code_Crime) & !is.na(dat$Zip_Code_Housing)
two <- is.na(dat$Zip_Code_Housing) &! is.na(dat$Zip_Code_Crime)
any(one)
any(two)

#Sometime Zip_Code_Crime is not missing while Zip_Code_Housing is 
dat %>%
  select(Zip_Code_Housing, Zip_Code_Crime, Highest_NIBRS_UCR_Offense_Description) %>%
  filter(two)
  

#
dat %>%
  rename(`Zip` = Zip_Code_Crime) %>%
  group_by(Zip) %>%
  summarise(Num_Crimes = n()) -> dat_zip

#Not all the Zips match?
all(hzip$`Zip Code` %in% dat_zip$Zip) #All match here but
all(dat_zip$Zip %in% hzip$`Zip Code`) #but not here
dat_zip$Zip[!dat_zip$Zip %in% hzip$`Zip Code`]#These zips aren't found


hzip %>%
  rename(`Zip` = `Zip Code`) %>%
  full_join(dat_zip, by = "Zip") -> hzip_joined

#Create housing zip code crime score
ucr <- unique(dat$Highest_NIBRS_UCR_Offense_Description)
desired <- c(4, 3, 2, 5, 1, 6, 7)
names(desired) <- ucr

desired[match(dat$Highest_NIBRS_UCR_Offense_Description, ucr)]


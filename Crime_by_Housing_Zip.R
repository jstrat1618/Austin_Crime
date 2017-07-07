library(dplyr)
library(readr)
library(corrplot)
library(ggplot2)

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


not_fnd <- unique(dat$Zip_Code_Crime)[!unique(dat$Zip_Code_Crime) %in% unique(hzip$`Zip Code`)]
cat('Need to find out why', not_fnd, 'is in dat but not hzip')


#Moving on for now, let's throw out those data that are in not_fnd
dat %>%
  rename(`Zip Code` = Zip_Code_Crime) %>%
  filter(`Zip Code` %in% hzip$`Zip Code`) %>%
  group_by(`Zip Code`) %>%
  summarise(Count = n()) %>%
  full_join(hzip %>%
            mutate(Pop = `Hispanic or Latino, of any race` + `Non-White, Non-Hispanic or Latino`),
            by = "Zip Code") %>%
  mutate(log_Crime_Pop = log(Count / Pop)) %>%
  select(-Count)-> hzip_joined

hzip_joined %>%
  select(-`Zip Code`) %>%
  as.matrix() -> hmat

hmat_cor <- cor(hmat, use = 'pairwise.complete')
sort(hmat_cor[,'log_Crime_Pop'])

#Corplot
rownames(hmat_cor) <- NULL
colnames(hmat_cor) <- NULL
corrplot(hmat_cor)


#Create housing zip code crime score
#Not sure if there is a "dplyr" way to do this?
ucr <- unique(dat$Highest_NIBRS_UCR_Offense_Description)
desired <- c(4, 3, 2, 5, 1, 6, 7)
names(desired) <- ucr

dat[,'Crime_Y1'] <- desired[match(dat$Highest_NIBRS_UCR_Offense_Description, ucr)]

dat %>%
  rename(`Zip Code` = Zip_Code_Crime) %>%
  filter(`Zip Code` %in% hzip$`Zip Code`) %>%
  group_by(`Zip Code`) %>%
  summarise(Crime_Score1 = sum(Crime_Y1)) %>%
  full_join(hzip %>%
              mutate(Pop = `Hispanic or Latino, of any race` + `Non-White, Non-Hispanic or Latino`),
            by = "Zip Code") %>%
  mutate(log_Crime_Score1_Pop = log(Crime_Score1 / Pop)) %>%
  select(-Crime_Score1)-> hzip_joined

hzip_joined %>%
  select(-`Zip Code`) %>%
  as.matrix() -> hmat

hmat_cor <- cor(hmat, use = 'pairwise.complete')
sort(hmat_cor[,'log_Crime_Score1_Pop'])

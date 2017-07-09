library(dplyr)
library(readr)
library(corrplot)
library(ggplot2)

hzip <- read_csv('cleaned_hzip.csv',
                 col_types = cols(`Zip Code` = col_character()))
dat <- read_csv('cleaned_data.csv',
                col_types = cols(Zip_Code_Crime = col_character(),
                                 Zip_Code_Housing = col_character()))


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
sort(hmat_cor[,'log_Crime_Pop'], decreasing = TRUE)

#Corplot
rownames(hmat_cor) <- NULL
colnames(hmat_cor) <- NULL
corrplot(hmat_cor)

#The problem with the above is it is simply the number of crimes that appear and 
#and doesn't distinguish between more violent crimes. Below, I will try to create a
#function that computes a weighted measure of crime a vector of weighted cromes

#Create housing zip code crime score
#Not sure if there is a "dplyr" way to do this?
weighted_crime <- function(w_rob = 4, w_burg = 3, w_auto =2, w_aglt = 5, w_theft = 1, 
                           w_rape = 6, w_murd = 7){
  
  ucr <- unique(dat$Highest_NIBRS_UCR_Offense_Description)
  desired <- c(4, 3, 2, 5, 1, 6, 7)
  names(desired) <- ucr
  out <- desired[match(dat$Highest_NIBRS_UCR_Offense_Description, ucr)]
  return(out)
}

dat[,'Crime_Y1'] <- weighted_crime()

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
sort(hmat_cor[,'log_Crime_Score1_Pop'], decreasing = TRUE)

#It would be interesting to use less "subjective weights like minimum sentences
library(readr)

dat <- read_csv('cleaned_data.csv',
                col_types = cols(Zip_Code_Crime = col_character(),
                                 Zip_Code_Housing = col_character()))


dat %>%
  group_by(Zip_Code_Housing) %>%
  summarise(Num_Crimes = n(),
            Populationbelowpovertylevel = mean(Populationbelowpovertylevel),
            Medianhouseholdincome = mean(Medianhouseholdincome),
            `Non-WhiteNon-HispanicorLatino` = mean(`Non-WhiteNon-HispanicorLatino`),
            HispanicorLatinoofanyrace = mean(HispanicorLatinoofanyrace),
            Populationbelowpovertylevel = mean(Populationbelowpovertylevel),
            Populationwithdisability = mean(Populationwithdisability),
            `Largehouseholds(5+members)` = mean(`Largehouseholds(5+members)`),
            HousingChoiceVoucherholders = mean(HousingChoiceVoucherholders),
            Unemployment = mean(Unemployment),
            `Changeinpercentageofpopulationbelowpoverty2000-2012` = mean(`Changeinpercentageofpopulationbelowpoverty2000-2012`),
            `Changeinmedianrent2000-2012` = mean(`Changeinmedianrent2000-2012`),
            `Changeinmedianhomevalue2000-2012` = mean(`Changeinmedianhomevalue2000-2012`),
            `Ownerunitsaffordabletoaverageretail/serviceworker` = mean(`Ownerunitsaffordabletoaverageretail/serviceworker`),
            Ownerunitsaffordabletoaverageartist = mean(Ownerunitsaffordabletoaverageartist),
            Ownerunitsaffordabletoaverageteacher = mean(Ownerunitsaffordabletoaverageteacher),
            Ownerunitsaffordabletoaveragetechworker = mean(Ownerunitsaffordabletoaveragetechworker)) -> dat_zip

#Create housing zip code crime score
ucr <- unique(dat$Highest_NIBRS_UCR_Offense_Description)
desired <- c(4, 3, 2, 5, 1, 6, 7)
names(desired) <- ucr

desired[match(dat$Highest_NIBRS_UCR_Offense_Description, ucr)]


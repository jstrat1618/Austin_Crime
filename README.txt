Got this data from https://data.world/dash/austin-crime-report-2015

Last left off:
Wanting to visualize the different weighted crime measures

EDA.R- This is more like my scratch sheet, doesn't really have a logical flow to it.

data_cleaning.R- the script I used to clean ./dash-austin-crime-report-2015/2014_Housing_Market_Analysis_Data_by_Zip_Code.csv and ./dash-austin-crime-report-2015/Crime_Housing_Joined.csv

Crime_by_Housing_Zip.R- an analysis of crime by zip code

Weight_Crime.R- the script I used to weight crime and come up with a crime score.

Scored_relative_to_proportion.csv- contains the cleaned data created by data_cleaning.R along with the column Crime_Score1- which is crime scored by scoring Crime by frequency. The most frequently commited crimes by (Highest_NIBRS_UCR_Offense_Description) get the least weight- in this case theft, whereas the least often commit gets the most weight- in this case murder.

scored_crime.csv- score_dat_crime-contains the cleaned data created by data_cleaning.R along with the column Crime_Score1- which is crime scored by scoring Crime according to the function scored_dat_crime in Weight_Crime.R, which scores crime acording to the 7 levels in Highest_NIBRS_UCR_Offense_Description
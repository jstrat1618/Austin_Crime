library(readr)
score_dat_crime <- function(input_file = 'cleaned_data.csv', out_file = 'scored_crime.csv',
                            w_rob = 4, w_burg = 3, w_auto =2, w_aglt = 5, w_theft = 1, 
                            w_rape = 6, w_murd = 7){
  

  dat <- read_csv('cleaned_data.csv',
                 col_types = cols(Zip_Code_Crime = col_character(),
                                  Zip_Code_Housing = col_character(),
                                  Clearance_Date = col_date(format = "%d-%b-%y")))

  #Create a function that weights crime
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
  
  
  
    
  dat[,'Crime_Score1'] <- weighted_crime(w_rob, w_burg, w_auto, w_aglt, w_theft, 
                                         w_rape, w_murd)
  
  write_csv(dat, path = out_file)
  
}

#score_dat_crime()

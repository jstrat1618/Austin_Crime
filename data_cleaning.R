library(readr)
library(ggplot2)
library(ggthemes)
library(dplyr)
library(sp)
library(ggmap)
library(corrplot)

hzip15 <- read_csv('./dash-austin-crime-report-2015/2014_Housing_Market_Analysis_Data_by_Zip_Code.csv')

dat <- read_csv('./dash-austin-crime-report-2015/Crime_Housing_Joined.csv',
                col_types = cols(Zip_Code_Crime = col_character(),
                                 Zip_Code_Housing = col_character()))

#Transform Coordinates
data.frame(lon = dat$X_Coordinate, lat = dat$Y_Coordinate) %>%
  filter(complete.cases(.)) -> df_coords

coordinates(df_coords) <- c("lon", "lat")
proj4string(df_coords) <- CRS("+init=esri:102739")
CRS.new <- CRS("+init=epsg:4326")

df_coords_trans <- spTransform(df_coords, CRS.new)

dat %>%
  filter(!(is.na(X_Coordinate) | is.na(Y_Coordinate))) %>%
  mutate(X_Coordinate = df_coords_trans@coords[ ,1],
         Y_Coordinate = df_coords_trans@coords[ ,2]) %>%
  select(Key, X_Coordinate, Y_Coordinate) -> dat_coords

dat %>% 
  select(-X_Coordinate, -Y_Coordinate) %>%
  full_join(dat_coords, by = "Key") -> dat

#Convert certain data that is supposed to be numeric
str(dat)

dat %>%
  select(-Key, -Council_District, -Highest_Offense_Desc, 
         -Highest_NIBRS_UCR_Offense_Description, -Report_Date, -Location,
         -Clearance_Status, -Clearance_Date, -District, -Zip_Code_Crime,
         -X_Coordinate, -Y_Coordinate, -Zip_Code_Housing) -> dat_num

my_sub <- function(x){
  x <- gsub("%", "", x)
  x <- gsub("\\$" ,"", x)
  x <- gsub(",", "", x)
  x <- as.numeric(x)
}

dat_mat <- apply(dat_num, 2, my_sub)
df_num <- tbl_df(dat_mat)

var_ix_2drop <- which(names(dat) %in% colnames(dat_mat))

dat %>%
  select(-var_ix_2drop) %>%
  bind_cols(df_num) -> dat


#write_csv(dat, 'cleaned_data.csv')

#Clean 2015/2014_Housing_Market_Analysis_Data_by_Zip_Code.csv
hzip_mat <- apply(hzip15, 2, my_sub) #I'll let alone the fact that Zip_Code is being treated as numeric

hzip_df <- tbl_df(hzip_mat)

#write_csv(hzip_df, 'cleaned_hzip.csv')


library(readr)
library(dplyr)
library(ggplot2)

dat <- read_csv('cleaned_data.csv',
                col_types = cols(Zip_Code_Crime = col_character(),
                                 Zip_Code_Housing = col_character()))

dat %>%
  select(X_Coordinate, Y_Coordinate) %>%
  filter(complete.cases(.)) -> coord_df

coord_df %>%
  ggplot(aes(X_Coordinate, Y_Coordinate))+
  geom_point() -> plt1


Nk <- 10
w <- numeric(Nk)
b <- numeric(Nk)
for (num_cent in 2:Nk){
  fit <- kmeans(coord_df, num_cent)
  w[num_cent] <- fit$tot.withinss
}

w <- w[-1]
plot(2:Nk,w, xlab = 'Number of Clusters', ylab = "Within SS")


kmeans_fit <- kmeans(coord_df, centers = 4)
cntrs <- kmeans_fit$centers

plt1 +
  geom_point(aes(cntrs[1,1], cntrs[1,2], col = 'red'))+
  geom_point(aes(cntrs[2,1], cntrs[2,2], col = 'red'))+
  geom_point(aes(cntrs[3,1], cntrs[3,2], col = 'red'))+
  geom_point(aes(cntrs[4,1], cntrs[4,2], col = 'red'))
  

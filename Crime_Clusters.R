library(readr)
library(dplyr)
library(ggplot2)
library(geosphere)

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
  
clust1 <- c(cntrs[1,1], cntrs[1,2])
clust2 <- c(cntrs[2,1], cntrs[2,2])
clust3 <- c(cntrs[3,1], cntrs[3,2])
clust4 <- c(cntrs[4,1], cntrs[4,2])

dist_clust1_miles <- numeric(nrow(coord_df))
dist_clust2_miles <- numeric(nrow(coord_df))
dist_clust3_miles <- numeric(nrow(coord_df))
dist_clust4_miles <- numeric(nrow(coord_df))

meters2miles <- 1/(1000*1.60934)

for(i in 1:nrow(coord_df)){
  x <- coord_df$X_Coordinate[i]
  y <- coord_df$Y_Coordinate[i]
  
  dist_clust1_miles[i] <- distHaversine(c(x,y), clust1) * meters2miles
  dist_clust2_miles[i] <- distHaversine(c(x,y), clust2) * meters2miles
  dist_clust3_miles[i] <- distHaversine(c(x,y), clust3) * meters2miles
  dist_clust4_miles[i] <- distHaversine(c(x,y), clust4) * meters2miles
  
}


dist_df <- tbl_df(data.frame(dist_clust1_miles, dist_clust2_miles,
                      dist_clust3_miles, dist_clust4_miles))


num_miles <- 3.5

dist_df %>%
  filter(dist_clust1_miles < num_miles 
         | dist_clust2_miles < num_miles
         | dist_clust3_miles < num_miles
         | dist_clust4_miles < num_miles) %>%
  nrow()/nrow(dist_df)

  


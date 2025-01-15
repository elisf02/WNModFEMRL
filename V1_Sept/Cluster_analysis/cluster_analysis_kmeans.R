####https://uc-r.github.io/kmeans_clustering

#packagese required
library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering algorithms & visualization
library(readxl) #to import dataset from excel

#import the dataset containing birds' demographic parameters
uccelli <- read_excel("/home/nicolaferrari/Scrivania/Progetto_WN_Uccelli/Birds/birds_demographic_dataset.xlsx", 
                     sheet = "Demo")

#eliminate the first column of the dataset containing bird species names 
mydata<-uccelli[,c(-1)]

#scale the data
df <- scale(mydata)
head(df)

#create a distance matrix: represent the dissimilarity between each couple of observations
#####euclidean method
dist.euc <- get_dist(df, method = "euclidean") #to use other method change "euclidean" with the name method you want to use (e.g. manhattan)
fviz_dist(dist.euc, gradient = list(low='#00AFBB', mid='white', high='#FC4E07'))

 #####pearson correlation matrix
data.cor <- get_dist(x = df, 
                     method = "kendall")
fviz_dist(data.cor, gradient = list(low='#00AFBB', mid='white', high='#FC4E07'))



#cluster analysis with 3 clusters 
k2 <- kmeans(df, centers = 3, nstart = 25)
str(k2)
k2

fviz_cluster(k2, data = df, main ="DEMOdataset", subtitle = "3 clusters")


#standard pairwise scatter plots to illustrate clusters with respect to the original variables
#
 df %>%
  as_tibble() %>%
  mutate(cluster = k2$cluster,
         species = row.names(uccelli)) %>%
  ggplot(aes(pop, mean_life, color = factor(cluster), label = species)) +
  geom_text()




k3 <- kmeans(df, centers = 3, nstart = 25)
k4 <- kmeans(df, centers = 4, nstart = 25)
k5 <- kmeans(df, centers = 5, nstart = 25)

# plots to compare
p1 <- fviz_cluster(k2, geom = "point", data = df) + ggtitle("k = 2")
p2 <- fviz_cluster(k3, geom = "point",  data = df) + ggtitle("k = 3")
p3 <- fviz_cluster(k4, geom = "point",  data = df) + ggtitle("k = 4")
p4 <- fviz_cluster(k5, geom = "point",  data = df) + ggtitle("k = 5")

library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)


set.seed(123)

# function to compute total within-cluster sum of square 
wss <- function(k) {
  kmeans(df, k, nstart = 10 )$tot.withinss
}

# Compute and plot wss for k = 1 to k = 13
k.values <- 1:12

# extract wss for 2-13 clusters
wss_values <- map_dbl(k.values, wss)

plot(k.values, wss_values,
     type="b", pch = 19, frame = FALSE, 
     xlab="Number of clusters K",
     ylab="Total within-clusters sum of squares")


set.seed(123)

fviz_nbclust(df, kmeans, method = "wss")

# function to compute average silhouette for k clusters
avg_sil <- function(k) {
  km.res <- kmeans(df, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(df))
  mean(ss[, 3])
}

# Compute and plot wss for k = 2 to k = 13
k.values <- 2:12

# extract avg silhouette for 2-15 clusters
avg_sil_values <- map_dbl(k.values, avg_sil)

plot(k.values, avg_sil_values,
     type = "b", pch = 19, frame = FALSE, 
     xlab = "Number of clusters K",
     ylab = "Average Silhouettes")


fviz_nbclust(df, kmeans, method = "silhouette")


# compute gap statistic
set.seed(123)
gap_stat <- clusGap(df, FUN = kmeans, nstart = 25,
                    K.max = 10, B = 50)
# Print the result
print(gap_stat, method = "firstmax")

fviz_gap_stat(gap_stat)


# Compute k-means clustering with k = 3
set.seed(123)
final <- kmeans(df, 3, nstart = 25)
print(final)
fviz_cluster(final, data = df)


mean_data<- mydata %>%
  mutate(Cluster = final$cluster) %>%
  group_by(Cluster) %>%
  summarise_all("mean")

view (mean_data)

write.table(mean_data,file="C:/Users/Utente/Desktop/PCA/medie_prova_nuovo+epi3.csv",sep=";", dec=",", quote=FALSE, row.names=FALSE)
 

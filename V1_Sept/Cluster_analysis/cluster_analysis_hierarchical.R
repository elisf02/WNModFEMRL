###https://uc-r.github.io/hc_clustering 


library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms
library(readxl)     #to import dataset from excel

uccelli <- read_excel("/home/nicolaferrari/Scrivania/Progetto_WN_Uccelli/Birds/birds_demographic_dataset.xlsx", 
                      sheet = "Demo")

mydata<-uccelli[,c(-1)]

df <- scale(mydata)
head(df)


# Dissimilarity matrix
d <- dist(df, method = "euclidean")

# Hierarchical clustering using Complete Linkage
hc1 <- hclust(d, method = "complete" )

# Plot the obtained dendrogram
plot(hc1, cex = 0.6, hang = -1)


# Compute with agnes
hc2 <- agnes(df, method = "complete")

# Agglomerative coefficient
hc2$ac



# methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(df, method = x)$ac
}

map_dbl(m, ac)



hc3 <- agnes(df, method = "ward")
pltree(hc3, cex = 0.6, hang = -1, main = "Dendrogram of agnes") 



# compute divisive hierarchical clustering
hc4 <- diana(df)

# Divise coefficient; amount of clustering structure found
hc4$dc

# plot dendrogram
pltree(hc4, cex = 0.6, hang = -1, main = "Dendrogram of diana")



# Ward's method
hc5 <- hclust(d, method = "ward.D2" )

# Cut tree into 2 groups
sub_grp <- cutree(hc5, k = 5)

# Number of members in each cluster
table(sub_grp)


mydata %>%
  mutate(cluster = sub_grp) %>%
  head


plot(hc5, cex = 0.6)
rect.hclust(hc5, k = 5, border = 2:6)


fviz_cluster(list(data = df, cluster = sub_grp))


# Cut agnes() tree into 2 groups
hc_a <- agnes(df, method = "ward")
cutree(as.hclust(hc_a), k = 2)

# Cut diana() tree into 2 groups
hc_d <- diana(df)
cutree(as.hclust(hc_d), k = 2)


# Compute distance matrix
res.dist <- dist(df, method = "euclidean")

# Compute 2 hierarchical clusterings
hc1 <- hclust(res.dist, method = "complete")
hc2 <- hclust(res.dist, method = "ward.D2")

# Create two dendrograms
dend1 <- as.dendrogram (hc1)
dend2 <- as.dendrogram (hc2)

tanglegram(dend1, dend2)


dend_list <- dendlist(dend1, dend2)

tanglegram(dend1, dend2,
           highlight_distinct_edges = FALSE, # Turn-off dashed lines
           common_subtrees_color_lines = FALSE, # Turn-off line colors
           common_subtrees_color_branches = TRUE, # Color common branches 
           main = paste("entanglement =", round(entanglement(dend_list), 2))
)


fviz_nbclust(df, FUN = hcut, method = "wss")

fviz_nbclust(df, FUN = hcut, method = "silhouette")

gap_stat <- clusGap(df, FUN = hcut, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)

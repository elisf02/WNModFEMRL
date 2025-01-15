###PCA http://www.sthda.com/english/articles/31-principal-component-methods-in-r-practical-guide/112-pca-principal-component-analysis-essentials/

library("FactoMineR")
library("factoextra")
library("corrplot")
library("ggforce")

#importazione dei dati
library(readxl)

uccelli <- read_excel("/home/nicolaferrari/Scrivania/Progetto_WN_Uccelli/Birds/birds_demographic_dataset.xlsx", 
                      sheet = "demo")
View(uccelli)

#modifica dataset
is.na(uccelli)


uccelli_dataset<-uccelli[,c(-1)] #elimino prima colonna (character)
View(uccelli_dataset)


options(ggrepel.max.overlaps = Inf)


#standardization
PCA(uccelli_dataset, scale.unit = TRUE, ncp = 12, graph = FALSE)


#computation
res.pca <- PCA(uccelli_dataset, scale.unit = TRUE, ncp = 12, graph = FALSE)
print(res.pca)

##### GRAPHS OF VARIABLES #####

#Eigenvalues / Variances
eig.val <- get_eigenvalue(res.pca) #Extract the eigenvalues/variances of principal components
eig.val


fviz_eig(res.pca, addlabels = TRUE, ylim = c(0, 50)) #Visualize the eigenvalues

var <- get_pca_var(res.pca) #Extract the results for variables.
var

#This function provides a list of matrices containing all the results for
#the active variables (coordinates, correlation between variables and axes, 
#squared cosine and contributions)

#access to the different var components:
# Coordinates
head(var$coord)
var$coord
# Correlation between variables and dimensions
head(var$cor)
var$cor
# Cos2: quality on the factore map
head(var$cos2)
var$cos2
# Contributions to the principal components
head(var$contrib)
var$contrib

#Correlation circle
#to plot variables
fviz_pca_var(res.pca, col.var = "black") #Visualize the results variables.

#Quality of representation
head(var$cos2, 4)

corrplot(var$cos2, is.corr=FALSE)

# Total cos2 of variables on Dim.1 and Dim.2
fviz_cos2(res.pca, choice = "var", axes = 1:2) #create a bar plot of variables cos2

# Color by cos2 values: quality on the factor map
fviz_pca_var(res.pca, col.var = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), 
             repel = TRUE # Avoid text overlapping
)


# Change the transparency by cos2 values
fviz_pca_var(res.pca, alpha.var = "cos2")

#Contributions of variables to PCs
head(var$contrib, 4)

corrplot(var$contrib, is.corr=FALSE) #highlight the most contributing variables for each dimension

#bar plot of variable contributions
# Contributions of variables to PC1
fviz_contrib(res.pca, choice = "var", axes = 1, top = 10)
# Contributions of variables to PC2
fviz_contrib(res.pca, choice = "var", axes = 2, top = 10)
# total contribution to PC1 and PC2
fviz_contrib(res.pca, choice = "var", axes = 1:2, top = 10)

fviz_contrib(res.pca, choice = "var", axes = 12)

#correlation plot
fviz_pca_var(res.pca, col.var = "contrib",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07")
)


# Change the transparency by contrib values
fviz_pca_var(res.pca, alpha.var = "contrib")

#Color by a custom continuous variable.
#The coloring variable should have the same length as the number of
#activevariables in the PCA (here n = 23)
# Create a random continuous variable of length 23
set.seed(123)
my.cont.var <- rnorm(28)
# Color variables by the continuous variable
fviz_pca_var(res.pca, col.var = my.cont.var,
             gradient.cols = c("blue", "yellow", "red"),
             legend.title = "Cont.Var")


#Color by groups
# Create a grouping variable using kmeans
# Create 3 groups of variables (centers = 3)
set.seed(123)
res.km <- kmeans(var$coord, centers = 3, nstart = 25)
grp <- as.factor(res.km$cluster)
# Color variables by groups
fviz_pca_var(res.pca, col.var = grp, 
             palette = c("#0073C2FF", "#EFC000FF", "#868686FF"),
             legend.title = "Cluster")

#Dimension description
res.desc <- dimdesc(res.pca, axes = c(1,2), proba = 0.05)
# Description of dimension 1
res.desc$Dim.1
# Description of dimension 2
res.desc$Dim.2

##### GRAPSH OF INDIVIDUALS #####
ind <- get_pca_ind(res.pca)
ind

#This function provides a list of matrices containing all the results for
#the active individuals (coordinates, correlation between variables and axes, 
#squared cosine and contributions)

#access to the different var components:
# Coordinates of individuals
head(ind$coord)
# Quality of individuals
head(ind$cos2)
# Contributions of individuals
head(ind$contrib)

#Plots: quality and contribution
fviz_pca_ind(res.pca) #produce the graph of individuals

#color individuals by their cos2 values
fviz_pca_ind(res.pca, col.ind = "cos2", 
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)


#change the point size according the cos2 of the corresponding individuals:
fviz_pca_ind(res.pca, pointsize = "cos2", 
             pointshape = 21, fill = "#E7B800",
             repel = TRUE # Avoid text overlapping (slow if many points)
)


#change both point size and color by cos2
fviz_pca_ind(res.pca, col.ind = "cos2", pointsize = "cos2",
             gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"),
             repel = TRUE # Avoid text overlapping (slow if many points)
)


#create a bar plot of the quality of representation (cos2) of individuals on the factor map
fviz_cos2(res.pca, choice = "ind")

#To visualize the contribution of individuals to the first two principal components
# Total contribution on PC1 and PC2
fviz_contrib(res.pca, choice = "ind", axes = 1:2)

#Color by a custom continuous variable
# Create a random continuous variable of length 7,
# Same length as the number of active individuals in the PCA
set.seed(123)
my.cont.var <- rnorm(13)
# Color individuals by the continuous variable
fviz_pca_ind(res.pca, col.ind = my.cont.var,
             gradient.cols = c("blue", "yellow", "red"),
             legend.title = "Cont.Var")


##### prova per ellissi ####

uccelli.pca <- prcomp(uccelli[, -1],  scale = TRUE)

fviz_pca_ind(uccelli.pca, label='none',alpha.ind = 1,
             habillage=uccelli$species,
             repel = TRUE, 
             # Don't use default Ellipses!!!!
             # addEllipses = TRUE,
             invisible='quali') +
  # ADD ggforce's ellipses
  ggforce::geom_mark_ellipse(aes(fill = Groups,
                                 color = Groups)) +
  theme(legend.position = 'bottom') +
  coord_equal()



uccelli.pca <- prcomp(uccelli_dataset,  scale = TRUE)

fviz_pca_ind(uccelli.pca, label='none',alpha.ind = 1,
             habillage=uccelli,
             repel = TRUE, 
             # Don't use default Ellipses!!!!
             #addEllipses = TRUE,
             invisible='quali') +
  # ADD ggforce's ellipses
  ggforce::geom_mark_ellipse(aes(fill = Groups,
                                 color = Groups)) +
  theme(legend.position = 'left') +
  coord_equal()
##################################################################################################################
trasposta_uccelli <- t(uccelli)
View(trasposta_uccelli)

t_uccelli <- trasposta_uccelli[c(-1),] 
View(t_uccelli)

t_uccelli.pca <- prcomp(t_uccelli,  scale = TRUE)


fviz_pca_ind(t_uccelli,
             geom.ind = "point", # show points only (nbut not "text")
             col.ind = t_uccelli, # color by groups
             palette = c("#00AFBB", "#E7B800", "#FC4E07"),
             addEllipses = TRUE, # Concentration ellipses
             legend.title = "Groups"
)

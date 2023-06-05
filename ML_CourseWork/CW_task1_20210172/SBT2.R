install.packages("FactoMineR")
install.packages("factoextra")
library(factoextra)
library(FactoMineR)

#apply PCA 
pca_data <- PCA(vehicles_cleaned, graph = FALSE)

# Show the eigenvalues and eigenvectors
summary(pca_data)

# Show the scree plot
fviz_eig(pca_data, addlabels = TRUE)

# Show the cumulative percentage of variance explained
eig_val <- get_eigenvalue(pca_data)
eig_val

cumulative_variances <- cumsum(eig_val/sum(eig_val)*100)
cumulative_variances

barplot(cumulative_variances, main = "Cumulative Percentage of Variance Explained", xlab = "Number of Components", ylab = "Cumulative %")

# Choose the PCs that provide at least cumulative score > 92%
num_components <- length(cumulative_variances[cumulative_variances > 92])
print(paste("Number of components needed to explain at least 92% of the variance:", num_components))

# Create a transformed data set 
pca_result <- PCA(vehicles_cleaned, ncp = num_components, graph = FALSE)$ind$coord


#determine the number of cluster centers

#NbClust method
library(NbClust)
nb <- NbClust(pca_result, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")
print(nb)

#Elbow method
x11() # creates an X11 graphics device
graphics.off() # reset the graphics device
#plot(x, y) # try plotting again
fviz_nbclust(pca_result, kmeans, method = "wss") + geom_vline(xintercept = 4, linetype = 2) + labs(subtitle = "Elbow method")

# Gap statistic method
set.seed(123)
fviz_nbclust(pca_result, kmeans, nstart = 25,  method = "gap_stat", nboot = 50) + labs(subtitle = "Gap statistic method")


# Silhouette method
library(cluster)
library(factoextra)
fviz_nbclust(pca_result, kmeans, method = "silhouette") + labs(subtitle = "Silhouette method")

#compute k-means clustering with k=3
set.seed(123)
final_stat_pca <- kmeans(pca_result,3,nstart = 25)
print(final_stat_pca)

#BSS
BSS_pca <- sum(final_stat_pca$size-(colMeans(pca_result)-final_stat_pca$centers)^2)
cat("BSS:",BSS_pca,"\n")

#TSS
TSS_pca <- sum((pca_result-colMeans(pca_result))^2)
cat("TSS:",TSS_pca,"\n")

#WSS
WSS_pca <- sum(final_stat_pca$withinss)
cat("WSS:",WSS_pca,"\n")

#BSS to TSS
ratio_BSS_to_TSS_pca <- BSS_pca/TSS_pca
cat("ratio_BSS_to_TSS:",ratio_BSS_to_TSS_pca,"\n")

#Silhouette plot
pam.res2 <- pam(pca_result,3,metric="euclidean",stand = FALSE)
fviz_silhouette(pam.res2,palette="jco",ggtheme=theme_classic())

#average Silhouette width score
sil <- silhouette(final_stat_pca$cluster, dist(pca_result))
avg_Sil_width_pca <-mean(sil[,3])
cat("average Silhouette width score:",avg_Sil_width_pca,"\n")

#Calinski-Harabasz Index
install.packages("fpc")
library(fpc)
ch_index <- calinhara(pca_result, final_stat_pca$cluster)
print(ch_index)
barplot(ch_index, main="Calinski-Harabasz Index for K-Means Clustering", xlab="Number of Clusters", ylab="Calinski-Harabasz Index")
plot(ch_index, type="b", xlab="Number of Clusters", ylab="Calinski-Harabasz Index")


#Silhouette plot
library(cluster)
pam.res2 <- pam(pca_result,3,metric="euclidean",stand = FALSE)
fviz_silhouette(pam.res2,palette="jco",ggtheme=theme_classic())

#average Silhouette width score
sil <- silhouette(final_stat_pca$cluster, dist(pca_result))
avg_Sil_width <-mean(sil[,3])
cat("average Silhouette width score:",avg_Sil_width,"\n")
























#lord the data set 
install.packages("readxl")
library(readxl)
setwd("D:/2nd Year/ML_CourseWork")
vehicles <- read_excel("vehicles.xlsx")

#define 18 attributes
install.packages("dplyr")
library(dplyr)
vehicles_subset <- select(vehicles,Comp:Holl.Ra)
     
#Scaling.
vehicles_scaled<-scale(vehicles_subset)

#set outlines using IQR method.
out1<- apply(vehicles_scaled,2,quantile,probs=0.25,na.rm = TRUE)
out3<- apply(vehicles_scaled,2,quantile,probs=0.75,na.rm =TRUE)
IQR<- out3 - out1

#identifies outliers.
outliers<-apply(vehicles_scaled,2,function(x){
  lower <- out1-1.5*IQR
  upper <- out3+1.5*IQR
  x < lower | x > upper
})

#remove outliers.
vehicles_cleaned <- vehicles_scaled
vehicles_cleaned[outliers] <- NA
vehicles_cleaned <- na.omit(vehicles_cleaned)

#determine the number of cluster centers
#NbClust method
install.packages("NbClust")
library(NbClust)
nb <- NbClust(vehicles_cleaned, distance = "euclidean", min.nc = 2, max.nc = 10, method = "kmeans")
print(nb)

#Elbow method
install.packages("factoextra")
library(factoextra)
x11() # creates an X11 graphics device
graphics.off() # reset the graphics device
#plot(x, y) # try plotting again
fviz_nbclust(vehicles_cleaned, kmeans, method = "wss") + geom_vline(xintercept = 4, linetype = 2) + labs(subtitle = "Elbow method")

# Gap statistic method
set.seed(123)
fviz_nbclust(vehicles_cleaned, kmeans, nstart = 25,  method = "gap_stat", nboot = 50) + labs(subtitle = "Gap statistic method")


# Silhouette method
install.packages("cluster")
library(cluster)
library(factoextra)
fviz_nbclust(vehicles_cleaned, kmeans, method = "silhouette") + labs(subtitle = "Silhouette method")

#compute k-means clustering with k=3
set.seed(123)
final_stat <- kmeans(vehicles_cleaned,3,nstart = 25)
print(final_stat)

#BSS
BSS <- sum(final_stat$size-(colMeans(vehicles_cleaned)-final_stat$centers)^2)
cat("BSS:",BSS,"\n")

#TSS
TSS <- sum((vehicles_cleaned-colMeans(vehicles_cleaned))^2)
cat("TSS:",TSS,"\n")

#WSS
WSS <- sum(final_stat$withinss)
cat("WSS:",WSS,"\n")

#BSS to TSS
ratio_BSS_to_TSS <- BSS/TSS
cat("ratio_BSS_to_TSS:",ratio_BSS_to_TSS,"\n")



#Silhouette plot
pam.res2 <- pam(vehicles_cleaned,3,metric="euclidean",stand = FALSE)
fviz_silhouette(pam.res2,palette="jco",ggtheme=theme_classic())

#average Silhouette width score
sil <- silhouette(final_stat$cluster, dist(vehicles_cleaned))
avg_Sil_width <-mean(sil[,3])
cat("average Silhouette width score:",avg_Sil_width,"\n")













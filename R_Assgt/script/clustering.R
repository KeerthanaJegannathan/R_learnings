###################################################################################
#
#         Segmentation  - This Script contains various Segmentation Analysis
#
###################################################################################

library("NbClust") #loading packages after installing
library("mclust")

seg_data <- read.csv(file = "SelfStatedData.csv",row.names=1)
head(seg_data)

set.seed(1990)
std_seg_data <- scale(seg_data [,c("Screen","Cell","OS","Battery","Price")])
dist <- dist(std_seg_data, method = "euclidean")
as.matrix(dist)[1:6,1:6]

set.seed(1990)
clust <- hclust(dist, method = "ward.D2")
plot(clust)
h_cluster <- cutree(clust, 2)
rect.hclust(clust, k=2, border="red")

table(h_cluster)

hclust_summary <- aggregate(std_seg_data[,c("Screen","Cell","OS","Battery","Price")],by=list(h_cluster),FUN=mean)
hclust_summary

set.seed(1990)
clust <- hclust(dist, method = "ward.D2")
plot(clust)
h_cluster <- cutree(clust, 4)
rect.hclust(clust, k=4, border="red")

hclust_summary <- aggregate(std_seg_data[,c("Screen","Cell","OS","Battery","Price")],by=list(h_cluster),FUN=mean)
hclust_summary

set.seed(1990)
NbClust(data=std_seg_data[,1:5], min.nc=2, max.nc=15, index="all", method="ward.D2")

set.seed(1990)
tablet_cluster3 <-kmeans(std_seg_data, 3, iter.max=100,nstart=100)
tablet_cluster3

set.seed(1990)
NbClust(data=std_seg_data[,1:5], min.nc=2, max.nc=15, index="all", method="kmeans")

set.seed(1990)
mclustBIC(std_seg_data[,1:5],verbose=F)

set.seed(1990)
lca_clust <- Mclust(std_seg_data[,1:5],verbose = FALSE)
summary(lca_clust)

set.seed(1990)
lca_clust <- Mclust(std_seg_data[,1:5],verbose = FALSE,modelNames="EEI")
summary(lca_clust)

lca_clusters <- lca_clust$classification
lca_clust_summary <- aggregate(std_seg_data[,c("Screen","Cell","OS","Battery","Price")],by=list(lca_clusters),FUN=mean)
lca_clust_summary

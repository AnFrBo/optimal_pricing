'CLUSTER ANALYSIS'

mxl_betai_cl <- mxl_betai[,-1]
jeans_dist <-dist(apply(mxl_betai_cl,2,scale))
jeans_clust <- hclust(jeans_dist, method ="ward.D2")
plot(jeans_clust)
plot(rev(jeans_clust$height^2)[1:50], type="b")

jeans_clust_seg <- cutree(jeans_clust, k=3)
str(jeans_clust_seg)
table(jeans_clust_seg)

seg.summ <- function (data , groups) 
{aggregate (data , list(groups), function (x) mean(as.numeric (x)))}

seg.summ(mxl_betai_cl,jeans_clust_seg)

cluster.stats(d = dist(apply(mxl_betai_cl,2,scale)), clustering = jeans_clust_seg) #check dunn and clus.avg.silwidths
#sil plot
sil <- silhouette(jeans_clust_seg, jeans_dist)
quartz()
plot(silhouette(jeans_clust_seg, jeans_dist), col= c("red", "blue", "darkgreen"), main = "Silhouette Plot")
#https://www.datanovia.com/en/lessons/cluster-validation-statistics-must-know-methods/#computing-cluster-validation-statistics-in-r
#Silouette
#Observations with a large Si (almost 1) are very well clustered.
#A small Si (around 0) means that the observation lies between two clusters.
#Negative S_i are probably placed in the wrong cluster.

#description of cluster, merge results with individual data set
indivData <- cbind(indivData, jeans_clust_seg)

absolutes <- function (data , groups) {aggregate (data , list(groups), function (x) sum(as.numeric (x)))}
absolutes(indivData,jeans_clust_seg)
seg.summ(indivData,jeans_clust_seg)

#prep clusters for market simulation
mxl_cl <- cbind(mxl_betai_cl, jeans_clust_seg)

cl1 <- mxl_cl[which(mxl_cl$jeans_clust_seg == 1),]
cl1 <- cl1[,-c(11)]
cl2 <- mxl_cl[which(mxl_cl$jeans_clust_seg == 2),]
cl2 <- cl2[,-c(11)]
cl3 <- mxl_cl[which(mxl_cl$jeans_clust_seg == 3),]
cl3 <- cl3[,-c(11)]

rm(jeans_dist, jeans_clust, seg.summ, absolutes)

   
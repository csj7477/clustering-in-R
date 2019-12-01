library('foreign')
km<-read.spss(file="C:/R_DATA/KM.sav", use.value.label=NA, to.data.frame=TRUE)
library('psych')
alpha(km[, c('cod1','cod2','cod3','cod4','cod5', 'cod6','cod7','cod8','cod9')], na.rm=TRUE)
alpha(km[, c('per1','per2','per3','per4','per5','per6', 'per7')], na.rm=TRUE)
alpha(km[, c('perf1','perf2','perf3','perf4','perf5','perf6','perf7','perf8')], na.rm=TRUE)


# Factor Analysis (Convergent Validity)
library('GPArotation')
km.subset<-na.omit(km)
cor(km.subset)
kmf<-princomp(km.subset[,], cor=TRUE)
summary(kmf)
summary(kmf)$sdev
sum(summary(kmf)$sdev^2)
plot(kmf, type='lines', ylim=c(0,5), main = 'Screeplot for PCA(Principal Component Analysis)')
abline(h=1)
kmf.vm<-principal(km[,], nfactors = 3, rotate = 'varimax')
kmf.vm

#Create Variables
km.subset$cod<-(km.subset$cod1+km.subset$cod2+km.subset$cod3+km.subset$cod4+km.subset$cod5
                +km.subset$cod6+km.subset$cod7+km.subset$cod8+km.subset$cod9)/9
km.subset$per<-(km.subset$per1+km.subset$per2+km.subset$per3+km.subset$per4+km.subset$per5
                +km.subset$per6+km.subset$per7)/7
km.subset$perf<-(km.subset$perf1+km.subset$perf2+km.subset$perf3+km.subset$perf4+km.subset$perf5
                 +km.subset$perf6+km.subset$perf7+km.subset$perf8)/8

#hierachical clustering
install.packages("flexclust")
library(flexclust)
head(km.subset[25:26])
d<-dist(km.subset[25:26])
class(d)
as.matrix(d)[1:185, 25:26]
kms<-km.subset[15:26]
d<-dist(kms)
clustering.average<-hclust(d, method = "ward.D2")
plot(clustering.average, hang=-1, cex=0.9, col="blue", xlab="Company",
     main="Hierachical clustering with Average")

#agglomeration schedule
library(cluster)
hc <- hclust(dist(km.subset[1:185, c(1,2,4)]), method="ward.D2")
data.frame(hc[2:1])
hc
f <- function(hc){
  data.frame(row.names=paste0("Cluster",seq_along(hc$height)),
             height=hc$height,
             components=ifelse(hc$merge<0, hc$labels[abs(hc$merge)], paste0("Cluster",hc$merge)),
             stringsAsFactors=FALSE)
}
f(hc)

#number of clusters
install.packages("factoextra")
library(factoextra)
fviz_nbclust(km.subset, FUN = hcut, method = "wss")
fviz_nbclust(km.subset, FUN = hcut, method = "silhouette")
gap_stat <- clusGap(km.subset, FUN = hcut, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)


install.packages("NbClust")
library(NbClust)
nc<-NbClust(kms, distance="euclidean", min.nc=3, max.nc=15, method="ward.D2")
nc$Best.nc
table(nc$Best.nc[1,])
barplot(table(nc$Best.nc[1,]),
        xlab="Number of Cluster", ylab="Number of Supporting Index",
        main="Number of Cluster Proposed by Indices")


clusters<-cutree(clustering.average, k=4)
clusters
table(clusters)
plot(clustering.average, hang=-1, cex=0.9, col="blue",
     xlab="Company", main="Hierachical Clustering with Ward D2")

rect.hclust(clustering.average, k=4)
km.subset$clusters<-clusters

#mean of cluster
aggregate(km.subset[25:26], by=list(cluster=clusters), mean)

#non-hierachical clustering (k-means)
set.seed(123)
nc<-NbClust(kms, distance="euclidean", min.nc=3, max.nc=15, method="kmeans")
table(nc$Best.nc[1,])
barplot(table(nc$Best.nc[1,]),col="blue",
        xlab="Number of Cluster", ylab="Number of Supporting Index",
        main="Number of Cluster Proposed by Indices")

set.seed(123)
clustering.km<-kmeans(km.subset, centers = 4, nstart = 25)
clustering.km$cluster
km.subset$clusters2<-clustering.km$cluster

aggregate(km.subset[25:26], by=list(cluster=clustering.km$cluster), mean)

#clustering plot
library(cluster)
clusplot(km.subset, clus=clustering.km$cluster, color=TRUE, shade=TRUE,
         labels=2, lines=0, main="Cluster Plot")

fviz_cluster(list(data = km.subset, cluster=clustering.km$cluster))

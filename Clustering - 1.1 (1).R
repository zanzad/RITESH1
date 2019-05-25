## Unsupervised Learning - I

#===========================================================================
##K-means Clustering
#===========================================================================
#library(data.table)

set.seed(2)
# create matrix of 50 observation where first 25 obs. 
#has mean shift relative to next 25 obs.
x=matrix(rnorm(50*2), ncol=2)
plot(x)
plot(x, col=c(rep(3,25),c(rep(4,25))))
x[1:25,1]=x[1:25,1]+3
x[1:25,2]=x[1:25,2]-4
plot(x,col=c(rep(3,25),c(rep(4,25))))

# perform K-means clustering with K=2
km.out=kmeans(x,centers=2,nstart=20)  # nstart for multiple initial cluster assignment
km.out$cluster                # cluster assignment

plot(x, col=(km.out$cluster+2), 
     main = "K-Means Clutering Results with K=2",
     xlab = "", ylab = "",pch=km.out$cluster+16,cex=2)

points(km.out$centers,col=1:2,pch=3,cex=3,lwd=3)

#total within-cluster sum of squares
km.out$tot.withinss
# total between cluster sum of squares
km.out$betweenss
# total sum of square
km.out$totss
# percent withinss of totalss
km.out$tot.withinss/km.out$totss

plot(cbind(no_of_clusters=2:10,
           withinss_by_totss=c(27,20,14,10,8.9,7.3,6.2,5.4,5)))
# perform K-means clustering with K=3

km.out=kmeans(x,3,nstart=20) #nstart=20

km.out$cluster               

plot(x, col=(km.out$cluster+2), main = "K-Means Clutering Results with K=2",
     xlab = "", ylab = "",pch=20,cex=2)
points(km.out$centers,col=1:3,pch=3,cex=3,lwd=3)
km.out
km.out$tot.withinss/km.out$totss

## Another example with iris dataset


data(iris) #iris dataset
iris1<-data.table(iris)
head(iris1)
str(iris1)
table(iris1$Species)


kMeans1<-kmeans(subset(iris1,select=-c(Species)),centers = 3,nstart=20)
kMeans1

iris1$clusters<-as.factor(kMeans1$cluster)
table(iris1$cluster,iris1$Species)

plot(iris1$Petal.Width,iris1$Petal.Length,col=iris1$clusters)
points(x=kMeans1$centers[,4],y=kMeans1$centers[,3],col=1:3,pch=3,cex=2,lwd=2)


#==/K-means Clustering======================================================



#===========================================================================
##Heirarchical Clustering
#===========================================================================

hist(dist(x)) #compute the 50 × 50 inter-observation Euclidean distance matrix
# heirarchical clustering with complete linkage
hc.complete = hclust(dist(x),method="complete")
# heirarchical clustering with average linkage
hc.average = hclust(dist(x),method="average")
# heirarchical clustering with single linkage
hc.single = hclust(dist(x),method="single")
plot(hc.complete ,main =" Complete Linkage ", xlab="", sub ="", cex =.9)
plot(hc.average ,main =" Average Linkage ", xlab="", sub ="", cex =.9)
plot(hc.single ,main =" Single Linkage ", xlab="", sub ="", cex =.9)
cutree(hc.complete,k=2)  
cutree(hc.average,k=2)
cutree(hc.single,k=2)   # No of cluster k=2
cutree(hc.single,h=1.2) # Cut at height h=1.2
plot(x,col=cutree(hc.complete,k=2)+2)
plot(x,col=cutree(hc.average,k=2)+2)
plot(x,col=cutree(hc.single,k=2)+2)
plot(x,col=cutree(hc.single,h=1.2))
# scale variables before performing heirarchical clustering of the obs.
xsc=scale(x)
plot(hclust(dist(xsc),method="complete"),main = "Heirarchical Clustering with Scaled Features")

## Another example for iris dataset

data(iris) #iris dataset
iris1<-data.table(iris)
hc.complete.iris = hclust(dist(iris1),method="complete")
plot(hc.complete.iris ,main =" Iris Complete Linkage ", xlab="", sub ="", cex =.9)

iris1$Clusters<-as.factor(cutree(hc.complete.iris,k=3))
table(iris1$Clusters,iris1$Species)

#=/Heirarchical Clustering==================================================

#===========================================================================
## DBSCAN
#===========================================================================
# DBSCAN for IRIS data
# showplot shows iterationwise results
# Choice of eps and MinPts is crucial

library(fpc)
data(iris)
head(iris,15)
table(iris$Species)
iris=data.frame(iris)
newiris <- iris[-5] # remove class tags
head(newiris,10)
hist(dist(newiris))
ds <- dbscan(newiris, eps=0.42, MinPts=5, showplot=1)
# compare clusters with original class labels
table(ds$cluster, iris$Species)

plot(ds,newiris)

#==/DBSCAN==================================================================


install.packages("dplyr")
install.packages("curl")
install.packages("reshape")
install.packages("tidyr")
install.packages("GMD")
install.packages("factoextra")


library("factoextra")
require("GMD")
library("tidyr")
library("dplyr")
library("curl")
library("reshape")

building_consumption <- read.csv("Finland_masked.csv")
building_area <- read.csv("building_features.csv")

building_consumption <- building_consumption %>% filter (type == 'elect' | type == 'Dist_Heating')

#converting factor to Character

building_consumption$vac <- as.character(building_consumption$vac)

# Handling missing Building names as per class instructions
building_consumption <- transform(building_consumption, vac = ifelse(BuildingID == 81909, 'Building 27', vac))
building_consumption <- transform(building_consumption, vac = ifelse(BuildingID == 82254 | BuildingID == 83427 | BuildingID == 84681, 'Building 9', vac))

grouped_consumption <- building_consumption %>% group_by(vac,type) %>% summarise(totalKwH = sum(Consumption)/n())

selected_features <- merge(grouped_consumption, building_area, by.y = "building", by.x = "vac",all.x = TRUE)

selected_features <- spread(selected_features, key = "type", value = "totalKwH")

selected_features$Dist_Heating[is.na(selected_features$Dist_Heating)] <- 0.0
selected_features$elect[is.na(selected_features$elect)] <- 0.0

  
selected_features$X <- NULL
selected_features$airport_code <- NULL
selected_features$X..address <- NULL


normalize<-function(x)(return((x-min(x))/(max(x)-min(x))))


cluster_features <- as.data.frame(lapply(selected_features[2:6],normalize))


km.out <- kmeans(cluster_features,10,nstart=10) #nstart tells how many times algorithm
# starts from beginning since the final answers is related to initial assignments.
names(km.out)
km.out$cluster # kmeans results
plot(cluster_features, col=(km.out$cluster), main="K-mean result with k=4") #Scatterplot matrix


test_clusters <- cluster_features
test_clusters$group <- km.out$cluster
View(test_clusters[order(test_clusters$group),])



#### Hierarchical clustering
c_features=scale(cluster_features) #Scaling the data

hc.complete=hclust(dist(c_features),method="complete") # Complete linkage type
hc.single=hclust(dist(c_features),method="single")  # single linkage type
hc.average=hclust(dist(c_features),method="average") # average linkage type
hc.centroid=hclust(dist(c_features),method="centroid") # centroid linkage type


par(mfrow=c(2,2)) #Plotting in a matrix form
plot(hc.complete,main='Complete')
plot(hc.single,main='single')
plot(hc.average,main='Average')
plot(hc.centroid,main='centroid')


#Linkage methods
cutree(hc.complete,4)
cutree(hc.single,4)
cutree(hc.average,4)
cutree(hc.centroid,4)

#Table for complete linkage method
h_clusters <- cluster_features
h_clusters$group <- cutree(hc.complete,4)
View(h_clusters[order(h_clusters$group),])


#Euclidean and Manhattan distance
dist.obj <- dist(cluster_features,method="euclidian") # Defining distance object and method
hclust.obj <- hclust(dist.obj) # Creating clusters
css.obj <- css.hclust(dist.obj,hclust.obj) # Creating multi object
names(css.obj)
elbow.obj <- elbow.batch(css.obj) # Creating elbow object
print(elbow.obj)
plot(css.obj,elbow.obj=NULL)

#clustering with more relaxed thresholds (resulting a smaller "good" k)
elbow.obj2 <- elbow(css.obj,ev.thres=0.90,inc.thres=0.05) 
print(elbow.obj2)


##Bend graph
wss <- nrow((cluster_features)-1)*sum(apply(cluster_features,2,var)) 
for (i in 2:15) 
{
  wss[i] <- sum(kmeans(cluster_features,centers=i)$withinss) # Within Sum of Squared for 
  # different number of clusters
}  

plot(1:15, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")


# Visualize k-means clusters
fviz_cluster(km.out, data = cluster_features, geom = "point",
             stand = FALSE, frame.type = "norm")




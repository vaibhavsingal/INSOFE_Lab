rm(list = ls(all=TRUE))

# Use the setwd() function to get to the directory where the data is present

cereals_data = read.csv('Cereals.csv', header = T)
str(cereals_data)
summary(cereals_data)
head(cereals_data)
tail(cereals_data)


###########################################################Data Pre-processing################################

#Store all column names in variable called 'attr'
attr = colnames(cereals_data)
attr

#Store all categorical attributes in 'cat_Attr'
cat_Attr = "shelf"

#Now, how to find the numerical attributes?
num_Attr = setdiff(attr, c(cat_Attr, "name"))
num_Attr

cereals_data$shelf = as.factor(as.character(cereals_data$shelf))

#Now see the structure of the dataframe
str(cereals_data)


#Convert the names of the breakfast cereals to the row names, as this will later help us in visualising the clusters
rownames(cereals_data) <- cereals_data$name


##Drop the name column as it is now just redundant information
#cereals_data <- cereals_data[, -c(colnames(cereals_data) %in% ("name"))]
# (or)
cereals_data$name = NULL

sum(is.na(cereals_data))
library(DMwR)
sum(is.na(cereals_data$shelf))

cereals_data[,num_Attr] <- knnImputation(cereals_data[,num_Attr], k = 3, scale = T)
sum(is.na(cereals_data))

#How do you find missing values per column?
colSums(is.na(cereals_data))

#Convert the categorical to dummy variables (converting to numeric attributes by using dummy)
#Make a copy of the dataframe for later use (mixed attributes)
cereals_data_copy = cereals_data

library("dummies")
##dummy.dataframe(DF,col_names)

shelfDummies = data.frame(dummy(cereals_data$shelf))
#Name the new attributes appropriately
names(shelfDummies) = c("Shelf1","Shelf2","Shelf3")
head(shelfDummies)


#Remove the original attribute 'shelf' and add the newly created dummy variables
cereals_data$shelf = NULL
cereals_data = data.frame(cbind(cereals_data, shelfDummies))
#check the dataframe using head()
head(cereals_data)


#The data must be scaled, before measuring any type of distance metric as the variables with higher ranges
#will significantly influence the distance

cereals_data[, num_Attr] =  scale(cereals_data[,num_Attr], center = T, scale = T)


###########################################################Data exploration################################


#We can use the fviz_dist() function from the factoextra package, to visualize the distances between the observations

# if install.packages("factoextra") doesn't work, use the following
# if(!require(devtools)) install.packages("devtools")
# devtools::install_github("kassambara/factoextra")

library(factoextra)
# Use the get_dist() function from the factoexrtra to calculate inter-observation distances
distance <- get_dist(cereals_data)
fviz_dist(distance, gradient = list(low = "#00AFBB", mid = "white", high = "#FC4E07"))

# The gradient argument, helps us define the color range for the distance scale

#Hierarchical Clustering
#1. Only numerical attributes - distance measure as 'Euclidean'
#Hierarchical Clustering procedure
#Let's now perform hierarchical clustering using the hclust() function, for which we'll first need to calculate the distance measures

# We use the euclidean distance measure (all attributes are numerical now)
dist <- dist(cereals_data, method = "euclidean")

hc_fit <- hclust(dist, method = "ward.D2")
#ward.D2 method - find the pair of clusters that leads to minimum increase in total within-cluster variance after merging


#We can display the dendrogram for hierarchical clustering, using the plot() function
plot(hc_fit)


#Plot clusters being surrounded by a border, using the rect.hclust() function
rect.hclust(hc_fit, k = 6, border = "red")


#Cut the tree to 6 clusters, using the cutree() function
points_hc <- cutree(hc_fit, k = 6)

# Store the clusters in a data frame along with the cereals data
cereals_clusts_hc <- cbind(points_hc, cereals_data)

# Have a look at the head of the new data frame
colnames(cereals_clusts_hc)[1] <- "cluster_hc"
head(cereals_clusts_hc)


#Quality of Clusters Created
#Shiloutte width

#The silhouette width/value is a measure of how similar an object is to its own cluster (cohesion) compared to other clusters (separation) [i.e., intra-cluster cohesion and inter-cluster separation]
#Ranges from -1 to +1
#Values closer to 1 means higher quality of the cluster created

library(cluster)
dist = daisy(x = cereals_data, metric = "euclidean")

## Warning in daisy(x = cereals_data, metric = "euclidean"): binary
## variable(s) 13, 14, 15 treated as interval scaled

sil_value = silhouette(points_hc, dist = dist)
plot(sil_value)


#Try to find the optimal number of clusters where silhouette width would be maximum

sil_value_hc = 0
for (i in 2:20) {
  points_hc <- cutree(hc_fit, k = i)
  sil_value_hc[i] = mean(silhouette(points_hc, dist = dist)[,3])
}
plot(1:20, sil_value_hc, type = "b", xlab = "No: of Clusters", ylab = "Silhouette Width")











#2. Mixed attributes - distance measure as 'gower'
#Hierarchical Clustering procedure - mixed attributes
#Let's now perform same hierarchical clustering using the hclust() function, for mixed datatypes
#Scaling the numeric attributes
str(cereals_data_copy)
cereals_data_copy[,num_Attr] = scale(cereals_data_copy[,num_Attr],scale=T,center=T)

#Calculating gower distance
library(cluster)
gower_dist = daisy(cereals_data_copy,metric = "gower")

#Now that you have the distance matrix, do the hclust()
hc_fit_mixed <- hclust(gower_dist, method = "ward.D2")
#We can display the dendogram for hierarchical clustering, using the plot() function
plot(hc_fit_mixed )

#Cut the tree to 6 clusters, using the cutree() function

points_hc_mixed <- cutree(hc_fit_mixed , k = 6)

# Store the clusters in a data frame along with the cereals data
cereals_clusts_hc_mixed <- cbind(points_hc_mixed, cereals_data_copy)

# Have a look at the head of the new data frame
colnames(cereals_clusts_hc_mixed)[1] <- "cluster_hc_mixed"
head(cereals_clusts_hc_mixed)

#Plot a new dendogram, with each of the clusters being surrounded by a border, using the rect.hclust() function

plot(hc_fit_mixed)
rect.hclust(hc_fit_mixed, k = 5, border = "red")


#Quality of Clusters Created
#Shiloutte value - mixed

library(cluster)
# daisy -Compute all the pairwise dissimilarities (distances) between observations in the data set
gower_dist = daisy(x = cereals_data_copy, metric = "gower")

sil_value_hc_mixed = silhouette(points_hc_mixed, dist = gower_dist)
plot(sil_value_hc_mixed)






#########################K-Means Clustering
#K-Means Clustering procedure
#Build a basic kmeans model with k = 2, using the kmeans() function


set.seed(123)
km_basic <- kmeans(cereals_data, centers = 2, nstart = 20)

str(km_basic)
fviz_cluster(km_basic, cereals_data)

#The kmeans() function returns a list of 9 objects which include the cluster assignments ("cluster"), cluster centers ("centers"), etc. You can further explore the returned object by calling the str() function on the returned object and going through the documentation

#Let's now build a screen plot to choose a "k"


# Initialize wss to 0
wss <- 0

# From 1 upto upto 10 cluster centers, fit the kmeans model
for (i in 1:20) {
  cfit = kmeans(cereals_data, centers = i, nstart = 20)
  # Store the sum of within sum of square
  wss[i] <- sum(cfit$withinss)
}
plot(1:20, wss, type = "b")

set.seed(123)
fviz_nbclust(cereals_data, kmeans, method = "wss")


#Let's choose k as 6 and cluster the data
set.seed(123)
km_clust <- kmeans(cereals_data, 6)
#Not using 'nstart' parameter for reproducability during cluster stability checking. Initial centriods would be chosen based on set seed here.

# after choosing k as 6, let's store the cluster groupings along with the data in a new data frame
km_points <- km_clust$cluster

# Store the cluster assignments in a new data frame
cereals_clusts_km <- as.data.frame(cbind(km_clust$cluster, cereals_data))

# Look at the head of the data
head(cereals_clusts_km)

colnames(cereals_clusts_km)[1] <- "cluster_km"
fviz_cluster(km_clust, cereals_data)

#K-Means Cluster Quality
#Shiloutte value

library(cluster)
dist = daisy(x = cereals_data, metric = "euclidean")
## Warning in daisy(x = cereals_data, metric = "euclidean"): binary
## variable(s) 13, 14, 15 treated as interval scaled
sil_value = silhouette(km_clust$cluster, dist = dist)
plot(sil_value)

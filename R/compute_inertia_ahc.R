######################################################
###  Computing of the intragroup variance for AHC  ###
######################################################

# This function allow to compute the centroid of a data frame
# arguments:
  # i: number of the cluster
  # data: R data frame
  # cluster: nom de la variable indiquant le cluster

clust_centroid <- function(i, data, cluster)
{
  
  return(colMeans(data[cluster == i,]))
  
}

# This function allows to compute the inertia in a R data frame 
# arguments:
  # data: R data frame (all columns are required to be numeric types)

compute_inertia <- function(data)
{
  
  n <- dim(data)[1]
  
  data_with_center <- t(data %>% rbind(colMeans(data)))
  
  squares <- (data_with_center[ ,-(n+1)] -  data_with_center[ ,(n+1)])^2
  
  inertia <- sum(apply(squares, 2, sum)) / n
  
  return(inertia) 
  
}

#compute_inertia(mtcars)

# This function allows to compute the intra group inertia from agglomerative clustering for different number of clusters
# arguments:
  # data: R data frame (all columns are required to be numeric types)
  # max_clusters: maximal number of clusters for which we compute intra group inertia

# Step 1: Find clusters centroids and the global centroid
# Step 2: Compute quadratic differences between cluster centroids and global centroid 
# Step 3: Compute the weighted average of quadratic differences (weigth = size of the cluster)

compute_inertia_ahc <- function(data, max_clusters = 10)
{
  
  n <- dim(data)[1]
  
  intergroup_inertia_ahc <- c()
  
  model_ahc <- hclust(d = dist(data), method = "ward.D")
  
  for (i in 2: max_clusters)
  {
    
    ahc_clusters <- cutree(tree = model_ahc, k = i)
    
    centroids <- sapply(unique(ahc_clusters), clust_centroid, data, ahc_clusters)
    
    centroids <- centroids %>% cbind(rowMeans(centroids))
    
    squares <- (centroids[, -(i+1)] - centroids[, (i+1)])^2
    
    frequencies <- as.vector(table(ahc_clusters)) / n
    
    intergroup_inertia_ahc <- c(intergroup_inertia_ahc, sum(apply(squares, 2, sum)*frequencies))
    
  }
  
  return(intergroup_inertia_ahc)
  
}
######################################################
###  Computing of the intragroup variance for AHC  ###
######################################################

#' Centroid of a cluster
#'
#' This function allows to compute the centroid of a cluster in a R data frame
#'
#' @importFrom stats hclust dist cutree
#' @param i integer that represents the cluster number
#' @param data R data frame (all columns are required to be numeric types)
#' @param cluster character column of the data frame representing the clusters
#' @author Simon CORDE
#' @keywords centroids data frame
#' @references Link to the author's github repository:
#' \url{https://www.github.com/Redcart}
#' @export clust_centroid


clust_centroid <- function(i, data, cluster)
{

  return(colMeans(data[cluster == i,]))

}

#' Inertia of a data frame
#'
#' This function allows to compute the inertia of a R data frame
#'
#' @importFrom stats hclust dist cutree
#' @param data R data frame (all columns are required to be numeric types)
#' @return numeric value representing the total inertia
#' @author Simon CORDE
#' @keywords inertia data frame
#' @references Link to the author's github repository:
#' \url{https://www.github.com/Redcart}
#' @export compute_inertia
#' @examples
#' compute_inertia(mtcars)

compute_inertia <- function(data)
{

  n <- dim(data)[1]

  data_with_center <- t(data %>% rbind(colMeans(data)))

  squares <- (data_with_center[ ,-(n+1)] -  data_with_center[ ,(n+1)])^2

  inertia <- sum(apply(squares, 2, sum)) / n

  return(inertia)

}

#' Intragroup inertia for choosing the optimal number of clusters in Agglomerative Clustering
#'
#' This function allows to compute the inter group inertia from agglomerative clustering
#' for different number of clusters
#'
#' @importFrom stats hclust dist cutree
#' @param data R data frame (all columns are required to be numeric types)
#' @param max_clusters maximal number of clusters for which we intend to compute intra group inertia
#' @return vector of length max_clusters containing the inter group inertia for agglomerative
#' clustering with 1 cluster to max_clusters
#' @author Simon CORDE
#' @keywords inertia data frame agglomerative clustering
#' @references Link to the author's github repository:
#' \url{https://www.github.com/Redcart}
#' @export compute_inertia_ahc
#' @examples
#' data = iris[c(1:4)]
#' results <- compute_inertia_ahc(data=data, max_clusters=15)
#' results

# The basic steps of the functions are the following:
# Step 1: Find clusters centroids and the global centroid
# Step 2: Compute quadratic differences between cluster centroids and global centroid
# Step 3: Compute the weighted average of quadratic differences (weigth = size of the cluster)

compute_inertia_ahc <- function(data, max_clusters=10)
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

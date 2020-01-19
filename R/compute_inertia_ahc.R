######################################################
###  Computing of the intragroup variance for AHC  ###
######################################################

#' @title Centroid of a cluster
#'
#' @description This function allows to compute the centroid of a cluster in a R data frame
#'
#' @importFrom stats hclust dist cutree
#' @param i integer that represents the cluster number
#' @param data R data frame (all columns are required to be numeric types)
#' @param cluster character representing the column name of the data frame representing the clusters
#' @return a vector of coordinates for the centroid of the cluster i
#' @author Simon CORDE
#' @keywords centroids data frame
#' @references Link to the author's github package repository:
#' \url{https://www.github.com/Redcart/helda}
#' @export clust_centroid


clust_centroid <- function(i, data, cluster)
{

  return(colMeans(data[cluster == i,]))

}

#' @title Inertia of a data frame
#'
#' @description This function allows to compute the inertia of a R data frame
#'
#' @importFrom stats hclust dist cutree
#' @param data R data frame (all columns are required to be numeric types)
#' @return a numeric value representing the total inertia
#' @author Simon CORDE
#' @keywords inertia data frame
#' @references Link to the author's github package repository:
#' \url{https://www.github.com/Redcart/helda}
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

#' @title Intragroup inertia for choosing the optimal number of clusters in Agglomerative Clustering
#'
#' @description This function allows to compute the inter group inertia from agglomerative clustering
#' for different number of clusters
#'
#' @importFrom stats hclust dist cutree
#' @param data R data frame (all columns are required to be numeric types)
#' @param method character that specifies the method on which the agglomerative is built upon (by default set to "ward.D")
#' @param max_clusters integer that represents the maximal number of clusters for which we intend to compute intra group inertia
#' @return a vector of length \code{max_clusters} containing the inter group inertia for agglomerative
#' clustering. The ith value of the vector corresponds to the inter group inertia from agglomerative
#' clustering run with i clusters.
#' @keywords inertia data frame agglomerative clustering
#' @references Link to the author's github package repository:
#' \url{https://www.github.com/Redcart/helda}
#' @export compute_inertia_ahc
#' @examples
#' data = iris[c(1:4)]
#' result <- compute_inertia_ahc(data=data, max_clusters=15)

# The basic steps of the functions are the following:
# Step 1: Find clusters centroids and the global centroid
# Step 2: Compute quadratic differences between cluster centroids and global centroid
# Step 3: Compute the weighted average of quadratic differences (weigth = size of the cluster)

compute_inertia_ahc <- function(data, method="ward.D", max_clusters=10)
{

  n <- dim(data)[1]

  intergroup_inertia_ahc <- c()

  model_ahc <- hclust(d = dist(data), method = method)

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

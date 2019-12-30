######################################################
###  Computing of the intragroup variance for AHC  ###
######################################################

#' Centroid of data frame
#'
#' This function allow to compute the centroid of a cluster in a data frame
#'
#' @importFrom stats hclust dist cutree
#' @param i integer that represents the cluster number
#' @param data R data frame (all columns are required to be numeric types)
#' @param cluster column of the data frame representing the clusters
#' @author Simon CORDE
#' @keywords centroids data frame
#' @references Link to the author's github repository:
#' \url{https://www.github.com/Redcart}
#' @export clust_centroid
#'
clust_centroid <- function(i, data, cluster)
{

  return(colMeans(data[cluster == i,]))

}

#' Inertia of data frame
#'
#' This function allows to compute the inertia in a R data frame
#'
#' @importFrom stats hclust dist cutree
#' @param data R data frame (all columns are required to be numeric types)
#' @author Simon CORDE
#' @keywords inertia data frame
#' @references Link to the author's github repository:
#' \url{https://www.github.com/Redcart}
#' @export compute_inertia
#'

compute_inertia <- function(data)
{

  n <- dim(data)[1]

  data_with_center <- t(data %>% rbind(colMeans(data)))

  squares <- (data_with_center[ ,-(n+1)] -  data_with_center[ ,(n+1)])^2

  inertia <- sum(apply(squares, 2, sum)) / n

  return(inertia)

}

#compute_inertia(mtcars)

#' Intragroup inertia for choosing best number of cluster in AHC
#'
#' This function allows to compute the intra group inertia from agglomerative clustering for different number of clusters

#'
#' @importFrom stats hclust dist cutree
#' @param data R data frame (all columns are required to be numeric types)
#' @param max_clusters maximal number of clusters for which we compute intra group inertia
#' @author Simon CORDE
#' @keywords inertia data frame
#' @references Link to the author's github repository:
#' \url{https://www.github.com/Redcart}
#' @export compute_inertia_ahc
#'
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

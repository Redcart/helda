######################################################
###  Computing of the intragroup variance for AHC  ###
######################################################

#' @title Inter group inertia for choosing the optimal number of clusters in Agglomerative Clustering
#'
#' @description This function allows to compute the inter group inertia from agglomerative clustering
#' for different number of clusters
#'
#' @importFrom stats hclust dist cutree
#' @importFrom dplyr %>%
#' @param data a R data frame (all columns are required to be numeric types).
#' @param method a character. This specifies the method on which the agglomerative is built upon (by default set to "ward.D")
#' @param max_clusters an integer. The maximal number of clusters for which we intend to compute inter group inertia
#' @return a vector of length \code{max_clusters} containing the inter group inertia for agglomerative
#' clustering. The ith value of the vector corresponds to the inter group inertia from agglomerative
#' clustering run with i clusters.
#' @author Simon CORDE
#' @keywords inertia data frame agglomerative clustering
#' @references Link to the author's github package repository:
#' \url{https://www.github.com/Redcart/helda}
#' @export compute_inertia_ahc
#' @examples
#' library(dplyr)
#' # We select only numeric features from Iris data set
#' data <- iris %>% select(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
#' result <- compute_inertia_ahc(data = data, max_clusters = 15)
#' result

# The basic steps of the functions are the following:
# Step 1: Find clusters centroids and the global centroid
# Step 2: Compute quadratic differences between cluster centroids and global centroid
# Step 3: Compute the weighted average of quadratic differences (weigth = size of the cluster)

compute_inertia_ahc <- function(data, method = "ward.D", max_clusters = 10)
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

    frequencies <- as.vector(table(ahc_clusters)) / (n-1)

    intergroup_inertia_ahc <- c(intergroup_inertia_ahc, sum(apply(squares, 2, sum)*frequencies))

  }

  # The inter group inertia for one cluster is 0 by definition
  intergroup_inertia_ahc <- c(0, intergroup_inertia_ahc)
  return(intergroup_inertia_ahc)

}

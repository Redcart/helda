###########################################
###  Compute the centroid of a cluster  ###
###########################################

#' @title Centroid of a cluster
#'
#' @description This function allows to compute the centroid of a cluster in a R data frame
#'
#' @importFrom dplyr %>%
#' @param i an integer that represents the cluster number.
#' @param data a R data frame (all columns are required to be numeric types).
#' @param cluster_variable a character. This refers to the column name of the data frame representing
#' the clusters.
#' @return a vector of coordinates of the centroid of the cluster i.
#' @author Simon CORDE
#' @keywords centroids data frame
#' @references Link to the author's github package repository:
#' \url{https://www.github.com/Redcart/helda}
#' @export cluster_centroid
#' @examples
#' library(dplyr)
#' # We create some cluster from k-means on the iris data set
#' data <- iris %>% select(Sepal.Length, Sepal.Width, Petal.Length, Petal.Width)
#' result_kmeans <- kmeans(data, 3)
#' data$cluster <- result_kmeans$cluster
#' # We get the coordinates of the centroid of the second cluster
#' result <- cluster_centroid(i = 2, data = data, cluster_variable = "cluster")
#' result

cluster_centroid <- function(i, data, cluster_variable)
{

  . <- NULL

  data_filtered <- data %>%
    filter_at(cluster_variable, all_vars(. == i)) %>%
    select(-!!cluster_variable)

  return(colMeans(data_filtered))

}

###########################################
###  Compute the centroid of a cluster  ###
###########################################

#' @title Centroid of a cluster
#'
#' @description This function allows to compute the centroid of a cluster in a R data frame
#'
#' @param i an integer that represents the cluster number.
#' @param data a R data frame (all columns are required to be numeric types).
#' @param cluster a character. This refers to the column name of the data frame representing the clusters
#' @return a vector of coordinates of the centroid of the cluster i.
#' @author Simon CORDE
#' @keywords centroids data frame
#' @references Link to the author's github package repository:
#' \url{https://www.github.com/Redcart/helda}
#' @export clust_centroid

clust_centroid <- function(i, data, cluster)
{

  return(colMeans(data[cluster == i,]))

}

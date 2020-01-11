###########################################
###  Function for the kmeans procedure  ###
###########################################

#' @title Kmeans procedure
#'
#' @description This function allows to perform kmeans clustering with some desired output sizes for clusters
#'
#' @importFrom stats kmeans
#' @import dplyr
#' @param data R data frame
#' @param columns vector of columns names of the data frame on which we perform the kmeans algorithm. These features have to be numeric.
#' @param threshold_min integer minimum size for cluster.
#' @param threshold_max integer maximum size fo cluster.
#' @param verbose if set to TRUE print the current state of the procedure (by default set to FALSE).
#' @param seed seed for the random call (if we want the output to be reproducible).
#' @return R data frame
#' @details
#' The R data frame contains the id of the original data frame and a column `cluster` representing
#' the cluster to which the observation belongs to.
#' @author Simon CORDE
#' @keywords kmeans cluster sizes
#' @references Link to the author's github repository:
#' \url{https://www.github.com/Redcart}
#' @export kmeans_procedure
#' @examples
#' data <- iris[, c(1:4)]
#' features <- colnames(iris)[c(1:4)]
#' result <- kmeans_procedure(data=data, columns=features, threshold_min=2, threshold=10,
#' verbose=TRUE, seed=10)

kmeans_procedure <- function(data, columns, threshold_min, threshold_max, verbose=FALSE, seed=42)
{

  cluster_bis <- NULL

  set.seed(seed)

  i <- 1
  b <- 1
  k <- 2
  c <- 1

  n_observation <- 1:dim(data)[1]
  cluster <- NA

  liste_clusterise <- data.frame(n_observation, cluster)
  table(liste_clusterise$cluster)

  test <- FALSE
  ## While clusters are not all filled

  while (test == FALSE)
    {

      if (verbose == TRUE)
       {

        print(paste("boucle : " , b, " nb de clusters : ", k, " N cluster : ", c))

       }


      liste_temp <- liste_clusterise %>%
        filter(is.na(cluster)) %>%
        rename(cluster_bis = cluster) %>%
        select(n_observation, cluster_bis)


      ## We perform clustering on observations that do not have one
      set.seed(seed)

      assign(paste("model_kmean_", b, ".", k, sep = ""),
             kmeans(data[liste_temp$n_observation, columns], centers = k, iter.max = 50, nstart = 5, algorithm = "Lloyd"))

      assign(paste("volumetrie_cluster_", b, ".", k, sep = ""),
             table(eval(as.name(paste("model_kmean_", b, ".", k, sep = "")))$cluster))

      eval(as.name(paste("volumetrie_cluster_", b, ".", k, sep = "")))

      # If at least one cluster has size between threshold_min and threshold_max
      if (any(between(eval(as.name(paste("volumetrie_cluster_", b, ".", k, sep = ""))), threshold_min, threshold_max)))
       {

        # We fill the cluster(s) in that case
        for (i in 1:k)
          {

          # We also fill clusters whose size would fall below the minimum treshold
            if (eval(as.name(paste("volumetrie_cluster_", b, ".", k, sep = "")))[i] <= threshold_max)
             {

               liste_temp$cluster_bis[eval(as.name(paste("model_kmean_", b, ".", k, sep = "")))$cluster==i] <- c

               liste_clusterise <- liste_clusterise %>%
                 left_join(liste_temp, by = "n_observation") %>%
                 mutate(cluster = ifelse(is.na(cluster), cluster_bis, cluster)) %>%
                 select(-cluster_bis)

                 c <- c + 1

             }

          }

          b <- b + 1
          k <- 2

       }

      # If every size cluster is above the maximum threshold we keep increasing the number of clusters
      else if (all(eval(as.name(paste("volumetrie_cluster_", b, ".", k, sep = ""))) > threshold_max))
        {

          k <- k + 1

        }

      # If one cluster has size below the minimum threshold and the other above the maximum threshold
      else if (any(eval(as.name(paste("volumetrie_cluster_", b, ".", k, sep = ""))) < threshold_min))
        {

        # We get back to the previous kmeans and fill the clusters
        for (i in 1:(k-1))
          {
            liste_temp$cluster_bis[eval(as.name(paste("model_kmean_", b, ".", k-1, sep = "")))$cluster==i] <- c

            liste_clusterise <- liste_clusterise %>%
              left_join(liste_temp, by = "n_observation") %>%
              mutate(cluster = ifelse(is.na(cluster), cluster_bis, cluster)) %>%
              select(-cluster_bis)

            c <- c + 1

          }

       }

      else
        {# Test to be sure to not forget a case

        print(" Be careful another case exists !")

        }
      # Are all observations clusterized ?
      test <- sum(is.na(liste_temp$cluster)) == 0

      if (verbose  == TRUE)
       {

         print(table(liste_clusterise$cluster))

       }

    }

    results <- list()
    results$clusters <- liste_clusterise$cluster
    results$frequencies <- table(liste_clusterise$cluster)

    return(results)

}


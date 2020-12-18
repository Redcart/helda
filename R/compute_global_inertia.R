#########################################
###  Computing of the global inertia  ###
#########################################

#' @title Inertia of a data frame
#'
#' @description This function allows to compute the inertia of a R data frame
#'
#' @importFrom dplyr %>%
#' @param data a R data frame (all columns are required to be numeric types).
#' @return a numeric value representing the total inertia.
#' @author Simon CORDE
#' @keywords inertia data frame
#' @references Link to the author's github package repository:
#' \url{https://github.com/Redcart/helda}
#' @export compute_global_inertia
#' @examples
#' result <- compute_global_inertia(mtcars)
#' result

compute_global_inertia <- function(data)
{

  n <- dim(data)[1]

  data_with_center <- t(data %>% rbind(colMeans(data)))

  squares <- (data_with_center[ ,-(n+1)] -  data_with_center[ ,(n+1)])^2

  inertia <- sum(apply(squares, 2, sum)) / (n-1)

  return(inertia)

}

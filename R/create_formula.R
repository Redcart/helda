######################################################
###  Create a formula for machine learning models  ###
######################################################

#' Create a formula
#'
#' This function allows to create a formula from the columns of a data frame very quickly
#'
#' @param data R data frame
#' @param position number of the column in the data frame that we want to predict
#' @return string that contains the formula
#' @details
#' The formula is diplayed with the following format: Y ~ X1 + X2
#' @author Simon CORDE
#' @keywords formula data frame
#' @references Link to the author's github repository:
#' \url{https://www.github.com/Redcart}
#' @export create_formula
#' @examples
#' data <- iris
#' str(data)
#' result_array <- proc_freq(data$Species)
#'

create_formula <- function(data, position = 1)
{

  formula <- paste(colnames(data)[position], " ~ ", paste(colnames(data[-position]), collapse = " + "), sep = "")
  return(formula)

}

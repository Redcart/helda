######################################################
###  Create a formula for machine learning models  ###
######################################################

#' @title Create a formula
#'
#' @description This function allows to create a formula from the columns of a data frame very quickly
#'
#' @param data a R data frame.
#' @param position integer representing the position of the column in the data frame that we want to predict.
#' The other columns are all considered as explanatory variables.
#' @return a string that contains the formula.
#' The formula is displayed with the following format: "Y ~ X1 + X2 + ..."
#' @author Simon CORDE
#' @keywords formula data frame
#' @references Link to the author's github package repository:
#' \url{https://github.com/Redcart/helda}
#' @export create_formula
#' @examples
#' data <- iris
#' str(data)
#' result <- create_formula(data = data, position = 4)
#' result

create_formula <- function(data, position = 1)
{

  formula <- paste(colnames(data)[position], " ~ ", paste(colnames(data[-position]), collapse = " + "), sep = "")
  return(formula)

}

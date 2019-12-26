######################################################
###  Create a formula for machine learning models  ###
######################################################

# This function allows to create a formula from the columns of a data frame
# arguments:
  # data: R data frame
  # position: number of the column in the data frame that we want to predict

#' Create a formula
#'
#' This function permits to reproduce the output of the SAS proc freq
#'
#' @param variable vector on which we want to apply the function
#' @param digits integer that specifies the number of decimals we want to keep in the rounded figures
#' @return data frame of dimension [nb of categories x 5].
#' @details
#' The five columns diplay the following information:
#' \itemize{
#'  \item the first one shows the different categories of the variable on which we want to perform proc_freq
#'  \item then we have frequency, percentage, cumulative frequency and cumulative percentage
#'  }
#' @author Simon CORDE
#' @keywords proc_freq frequency table categorical variable SAS
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

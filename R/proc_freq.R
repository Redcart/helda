########################
###  SAS Proc Freq   ###
########################

#' @title SAS proc freq in R
#'
#' @description This function permits to reproduce the output of the SAS proc freq
#'
#' @param variable a vector on which we want to apply the function
#' @param digits an integer that specifies the number of decimals we want to keep in the rounded figures
#' @return a R data frame of dimension [number of categories x 5].
#' @details
#' The five columns diplay the following information:
#' \itemize{
#'  \item Category: different categories of the original categorical variable
#'  \item Frequency
#'  \item Percentage
#'  \item Cumulative.Frequency
#'  \item Cumulative.Percentage
#'  }
#' @author Simon CORDE
#' @keywords proc_freq frequency table categorical variable SAS
#' @references Link to the author's github package repository:
#' \url{https://www.github.com/Redcart/helda}
#' @export proc_freq
#' @examples
#' data <- iris
#' str(data)
#' result <- proc_freq(data$Species)

proc_freq <- function(variable, digits = 4)
{

  categories <- names(sort(table(variable)))
  frequencies <- as.vector(sort(table(variable)))

  n <- sum(frequencies)
  percentages <- round(frequencies / n, digits)*100
  cum_frequencies <- cumsum(frequencies)
  cum_percentages <- cumsum(percentages)

  result <- data.frame("Category" = categories,
                       "Frequency" = frequencies,
                       "Percentage" = percentages,
                       "Cumulative.Frequency" = cum_frequencies,
                       "Cumulative.Percentage" = cum_percentages)

  return(result)

}

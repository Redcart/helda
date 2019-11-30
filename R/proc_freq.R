########################
###  SAS Proc Freq   ###
########################

# This function permits to reproduce the output of the SAS proc freq
# arguments:
  # variable: vector on which we want to apply the function
  # digits: integer that specifies the number of decimals we want to keep in the rounded figures

#' This function permits to reproduce the output of the SAS proc freq
#'
#' @param variable vector on which we want to apply the function
#' @param digits: integer that specifies the number of decimals we want to keep in the rounded figures
#' @return data frame of dimension [nb of categories x 5].
#' @details
#' The five columns diplay the following information:
#' *the first one shows the different categories of the variable on which we want to perform proc_freq
#' *then we have frequency, percentage, cumulative frequency and cumulative percentage
#' @author Simon CORDE
#' @keywords proc_freq frequency table categorical variable SAS
#' @references Link to the author's github repository:
#' https://www.github.com/Redcart
#' @export
#' @examples
#' data <- iris
#' str(data)
#' result_array <- proc_freq(data$Species)
#' View(result_array)
#'


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

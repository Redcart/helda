##############################################
###  Function for complete empty calendar  ###
##############################################

#' Complete empty calendar
#'
#' This function allows to create a complete empty calendar
#'
#' @import dplyr sqldf
#' @param data R data frame
#' @param key_variable variable name that refers to the key variable in the panel data (ID, ...)
#' @param time_variable time variable name that permits to sort observation on a time scale
#' @param start_year starting year of the time serie
#' @param end_year ending year of the time serie
#' @return a R data frame with the key and time variable for a complete calendar
#' @author Simon CORDE
#' @keywords time series fill gaps calendar
#' @references Link to the author's github repository:
#' \url{https://www.github.com/Redcart}
#' @export create_calendar_day
#'

create_calendar_day <- function(data, key_variable, time_variable, start_year, end_year){

  #, scale_time

  ids <- data %>%
    select(key_variable) %>%
    distinct()

  years <- data.frame(start_year:end_year)

  calendar <- sqldf("SELECT *
                     FROM ids
                     CROSS JOIN years")

  colnames(calendar) <- c(key_variable, time_variable)

  return(calendar)

}

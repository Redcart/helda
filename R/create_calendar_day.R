##############################################
###  Function for complete empty calendar  ###
##############################################

# This function allows to create a complete empty calendar
# arguments:
  # - data: R data frame
  # - key_variable: variable name that refers to the key variable in the panel (ID, ...)
  # - time_variable: time variable name that permits to sort observation on a time scale
  # - start_year: starting year of the time serie
  # - end_year: ending year of the time serie

#' @import dplyr sqldf
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

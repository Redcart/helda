##############################################
###  Function for complete empty calendar  ###
##############################################

#' @title Complete empty calendar
#'
#' @description This function allows to create a complete empty calendar on a year scale
#'
#' @import dplyr sqldf
#' @param data R data frame
#' @param key_variable character that represents the variable name that refers to the key variable in the panel data (ID, ...)
#' @param time_variable character that represents the variable name that permits to sort observation on a time scale
#' @param start_year integer of the starting year of the time serie
#' @param end_year integer of the ending year of the time serie
#' @return a R data frame with the key and time variable for a complete calendar
#' @author Simon CORDE
#' @keywords time series fill gaps calendar
#' @references Link to the author's github package repository:
#' \url{https://www.github.com/Redcart/helda}
#' @export create_calendar_day
#' @examples
#' rep(c("Paris", "Madrid", "Berlin"), each = 10)
#' jeu_donnees <- data.frame("country" = rep(c("France", "Spain", "Germany"), each = 10),
#' "capital" = rep(c("Paris", "Madrid", "Berlin"), each = 10),
#' "year" = 2009:2018,
#' "gdp" = c(NA, NA, 200, 300, 500, 1000, NA, NA, NA, 500,
#'  0, NA, NA, NA, NA, NA, NA, 800, 1200, 1500,
#'  100, 200, 400, 700, 700, 800, 600, 500, NA, NA))
#'  jeu_donnees <- na.omit(jeu_donnees)# we artificially create some gaps in the time series
#'  data_1 <- create_calendar_day(data = jeu_donnees, key_variable = "country", time_variable = "year",
#'  start_year = 2009, end_year = 2018)
#'  data_2 <- start_end_to_fill(data = jeu_donnees, calendar = data_1, gap_variable = "gdp",
#'  key_variable = "country", time_variable = "year")
#'  data_3 <- gap_to_fill(data = data_2, gap_variable = "gdp_corrected_1", key_variable = "country",
#'  time_variable = "year", digits = 1)

create_calendar_day <- function(data, key_variable, time_variable, start_year, end_year){

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

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
#' @return a R data frame with the key and time variable. Each id key is associated with all years between
#' \code{start_year} and \code{end_year}
#' @author Simon CORDE
#' @keywords time series fill gaps calendar
#' @references Link to the author's github package repository:
#' \url{https://www.github.com/Redcart/helda}
#' @export create_calendar
#' @examples
#' library(dplyr)
#' # We take three countries from 2011 to 2018
#' fr_sp_ge_pop <- world_countries_pop %>%
#' filter(country_name %in% c('France', 'Spain', 'Germany')) %>%
#' filter(year > 2010) %>%
#' arrange(country_name, year)
#'
#' # We artificially create some gaps in time series
#' fr_sp_ge_pop$population[c(1, 5, 11, 12, 24)] <- NA
#' fr_sp_ge_pop <- na.omit(fr_sp_ge_pop)
#'
#' data_1 <- create_calendar(data = fr_sp_ge_pop, key_variable = "country_code",
#' time_variable = "year", start_year = 2011, end_year = 2018)
#' data_2 <- start_end_to_fill(data = fr_sp_ge_pop, calendar = data_1, gap_variable = "population",
#' key_variable = "country_code", time_variable = "year")
#' data_3 <- gap_to_fill(data = data_2, gap_variable = "population_corrected_1",
#' key_variable = "country_code", time_variable = "year", digits = 1)

create_calendar <- function(data, key_variable, time_variable, start_year, end_year){

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

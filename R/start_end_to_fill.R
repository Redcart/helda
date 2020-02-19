################################################################
###  Function for filling start and end gaps in time series  ###
################################################################

#' @title Function for filling start and end gaps in time series
#'
#' @description This function allows to fill the start and end gaps of a time series
#' by doing repetition of next (for the start) and previous values (for the end)
#'
#' @import dplyr
#' @param data a R data frame
#' @param calendar a R data frame containing a complete empty calendar (as one can performs with \code{create_calendar_day})
#' @param gap_variable a character. This represents the name of the variable we want to fill the start and end gaps
#' @param key_variable a character. This represents the variable name that refers to the key variable in the panel data (an ID, ...)
#' @param time_variable a character. This represents the time variable name that permits to sort observation on a time scale
#' @return a R data frame that contains the original columns and a new one:
#' \itemize{
#'  \item \code{gap_variable}_corrected_1: the gap variable with starts and ends filled
#'  }
#' @author Simon CORDE
#' @keywords time series fill gaps
#' @seealso \code{\link{create_calendar}} \code{\link{gap_to_fill}}
#' @references Link to the author's github package repository:
#' \url{https://www.github.com/Redcart/helda}
#' @export start_end_to_fill
#' @examples
#' library(dplyr)
#'
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

start_end_to_fill <- function(data, calendar, gap_variable, key_variable, time_variable){

  boo_gap<-boo_gap<-lag_boo_gap<-first_gap<-n_gap<-boo_start_toreplace <- NULL
  values_to_replace<-first_gap_variable<-gap_variable_corrected <- NULL
  boo_end_toreplace<-last_gap_variable <- NULL

  new_var <- paste(gap_variable, "_corrected_1", sep = "")

  start_end <- data %>%
    arrange(get(key_variable), get(time_variable)) %>%
    select(key_variable, gap_variable) %>%
    group_by(get(key_variable)) %>%
    mutate(first_gap_variable = first(get(gap_variable))) %>%
    mutate(last_gap_variable = last(get(gap_variable))) %>%
    ungroup() %>%
    select(-gap_variable) %>%
    distinct()


  data_1 <- calendar %>%
    left_join(data, by = c(key_variable, time_variable)) %>%
    left_join(start_end, by = key_variable) %>%
    arrange(get(key_variable), get(time_variable)) %>%
    ungroup() %>%
    mutate(boo_gap = ifelse(is.na(get(gap_variable)), 1, 0)) %>% # start of the period
    group_by(get(key_variable)) %>%
    mutate(lag_boo_gap = lag(boo_gap)) %>%
    mutate(first_gap = ifelse(boo_gap == 1 & (lag_boo_gap == 0 | is.na(lag_boo_gap)), 1, 0)) %>%
    mutate(n_gap = cumsum(first_gap)) %>%
    mutate(n_gap = ifelse(boo_gap == 1, n_gap, 0)) %>%
    mutate(boo_start_toreplace = ifelse(is.na(first(get(gap_variable))), 1, 0)) %>%
    ungroup() %>%
    mutate(values_to_replace = ifelse(boo_start_toreplace == 1 & n_gap == 1, 1, 0)) %>%
    mutate(gap_variable_corrected = ifelse(values_to_replace == 1, first_gap_variable, get(gap_variable))) %>%
    arrange(get(key_variable), desc(get(time_variable))) %>% # end of the period
    mutate(boo_gap = ifelse(is.na(gap_variable_corrected), 1, 0)) %>%
    group_by(get(key_variable)) %>%
    mutate(lag_boo_gap = lag(boo_gap)) %>%
    mutate(first_gap = ifelse(boo_gap == 1 & (lag_boo_gap == 0 | is.na(lag_boo_gap)), 1, 0)) %>%
    mutate(n_gap = cumsum(first_gap)) %>%
    mutate(n_gap = ifelse(boo_gap == 1, n_gap, 0)) %>%
    mutate(boo_end_toreplace = ifelse(is.na(first(gap_variable_corrected)), 1, 0 )) %>%
    ungroup() %>%
    mutate(values_to_replace = ifelse(boo_end_toreplace == 1 & n_gap == 1, 1, 0)) %>%
    mutate(gap_variable_corrected = ifelse(values_to_replace == 1, last_gap_variable, gap_variable_corrected)) %>%
    arrange(get(key_variable), get(time_variable)) %>%
    select(key_variable, time_variable, gap_variable, gap_variable_corrected) %>%
    rename(!!new_var:=gap_variable_corrected)

  return(data_1)

}

###################################################################
####   Function for filling intermediate gaps in a time serie   ###
###################################################################

#' @title Filling intermediate gaps in a time serie
#'
#' @description This function allows to fill intermediate gaps in panel data by linear interpolation
#'
#' @importFrom stringr str_sub str_length
#' @importFrom rlang :=
#' @import dplyr
#' @param data a R data frame.
#' @param gap_variable a character. This represents the name of the variable we want to fill the start and end gaps.
#' @param key_variable a character. This represents the variable name that refers to the key variable in the panel data (an ID, ...).
#' @param time_variable a character. This represents the time variable name that permits to sort observation on a time scale.
#' @param digits an integer. This is the number of decimals to keep for the rounding (by default set to 2).
#' @return a R data frame that contains the original columns and a new one:
#' \itemize{
#'  \item \code{gap_variable}_corrected_2: the gap variable with intermediate gaps filled
#'  }
#' @author Simon CORDE
#' @keywords time series fill gaps interpolation
#' @seealso \code{\link{create_calendar}} \code{\link{start_end_to_fill}}
#' @references Link to the author's github package repository:
#' \url{https://www.github.com/Redcart/helda}
#' @export gap_to_fill
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

gap_to_fill <- function(data, gap_variable, key_variable, time_variable, digits = 2){

  boo_gap<-lag_boo_gap<-first_gap<-n_gap<-n_gap_step<-gap_variable_before <- NULL
  gap_variable_after<-number_gap_step<-gap_variable_corrected <- NULL

  new_var <- paste(str_sub(gap_variable, 1, str_length(gap_variable)-1), "2", sep = "")

  data_1 <- data %>%
    arrange(get(key_variable), get(time_variable)) %>%
    mutate(boo_gap = ifelse(is.na(get(gap_variable)), 1, 0)) %>%
    group_by_at(key_variable) %>%
    mutate(lag_boo_gap = lag(boo_gap)) %>%
    mutate(first_gap = ifelse(boo_gap == 1 & lag_boo_gap == 0, 1, 0)) %>%
    mutate(n_gap = cumsum(first_gap)) %>%
    mutate(n_gap = ifelse(boo_gap == 1, n_gap, 0)) %>%
    ungroup() %>%
    group_by(n_gap) %>%
    group_by_at(key_variable, .add = TRUE) %>%
    mutate(n_gap_step = cumsum(boo_gap)) %>%
    mutate(number_gap_step = max(n_gap_step)) %>%
    ungroup() %>%
    group_by_at(key_variable) %>%
    mutate(gap_variable_before = lag(get(gap_variable))) %>%
    mutate(gap_variable_after = lead(get(gap_variable))) %>%
    ungroup() %>%
    group_by(n_gap) %>%
    group_by_at(key_variable, .add = TRUE) %>%
    mutate(gap_variable_before = ifelse(is.na(gap_variable_before), 0, gap_variable_before)) %>%
    mutate(gap_variable_after = ifelse(is.na(gap_variable_after), 0, gap_variable_after)) %>%
    mutate(gap_variable_before = max(gap_variable_before)) %>%
    mutate(gap_variable_after = max(gap_variable_after)) %>%
    ungroup() %>%
    mutate(gap_variable_corrected = ifelse(is.na(get(gap_variable)),
                                           gap_variable_before + ((gap_variable_after - gap_variable_before)*n_gap_step/(number_gap_step+1)),
                                           get(gap_variable))) %>%
    mutate(gap_variable_corrected = round(gap_variable_corrected, digits)) %>%
    ungroup() %>%
    select(-boo_gap, -lag_boo_gap, -first_gap, -n_gap, -n_gap_step, -number_gap_step, -gap_variable_before, -gap_variable_after) %>%
    rename(!!new_var:=gap_variable_corrected)

  return(data_1)

}

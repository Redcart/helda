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
#' @param data R data frame
#' @param gap_variable character name of the variable we want to fill the start and end gaps
#' @param key_variable character variable name that refers to the key variable in the panel data (ID, ...)
#' @param time_variable character time variable name that permits to sort observation on a time scale
#' @param digits integer number of decimals to keep for the rounding (by default set to 2)
#' @return a R data frame of dimension containing the original columns and a new one
#' @details
#' The new column is:
#' \itemize{
#'  \item _corrected_2: the variable with intermediate gaps filled
#'  }
#' @author Simon CORDE
#' @keywords time series fill gaps interpolation
#' @references Link to the author's github package repository:
#' \url{https://www.github.com/Redcart/helda}
#' @export gap_to_fill
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

gap_to_fill <- function(data, gap_variable, key_variable, time_variable, digits=2){

  boo_gap<-lag_boo_gap<-first_gap<-n_gap<-n_gap_step<-gap_variable_before <- NULL
  gap_variable_after<-number_gap_step<-gap_variable_corrected <- NULL

  new_var <- paste(str_sub(gap_variable, 1, str_length(gap_variable)-1), "2", sep = "")

  data_1 <- data %>%
    arrange(get(time_variable), get(key_variable)) %>%
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

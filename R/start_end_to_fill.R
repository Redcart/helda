###########################################################
###  Function for filling start and end of time series  ###
###########################################################

# this function allows to fill the start and end gaps of time series by doing repetition
# arguments:
  # - data: R data frame
  # - calendar: complete empty calendar (performed by create_calendar_day)
  # - gap_variable: name of the variable we want to fill the gaps
  # - key_variable: variable name that refers to the key variable in the panel (ID, ...)
  # - time_variable: time variable name that permits to sort observation on a time scale
  # - digits: number of decimals to keep for the rounding

#' Function for filling start and end of time series
#'
#' This function allows to fill the start and end gaps of time series by doing repetition
#'
#' @import dplyr
#' @param data R data frame
#' @param calendar complete empty calendar (performded by \code{create_calendar_day})
#' @param gap_variable name of the variable we want to fill the gaps
#' @param key_variable variable name that refers to the key variable in the panel (ID, ...)
#' @param time_variable time variable name that permits to sort observation on a time scale
#' @param digits number of decimals to keep for the rounding
#' @return a R data frame of dimension containing the original columns and new ones
#' @details
#' The five columns diplay the following information:
#' \itemize{
#'  \item the first one shows the different categories of the variable on which we want to perform proc_freq
#'  \item then we have frequency, percentage, cumulative frequency and cumulative percentage
#'  }
#' @author Simon CORDE
#' @keywords time series fill gaps
#' @references Link to the author's github repository:
#' \url{https://www.github.com/Redcart}
#' @export start_end_to_fill
#'

start_end_to_fill <- function(data, calendar, gap_variable, key_variable, time_variable, digits = 2){

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

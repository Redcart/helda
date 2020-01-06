###################################################################
####   Function for filling intermediate gaps in a time serie   ###
###################################################################

#' Filling intermediate gaps in a time serie
#'
#' This function allows to fill intermediate gaps in panel data by linear interpolation
#'
#' @importFrom stringr str_sub str_length
#' @importFrom rlang :=
#' @import dplyr
#' @param data R data frame
#' @param gap_variable character name of the variable we want to fill the start and end gaps
#' @param key_variable character variable name that refers to the key variable in the panel data (ID, ...)
#' @param time_variable character time variable name that permits to sort observation on a time scale
#' @param digits integer number of decimals to keep for the rounding (by default set to 2)
#' @return a R data frame of dimension containing the original columns and new ones
#' @details
#' The news columns are:
#' \itemize{
#'  \item _corrected_2: the variable with intermediate gaps filled
#'  }
#' @author Simon CORDE
#' @keywords time series fill gaps interpolation
#' @references Link to the author's github repository:
#' \url{https://www.github.com/Redcart}
#' @export gap_to_fill
#'
gap_to_fill <- function(data, gap_variable, key_variable, time_variable, digits=2){

  boo_gap<-lag_boo_gap<-first_gap<-n_gap<-n_gap_step<-gap_variable_before <- NULL
  gap_variable_after<-number_gap_step<-gap_variable_corrected <- NULL

  new_var <- paste(str_sub(gap_variable, 1, str_length(gap_variable)-1), "2", sep = "")

  data <- data %>%
    arrange(get(key_variable), get(time_variable)) %>%
    mutate(boo_gap = ifelse(is.na(get(gap_variable)), 1, 0)) %>%
    group_by(get(key_variable)) %>%
    mutate(lag_boo_gap = lag(boo_gap)) %>%
    mutate(first_gap = ifelse(boo_gap == 1 & lag_boo_gap == 0, 1, 0)) %>%
    mutate(n_gap = cumsum(first_gap)) %>%
    mutate(n_gap = ifelse(boo_gap == 1, n_gap, 0)) %>%
    ungroup() %>%
    group_by(get(key_variable), n_gap) %>%
    mutate(n_gap_step = cumsum(boo_gap)) %>%
    mutate(number_gap_step = max(n_gap_step)) %>%
    ungroup() %>%
    group_by(get(key_variable)) %>%
    mutate(gap_variable_before = lag(get(gap_variable))) %>%
    mutate(gap_variable_after = lead(get(gap_variable))) %>%
    ungroup() %>%
    group_by(get(key_variable), n_gap) %>%
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
    select(-boo_gap, -lag_boo_gap, -first_gap, -n_gap, -n_gap_step, -number_gap_step, -gap_variable_before, -gap_variable_after, -`get(key_variable)`) %>%
    rename(!!new_var:=gap_variable_corrected)

  return(data)

}

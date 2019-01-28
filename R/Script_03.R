###################################################################################################################################################
#######                                                                                                                                      ######
#######                                                       Helpful functions                                                              ######
#######                                                       for data analysis                                                              ######
#######                                                     S.CORDE December 2018                                                            ######
#######                                                                                                                                      ######
###################################################################################################################################################

### Functions coded in this script

# - Procedure that allows to fill the gaps in time series in panel data 

  # - Function that allows to create an empty calendar
  # - Function that permits to do repetition values for the start and the end of time series
  # - Function that permits to fill the intermdediate gaps in time series by linear interpolationb

rm(list = ls())

### Loading of required R packages

library(ggplot2)
library(dplyr)
library(sqldf)
library(stringr)

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

end_start_to_fill <- function(data, calendar, gap_variable, key_variable, time_variable, digits = 2){
  
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

#####################################################################
####    Function for filling intermdediate gaps in a time serie   ###
#####################################################################

# arguments:
  # - data: R data frame
  # - gap_variable: name of the variable we want to fill the gaps
  # - key_variable: variable name that refers to the key variable in the panel (ID, ...)
  # - time_variable: time variable name that permits to sort observation on a time scale
  # - digits: number of decimals to keep for the rounding 

gap_to_fill <- function(data, gap_variable, key_variable, time_variable, digits = 2){

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

############################################
###  Performing example of the function  ###
############################################

# ### creation of a data set
# 
# rep(c("Paris", "Madrid", "Berlin"), each = 10)
# 
# jeu_donnees <- data.frame("country" = rep(c("France", "Spain", "Germany"), each = 10),
#                           "capital" = rep(c("Paris", "Madrid", "Berlin"), each = 10),
#                           "year" = 2009:2018,
#                           "gdp" = c(NA, NA, 200, 300, 500, 1000, NA, NA, NA, 500, 
#                                       0, NA, NA, NA, NA, NA, NA, 800, 1200, 1500,
#                                       100, 200, 400, 700, 700, 800, 600, 500, NA, NA))
# 
# 
# jeu_donnees <- na.omit(jeu_donnees)# we artificially create some gaps in the time series
# 
# ### Check
# 
# data_to_check_1 <- create_calendar_day(data = jeu_donnees, key_variable = "country", time_variable = "year", start_year = 2009, end_year = 2018)
#   
# data_to_check_2 <- end_start_to_fill(data = jeu_donnees, calendar = data_to_check_1, gap_variable = "gdp", key_variable = "country", time_variable = "year", digits = 2)
# 
# data_to_check_3 <- gap_to_fill(data = data_to_check_2, gap_variable = "gdp_corrected_1", key_variable = "country", time_variable = "year", digits = 1)
# 
# ### OK !!!
# 
# 
# ### TO DO
# # creation of empty calendar more complex
# 
# colnames(calendrier) = c("annee", "semaine")
# calendrier$id_semaine <- paste0(calendrier$annee, "W", calendrier$semaine, sep = "")
# calendrier <- data.frame(calendrier)
# str(calendrier)
# 
# 
# calendrier %>% select(annee) %>% distinct() %>% count
# calendrier %>% select(id_semaine) %>% distinct() %>% count
# 
# calendrier_pdv_gamme_cst <- sqldf("SELECT * 
#                                   FROM perimetre_1
#                                   CROSS JOIN calendrier")
# 
# 
# calendrier_pdv_gamme_cst %>% select(`Code PDV`) %>% distinct() %>% count
# calendrier_pdv_gamme_cst %>% select(annee) %>% distinct() %>% count
# calendrier_pdv_gamme_cst %>% select(id_semaine) %>% distinct() %>% count
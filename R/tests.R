##############################################################
### Tests de fonctionnement des fonctions du package helda ###
##############################################################

# windows_to_linux_path()
# 
# C:\Users\scorde\Desktop\Data_Science
# 
# create_formula(mtcars)
# 
# data <- iris
# 
# str(data)
# 
# dat <- proc_freq(data$Species)
# 
# 
# 
# data <- mtcars
# str(data)
# 
# compute_inertia_ahc(data = data)
# 
# ### Questions
# Is it possible for intra group inertia to decreases sometimes when number of clusters increases ?


############################################
###  Performing example of the function  ###
############################################

# data <- trees
# str(data)
# 
# final <- kmeans_procedure(data = data, columns = 1:3, threshold_min = 3, threshold_max = 5)
# final


### creation of a data set

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
# data_to_check_2 <- start_end_to_fill(data = jeu_donnees, calendar = data_to_check_1, gap_variable = "gdp", key_variable = "country", time_variable = "year", digits = 2)
# 
# data_to_check_3 <- gap_to_fill(data = data_to_check_2, gap_variable = "gdp_corrected_1", key_variable = "country", time_variable = "year", digits = 1)

### OK !!!


### TO DO
# creation of empty calendar more complex

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

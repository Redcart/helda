###################################################################################################################################################
#######                                                                                                                                      ######
#######                                                       Helpful functions                                                              ######
#######                                                       for data analysis                                                              ######
#######                                                     S.CORDE December 2018                                                            ######
#######                                                                                                                                      ######
###################################################################################################################################################


### Functions coded in this script

# -	Function that changes automatically \ into / 
# -	Function that creates atomatically formula from the columns names of a data frame
# -	Function that reproduces the SAS proc freq
# -	Function that creates both lift effect curve and lift curve (for predictive classification models)
# - Fonction that permits to compute the intragroup variance from agglomerative clustering

rm(list = ls())

### Loading of required R packages

library(ggplot2)
library(dplyr)


#' Change the path windows ==> linux  
#'
#' This function allows to change "\" into "/" from system paths
#' @input : string that is required to be inserted int console
#' windows_to_linux_path()

windows_to_linux_path <- function()
{
  
  path <- readline("Input File: ")
  new_path <- gsub(pattern = "\\", replacement = "/", x = path, fixed = TRUE)
  return(new_path)
  
}

#windows_to_linux_path()

#C:\Users\scorde\Desktop\Data_Science

######################################################
###  Create a formula for machine learning models  ###
######################################################

# This function allows to create a formula from the columns of a data frame
# arguments:
  # data: R data frame
  # position: number of the column in the data frame that we want to predict

create_formula <- function(data, position = 1)
{
  
  formula <- paste(colnames(data)[position], " ~ ", paste(colnames(data[-position]), collapse = " + "), sep = "")
  return(formula)
  
}

#create_formula(mtcars)


########################
###  SAS Proc Freq   ###
########################

# This function permits to reproduce the output of the SAS proc freq
# arguments:
  # variable: vector on which we want to apply the function
  # digits: integer that specifies the number of decimals we want to keep in the rounded figures

proc_freq <- function(variable, digits = 4)
{
  
  categories <- names(sort(table(variable)))
  frequencies <- as.vector(sort(table(variable)))
  
  n <- sum(frequencies)
  percentages <- round(frequencies / n, digits)*100
  cum_frequencies <- cumsum(frequencies)
  cum_percentages <- cumsum(percentages)
  
  result <- data.frame("Category" = categories, 
                       "Frequency" = frequencies, 
                       "Percentage" = percentages, 
                       "Cumulative.Frequency" = cum_frequencies,
                       "Cumulative.Percentage" = cum_percentages)
  
  return(result)
  
}


# data <- iris
# 
# str(data)
# 
# dat <- proc_freq(data$Species)


#######################################
###  Lift Effect Curve / Lift Curve ###
#######################################

# This function allows to draw the lift effect on a graph
# arguments:
  # predictions: vector of predicted probabilities 
  # true_labels: vector of true labels 
  # positive_label: string for the positive label (Y = 0)

lift_effect <- function(predictions, true_labels, positive_label)
{
  
  data <- data.frame("predictions" = predictions, "true_labels" = true_labels)
  
  data_1 <- data %>% 
    arrange(desc(predictions))
  
  data_2 <- data %>% 
    arrange(desc(true_labels))
  
  n <- length(true_labels)
  step <- floor(n/100)
  points <- seq(1, n, step)
  quantiles <- quantile(0:n)
                        
  lift_1 <- c()
  
  for (i in 1:100)
  {
    
    lift_1 <- c(lift_1, mean(data_1$true_labels[1:points[i]] == positive_label))
    
  }
  
  plot_1 <- ggplot() +
    geom_line(aes(x = points, y = lift_1), color = "#56B4E9") +
    geom_hline(yintercept = mean(true_labels == positive_label), lty = "dashed", color = "grey") +
    coord_cartesian(ylim = c(0.05, 0.8)) +
    scale_x_continuous(breaks = as.vector(quantiles), labels = names(quantiles)) +
    ggtitle("Effect Lift Curve") +
    xlab("Cumulative Population") +
    ylab("% True Positive Label") + 
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5)) 
  
  return(plot_1)
    
}


# This function allows to draw the lift curve on a graph
  # arguments:
    # predictions: vector of predicted probabilities 
    # true_labels: vector of true labels 
    # positive_label: string for the positive label (Y = 0)

lift_curve <- function(predictions, true_labels, positive_label)
{
  
  data <- data.frame("predictions" = predictions, "true_labels" = true_labels)
  
  data_1 <- data %>% 
    arrange(desc(predictions))
  
  data_2 <- data %>% 
    arrange(desc(true_labels))
  
  n <- length(true_labels)
  step <- floor(n/100)
  points <- seq(1, n, step)
    
  lift_2 <- c()
  truth_lift <- c()
  nb_positifs <- sum(data_1$true_labels == positive_label)
  quantiles <- quantile(0:n)
  
  for (i in 1:100)
   {
      
      lift_2 <- c(lift_2, sum(data_1$true_labels[1:points[i]] == positive_label)/nb_positifs)
      truth_lift <- c(truth_lift, sum(data_2$true_labels[1:points[i]] == positive_label)/nb_positifs)
      
   }

  plot_2 <- ggplot() +
     geom_line(aes(x = points, y = lift_2), color = "#56B4E9") +
     geom_line(aes(x = points, y = truth_lift)) +
     geom_segment(aes(x = 0, y = 0, xend = n, yend = 1), lty = "dashed", color = "grey") +
     coord_cartesian(ylim = c(0.05, 1)) +
     scale_x_continuous(breaks = as.vector(quantiles), labels = names(quantiles)) +
     ggtitle("Lift Curve") +
     xlab("Cumulative Population") +
     ylab("Lift") + 
     theme_bw() +
     theme(plot.title = element_text(hjust = 0.5)) 
    
  return(plot_2)
  
}


######################################################
###  Computing of the intragroup variance for AHC  ###
######################################################

# This function allow to compute the centroid of a data frame
# arguments:
  # i: number of the cluster
  # data: R data frame
  # cluster: nom de la variable indiquant le cluster

clust_centroid <- function(i, data, cluster)
{
  
  return(colMeans(data[cluster == i,]))
  
}

# This function allows to compute the inertia in a R data frame 
# arguments:
  # data: R data frame (all columns are required to be numeric types)

compute_inertia <- function(data)
{
  
  n <- dim(data)[1]
  
  data_with_center <- t(data %>% rbind(colMeans(data)))
  
  squares <- (data_with_center[ ,-(n+1)] -  data_with_center[ ,(n+1)])^2
  
  inertia <- sum(apply(squares, 2, sum)) / n
  
  return(inertia) 
  
}

#compute_inertia(mtcars)

# This function allows to compute the intra group inertia from agglomerative clustering for different number of clusters
# arguments:
  # data: R data frame (all columns are required to be numeric types)
  # max_clusters: maximal number of clusters for which we compute intra group inertia

# Step 1: Find clusters centroids and the global centroid
# Step 2: Compute quadratic differences between cluster centroids and global centroid 
# Step 3: Compute the weighted average of quadratic differences (weigth = size of the cluster)

compute_inertia_ahc <- function(data, max_clusters = 10)
{
  
  n <- dim(data)[1]
  
  intergroup_inertia_ahc <- c()
  
  model_ahc <- hclust(d = dist(data), method = "ward.D")

  for (i in 2: max_clusters)
   {
  
    ahc_clusters <- cutree(tree = model_ahc, k = i)
  
    centroids <- sapply(unique(ahc_clusters), clust_centroid, data, ahc_clusters)
  
    centroids <- centroids %>% cbind(rowMeans(centroids))
  
    squares <- (centroids[, -(i+1)] - centroids[, (i+1)])^2
  
    frequencies <- as.vector(table(ahc_clusters)) / n
    
    intergroup_inertia_ahc <- c(intergroup_inertia_ahc, sum(apply(squares, 2, sum)*frequencies))
  
   }

  return(intergroup_inertia_ahc)

}

# data <- mtcars
# str(data)
# 
# compute_inertia_ahc(data = data)

### Questions
# Is it possible for intra group inertia to decreases sometimes when number of clusters increases ?
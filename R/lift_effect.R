##########################
###  Lift Effect Curve ###
##########################

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

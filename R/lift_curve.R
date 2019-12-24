###################
###  Lift Curve ###
###################

# This function allows to draw the lift curve on a graph
# arguments:
  # predictions: vector of predicted probabilities
  # true_labels: vector of true labels
  # positive_label: string for the positive label (Y = 0)

#' Lift curve graph
#'
#' This function allows to draw a lift curve in a ggplot style for binary classification model
#'
#' @param predictions vector of predictions. These are generally the result of a machine learning model.
#' The predictions must be probabilities (a real number between 0 and 1).
#' @param true_labels vector of true labels.
#' @param positive_label string that specify the positive label (Y=1) in the `true_labels`
#' @return plot the lift curve
#' @author Simon CORDE
#' @keywords proc_freq frequency table categorical variable SAS
#' @references Link to the author's github repository:
#' \url{https://www.github.com/Redcart}
#' @export lift_curve
#' @examples
#'
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

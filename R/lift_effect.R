##########################
###  Lift Effect Curve ###
##########################

#' @title Lift effect curve
#'
#' @description This function allows to draw the lift effect on a graph for binary classification model
#'
#' @import ggplot2 dplyr
#' @importFrom stats quantile
#' @param predictions a vector of predictions. These are generally the result of a machine learning model.
#' The predictions must be probabilities (a real number between 0 and 1).
#' @param true_labels a vector of true labels.
#' @param positive_label a character or integer that specify the positive label (Y=1) in the `true_labels`
#' @return a ggplot object containing the lift effect
#' @author Simon CORDE
#' @keywords lift curve machine learning classification
#' @references Link to the author's github package repository:
#' \url{https://www.github.com/Redcart/helda}
#' @export lift_effect
#' @examples
#' data_training <- titanic_training
#' data_validation <- titanic_validation
#' model_glm <- glm(formula = "Survived ~ Pclass + Sex + Age + SibSp + Fare + Embarked",
#' data = data_training,
#' family = binomial(link = "logit"))
#' predictions <- predict(object=model_glm, newdata = titanic_validation, type = "response")
#' lift_effect(predictions = predictions, true_labels = titanic_validation$Survived,
#' positive_label = 1)


lift_effect <- function(predictions, true_labels, positive_label)
{

  data <- data.frame("predictions" = predictions, "true_labels" = true_labels)

  data_1 <- data %>%
    arrange(desc(predictions))

  n <- length(true_labels)
  step <- floor(n/100)
  points <- seq(1, n, step)
  quantiles <- quantile(0:n)

  lift_1 <- c()

  for (i in 1:length(points))
  {

    lift_1 <- c(lift_1, mean(data_1$true_labels[1:points[i]] == positive_label))

  }

  plot_1 <- ggplot() +
    geom_line(aes(x = points, y = lift_1), color = "#56B4E9") +
    geom_hline(yintercept = mean(true_labels == positive_label), lty = "dashed", color = "grey") +
    coord_cartesian(ylim = c(0, 1)) +
    scale_x_continuous(breaks = as.vector(quantiles), labels = names(quantiles)) +
    ggtitle("Lift Effect Curve") +
    xlab("Cumulative Population") +
    ylab("% True Positive Label") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5))

  return(plot_1)

}

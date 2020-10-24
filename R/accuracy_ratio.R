#######################
###  Accuracy Ratio ###
#######################

#' @title Accuracy ratio
#'
#' @description This function allows to compute the accuracy ratio for binary classification model
#'
#' @import ggplot2 dplyr
#' @importFrom stats quantile
#' @param predictions a vector of predictions. These are generally the result of a machine learning model.
#' The predictions must be probabilities (a real number between 0 and 1).
#' @param true_labels a vector of true labels.
#' @param positive_label a character or integer that specify the positive label (Y=1) in the `true_labels`.
#' @return a real number  representing the accuracy ratio.
#' @author Simon CORDE
#' @keywords lift effect accuracy ratio machine learning classification
#' @seealso lift_curve lift_effect
#' @references Link to the author's github package repository:
#' \url{https://www.github.com/Redcart/helda}
#' @export accuracy_ratio
#' @examples
#' data_training <- titanic_training
#' data_validation <- titanic_validation
#' model_glm <- glm(formula = "Survived ~ Pclass + Sex + Age + SibSp + Fare + Embarked",
#' data = data_training,
#' family = binomial(link = "logit"))
#' predictions <- predict(object = model_glm, newdata = data_validation, type = "response")
#' plot <- lift_effect(predictions = predictions, true_labels = data_validation$Survived,
#' positive_label = 1)
#' plot


accuracy_ratio <- function(predictions, true_labels, positive_label)
{

  data <- data.frame("predictions" = predictions, "true_labels" = true_labels)

  data_1 <- data %>%
    arrange(desc(predictions))

  data_2 <- data %>%
    arrange(desc(true_labels))

  n <- length(true_labels)
  points <- seq(1, n, 1)

  lift_2 <- c()
  truth_lift <- c()
  nb_positives <- sum(data_1$true_labels == positive_label)
  quantiles <- quantile(points, probs = seq(0, 1, 0.2))

  for (i in 1:length(points))
    {

      lift_2 <- c(lift_2, sum(data_1$true_labels[1:points[i]] == positive_label)/nb_positives)
      truth_lift <- c(truth_lift, sum(data_2$true_labels[1:points[i]] == positive_label)/nb_positives)

    }

    scale <- 1/n
    acr <- sum(truth_lift * scale)

    return(acr)

}

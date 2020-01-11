library(helda)

context('lift curve function')

test_that("lift curve for titanic data set", {
  data_training <- titanic_training
  data_validation <- titanic_validation
  model_glm <- glm(formula="Survived ~ Pclass + Sex + Age + SibSp + Fare + Embarked",
                   data=data_training,
                   family=binomial(link="logit"))
  predictions <- predict(object=model_glm, newdata=data_validation, type="response")
  result <- lift_curve(predictions=predictions, true_labels=data_validation$Survived, positive_label=1)
  load(file = 'plot_lift_curve_test.Rda')
  expect_equal(result, plot_lift_curve_test)
})

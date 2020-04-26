library(helda)

context('lift effect function')

test_that("lift effect for titanic data set", {
  data_training <- titanic_training
  data_validation <- titanic_validation
  model_glm <- glm(formula="Survived ~ Pclass + Sex + Age + SibSp + Fare + Embarked",
                   data=data_training,
                   family=binomial(link="logit"))
  predictions <- predict(object=model_glm, newdata=data_validation, type="response")
  result <- lift_effect(predictions=predictions, true_labels=data_validation$Survived, positive_label=1)
  if(R.version$major == '4'){
    load(file='lift_effect_test_v4.Rda')
  }else{
    load(file='lift_effect_test_v3.Rda')
  }
  expect_equal(result, lift_effect_test)
})

library(helda)

context('Create formula function')

test_that("create formula", {
  data <- iris
  load(file = 'create_formula_test.Rda')
  expect_equal(create_formula(data), create_formula_test)
})

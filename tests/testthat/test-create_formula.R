library(helda)

context('Create formula function')

test_that("create formula", {
  data <- iris
  result_expected <- create_formula_test
  expect_equal(create_formula(data), result_expected)
})

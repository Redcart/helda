library(helda)
#
context('Create formula function')

# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })

test_that("create formula", {
  #print(getwd())
  data <- iris
  data_expected <- create_formula_test
  print(data_expected)
  expect_equal(create_formula(data), data_expected)
})

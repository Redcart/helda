library(helda)
#
context('Create formula function')

# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })

test_that("create formula", {
  #print(getwd())
  data <- iris
  data_expected <- formula_test
  #print(helda::proc_freq(data$Species))
  expect_equal(create_formula(data), data_expected)
})

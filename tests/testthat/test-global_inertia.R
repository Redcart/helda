library(helda)

context('Global inertia function')

test_that("global inertial", {
  data <- mtcars
  result <- compute_global_inertia(data)
  load('global_inertia_test.Rda')
  expect_equal(result, global_inertia_test)
})

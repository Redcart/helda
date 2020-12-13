library(helda)

context('AHC inter group inertia Function')

test_that("ahc function", {
  data <- mtcars
  results <- compute_inertia_ahc(data = data, max_clusters = 15)
  load('ahc_test.Rda')
  expect_equal(results, ahc_test, check.environment = FALSE)
})

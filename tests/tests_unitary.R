library(testthat)
library(devtools)
devtools::load_all()

test_that("single number", {
  
  data <- iris
  
  str(data)
  
  result_array <- proc_freq(data$Species)
  
  expected_result <- read.csv("results_proc_freq.csv")

  expect_equal(result_array, expected_result)
})
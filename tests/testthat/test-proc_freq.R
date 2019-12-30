library(helda)

context('Proc Freq Function')

test_that("proc freq", {
  data <- iris
  result_expected <- proc_freq_test
  expect_equal(proc_freq(data$Species), result_expected)
})


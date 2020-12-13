library(helda)

context('Proc Freq Function')

test_that("proc freq", {
  data <- iris
  load(file = 'proc_freq_test.Rda')
  expect_equal(proc_freq(data$Species), proc_freq_test, check.environment = FALSE)
})


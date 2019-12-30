library(helda)
#
context('Proc Freq Function')

# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })

test_that("proc freq", {
  #print(getwd())
  data <- iris
  data_expected <- proc_freq_test
  #print(helda::proc_freq(data$Species))
  expect_equal(proc_freq(data$Species), data_expected)
})


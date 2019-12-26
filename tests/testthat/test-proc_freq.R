library(helda)
#
context('Proc Freq Function')

# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })

test_that("proc freq", {
  print(getwd())
  data <- iristtttt
  load(file = "../../data/results_proc_freq.RData")
  print(data)
  print(helda::proc_freq(data$Species))
  expect_equal(helda::proc_freq(data$Species), results_proc_freq)
})


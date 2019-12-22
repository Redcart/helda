test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})


print(getwd())
data <- iris
result_proc_freq <- load(file = "../../results_proc_freq.RData")

test_that("proc freq", {
  expect_equal(proc_freq(data$Species), result_proc_freq)
})


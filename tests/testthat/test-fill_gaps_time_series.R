library(helda)

context('Integration test for fill in gaps time series')

test_that("fill gaps time series functions", {
  rep(c("Paris", "Madrid", "Berlin"), each = 10)

  jeu_donnees <- data.frame("country" = rep(c("France", "Spain", "Germany"), each = 10),
                            "capital" = rep(c("Paris", "Madrid", "Berlin"), each = 10),
                            "year" = 2009:2018,
                            "gdp" = c(NA, NA, 200, 300, 500, 1000, NA, NA, NA, 500,
                                      0, NA, NA, NA, NA, NA, NA, 800, 1200, 1500,
                                      100, 200, 400, 700, 700, 800, 600, 500, NA, NA))

  jeu_donnees <- na.omit(jeu_donnees)# we artificially create some gaps in the time series

  data_to_check_1 <- create_calendar_day(data = jeu_donnees, key_variable = "country", time_variable = "year", start_year = 2009, end_year = 2018)

  data_to_check_2 <- start_end_to_fill(data = jeu_donnees, calendar = data_to_check_1, gap_variable = "gdp", key_variable = "country", time_variable = "year")

  data_to_check_3 <- gap_to_fill(data = data_to_check_2, gap_variable = "gdp_corrected_1", key_variable = "country", time_variable = "year", digits = 1)

  load(file='fill_gaps_test.Rda')

  expect_equal(data_to_check_3, fill_gaps_test)
})


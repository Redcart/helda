library(helda)

context('Integration test for fill in gaps time series')

test_that("fill gaps time series functions", {

  # We take three countries from 2011 to 2018
  fr_sp_ge_pop <- world_countries_pop %>%
   filter(country_name %in% c('France', 'Spain', 'Germany')) %>%
   filter(year > 2010) %>%
   arrange(country_name, year)

  # We artificially create some gaps in time series
  fr_sp_ge_pop$population[c(1, 5, 11, 12, 24)] <- NA
  fr_sp_ge_pop <- na.omit(fr_sp_ge_pop)

  data_1 <- create_calendar(data = fr_sp_ge_pop, key_variable = "country_code",
   time_variable = "year", start_year = 2011, end_year = 2018)
  data_2 <- start_end_to_fill(data = fr_sp_ge_pop, calendar = data_1, gap_variable = "population",
   key_variable = "country_code", time_variable = "year")
  data_3 <- gap_to_fill(data = data_2, gap_variable = "population_corrected_1",
   key_variable = "country_code", time_variable = "year", digits = 1)
  load(file='fill_gaps_test.Rda')

  expect_equal(data_3, fill_gaps_test)
})

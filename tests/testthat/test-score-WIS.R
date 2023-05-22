library(testthat)
library(dplyr)

# Test 1: Ensure function returns a data.frame object with expected columns
test_that("Returned output is a data.frame with expected columns", {
  # Create dummy inputs
  forecast_data <- data.frame(model_name = rep(c("model_a", "model_b"), each = 4),
                              target_end_date = rep(seq(as.Date("2022-01-01"), by = "day", length.out = 4), times = 2),
                              forecast_date = as.Date(rep(c("2022-01-02"), times = 8)),
                              location = rep(c("location_1", "location_2"), each = 4),
                              type = rep("quantile", times = 8),
                              quantile = sort(c(0.1, 0.5, 0.9, 0.1, 0.5, 0.9, 0.5, 0.9)),
                              value = rnorm(8))

  truth <- data.frame(date = c(as.Date("2022-01-01"), as.Date("2022-01-01")),
                      location = c("location_1", "location_2"),
                      value = rnorm(2))

  # Call score_WIS function
  scores_out <- score_WIS(forecast_data, truth, ingest = "dataframe")

  # Check column names
  expect_identical(colnames(scores_out), c("model", "date", "location_name", "forecast_date", "WIS"))
  # Check class of object
  expect_identical(class(scores_out), "data.frame")
})

# Test 2: Ensure function produces correct WIS score for a simple input case
test_that("Correct WIS score is produced for a simple input case", {
  # Create dummy inputs
  forecast_data <- data.frame(model_name = rep("model_a", times = 4),
                              target_end_date = rep(seq(as.Date("2022-01-01"), by = "day", length.out = 4), times = 1),
                              forecast_date = as.Date(rep(c("2022-01-02"), times = 4)),
                              location = rep(c("location_1"), each = 4),
                              type = rep("quantile", times = 4),
                              quantile = sort(c(0.1, 0.5, 0.9, 0.5)),
                              value = c(1, 2, 3, 4))

  truth <- data.frame(date = c(as.Date("2022-01-01")),
                      location = c("location_1"),
                      value = c(2.5))

  # Call score_WIS function
  scores_out <- score_WIS(forecast_data, truth, ingest = "dataframe")

  # Check the calculated WIS score
  expect_equal(scores_out$WIS, 0.25)
})

# Test 3: Ensure function produces expected output when missing true values are encountered
test_that("Expected output is produced when missing true values are encountered", {
  # Create dummy inputs
  forecast_data <- data.frame(model_name = rep("model_a", times = 4),
                              target_end_date = rep(seq(as.Date("2022-01-01"), by = "day", length.out = 4), times = 1),
                              forecast_date = as.Date(rep(c("2022-01-02"), times = 4)),
                              location = rep(c("location_1"), each = 4),
                              type = rep("quantile", times = 4),
                              quantile = sort(c(0.1, 0.5, 0.9, 0.5)),
                              value = c(1, 2, 3, 4))

  truth <- data.frame(date = c(as.Date("2022-01-01"), as.Date("2022-01-02")),
                      location = c("location_1", "location_2"),
                      value = c(NA, 3))

  # Call score_WIS function
  suppressMessages(scores_out <- score_WIS(forecast_data, truth, ingest = "dataframe"))

  # Check that the missing data alert is shown
  expect_output(scores_out, regex("A total of 1 predictions weren't evaluated due lack of truth data"))
})

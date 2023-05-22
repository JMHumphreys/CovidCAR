context("setup_analysis")

test_that("Output directory path should be created correctly", {
  report_date <- "2022-01-31"
  training_period <- 56
  forecast_horizon <- 14

  setup_analysis(report_date, training_period, forecast_horizon, output_dir = "/my/output/dir")

  expect_true(dir.exists("/my/output/dir/2022-01-31-CovidCAR-run"))
})

context("setup_analysis")

test_that("YAML file should be written with correct values", {
  report_date <- "2022-01-31"
  training_period <- 56
  forecast_horizon <- 14

  setup_analysis(report_date, training_period, forecast_horizon)

  expect_true(file.exists(paste0(getwd(), "/", report_date, "-CovidCAR-run", Sys.Date(), "/setup-",Sys.Date(),".yml")))
})

context("setup_analysis")

test_that("setup_analysis should error when given invalid inputs", {
  report_date <- "2022-01-31"
  training_period <- -10
  forecast_horizon <- 20

  expect_error(setup_analysis(report_date, training_period, forecast_horizon), message = "Error: 'training_period' must be a positive integer less than or equal to 56.")
})




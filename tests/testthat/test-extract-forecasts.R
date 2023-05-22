test_that("Test 1 - Verify if function throws an error when su_yaml$forecast_date is NULL",{
  su_yaml <- list(forecast_date = NULL)
  dataStack <- "datastack"
  train_data <- "traindata"
  mod_out <- list(model1 = "model1")

  expect_error(extract_forecasts(mod_out, dataStack, train_data), "Run setup_analysis() to designate key analysis dates before proceeding")
})


test_that("Test 2 - Verify if output of the function has all the required columns and is of class 'data.frame'", {

  # create some dummy input data
  set.seed(123)
  train_data <- data.frame(location = c(rep("US", 5), rep("UK", 5)),
                           date = c(seq(as.Date("2020-01-01"), as.Date("2020-01-05"), by = "days"),
                                    seq(as.Date("2020-01-01"), as.Date("2020-01-05"), by = "days")))

  mod_out <- list(model1 = list(summary.fitted.values = matrix(runif(25), ncol = 23)))

  # define su_yaml with a forecast_date
  su_yaml <- list(forecast_date = "2020-01-07")

  # call extract_forecasts
  extracted_data <- extract_forecasts(mod_out, dataStack, train_data)

  # check if output class is data.frame
  expect_is(extracted_data, "data.frame")

  # check if all required columns are present in the output
  expected_columns <- c("forecast_date", "location", "target", "target_end_date", "type", "quantile", "value")
  expect_equal(all(expected_columns %in% colnames(extracted_data)), TRUE)
})

test_that("Test 3 - Verify if the function is able to successfully create the 'forecast' directory and save .csv files", {

  # create some dummy input data
  set.seed(123)
  train_data <- data.frame(location = c(rep("US", 5), rep("UK", 5)),
                           date = c(seq(as.Date("2020-01-01"), as.Date("2020-01-05"), by = "days"),
                                    seq(as.Date("2020-01-01"), as.Date("2020-01-05"), by = "days")))

  mod_out <- list(model1 = list(summary.fitted.values = matrix(runif(25), ncol = 23)))

  # define su_yaml with a forecast_date
  su_yaml <- list(forecast_date = "2020-01-07")

  # call extract_forecasts
  extracted_data <- extract_forecasts(mod_out, dataStack, train_data)

  # check if directory 'forecasts' has been created in 'su_yaml$out_dir_name'
  expected_dir_name <- paste0(su_yaml$out_dir_name,"/forecasts")
  expect_true(dir.exists(expected_dir_name))

  # check if file has been saved in the expected path
  expected_filename <- paste0(expected_dir_name, "/", as_date(su_yaml$forecast_date), "-TeamName-model1.csv")
  expect_true(file.exists(expected_filename))
})


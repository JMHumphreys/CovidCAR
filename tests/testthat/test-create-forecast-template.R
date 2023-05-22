
train_data <- data.frame(
  location = c("location1", "location2"),
  value = c(10, 20),
  signal = c("hosp", "hosp")
)

expect_error(create_forecast_template(train_data))


train_data <- data.frame(
  location = c("location1", "location2"),
  value = c(10, 20),
  signal = c("hosp", "hosp")
)

output <- create_forecast_template(train_data)

expect_equal(nrow(output), 14)
expect_equal(ncol(output), 6)


train_data <- data.frame(
  location_name = c("loc1", "loc1", "loc2", "loc2"),
  location = c("location1", "location1", "location2", "location2"),
  Region = c("region1", "region1", "region2", "region2"),
  date = as_date(c("2022-01-01", "2022-01-02", "2022-01-01", "2022-01-02")),
  value = c(10, 20, 30, 40),
  signal = c("hosp", "hosp", "hosp", "hosp"),
  day = c("Saturday", "Sunday", "Saturday", "Sunday"),
  trn_tst = c("train", "train", "train", "train")
)

output <- create_forecast_template(train_data)

get_Region_table <- train_data %>%
  select(location_name, location, Region) %>%
  distinct()

expected_output <- left_join(output, get_Region_table, by = "location")
expect_equal(output, expected_output)



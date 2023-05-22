scores_df <- data.frame(date = seq(as.Date("2022/01/01"), as.Date("2022/12/31"), by = "day"),
                        location_name = rep(c("New York", "Los Angeles"), each = 365),
                        forecast_date = rep(seq(as.Date("2022/01/01"), as.Date("2022/12/31"), by = "day"), 2),
                        WIS = runif(730, 0, 1),
                        model = rep(c("model1", "model2"), each = 365))

test_that("plot_WIS_lines returns a plot", {

  expect_s3_class(plot_WIS_lines(scores_df), "ggplot")

})

scores_df <- data.frame(date = seq(as.Date("2022/01/01"), as.Date("2022/12/31"), by = "day"),
                        location_name = rep(c("New York", "Los Angeles"), each = 365),
                        forecast_date = rep(seq(as.Date("2022/01/01"), as.Date("2022/12/31"), by = "day"), 2),
                        WIS = runif(730, 0, 1),
                        model = rep(c("model1", "model2"), each = 365))

test_that("plot_WIS_lines scales WIS values when range is set to 'scaled'", {

  expect_equal(nrow(scores_df), 730)

  # Non-scaled
  orig_plot <- plot_WIS_lines(scores_df, range = "abs")
  orig_data <- layer_data(orig_plot)[,c("x","y","group","colour")]
  expect_equal(nrow(orig_data), 1460)

  # Scaled
  scaled_plot <- plot_WIS_lines(scores_df, range = "scaled", scale_model = "model1")
  scaled_data <- layer_data(scaled_plot)[,c("x","y","group","colour")]
  expect_equal(nrow(scaled_data), 730)
  expect_true(all(scaled_data$y <= 1))
  expect_true(all(scaled_data$y >= 0))

})

scores_df <- data.frame(date = seq(as.Date("2022/01/01"), as.Date("2022/12/31"), by = "day"),
                        location_name = rep(c("New York", "Los Angeles"), each = 365),
                        forecast_date = rep(seq(as.Date("2022/01/01"), as.Date("2022/12/31"), by = "day"), 2),
                        WIS = runif(730, 0, 1),
                        model = rep(c("model1", "model2"), each = 365))

test_that("plot_WIS_lines groups data by date or tile as specified by the 'by' argument", {

  expect_equal(nrow(scores_df), 730)

  # Group by date
  date_plot <- plot_WIS_lines(scores_df, by = "date")
  date_data <- layer_data(date_plot)[,c("x","y","group","colour")]
  expect_equal(nrow(unique(date_data[,c("x","group")])), 730)

  # Group by tile
  tile_plot <- plot_WIS_lines(scores_df, by = "tile")
  tile_data <- layer_data(tile_plot)[,c("x","y","group","fill")]
  expect_equal(nrow(unique(tile_data[,c("x","group")])), 2)

})

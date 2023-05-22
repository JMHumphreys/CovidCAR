# Test 1: Check if output is numeric
test_that("Output is numeric", {
  x <- c(1, 2, 3, 4, 5)
  y <- c(0.5, 1.5, 2.5, 3.5, 4.5)
  expect_type(time_index(x, y), "numeric")
})

# Test 2: Check output length
test_that("Output has the same length as input x", {
  x <- c(1, 2, 3, 4, 5)
  y <- c(0.5, 1.5, 2.5, 3.5, 4.5, 5.5)
  expect_equal(length(time_index(x, y)), length(x))
})

# Test 3: Check correctness of output
test_that("Output is correct", {
  x <- c(1, 2, 3, 4, 5)
  y <- c(0.5, 1.5, 2.5, 3.5, 4.5)
  expect_equal(time_index(x, y), y)
})

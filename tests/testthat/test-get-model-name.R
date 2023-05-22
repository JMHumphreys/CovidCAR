# Test if the function returns correct model name for the given filename
test_that("get_model_name returns correct model name", {
  expect_equal(get_model_name("code_data-regression.csv"), "regression")
})


# Test if the function returns NULL if filename does not contain a "-" character
test_that("get_model_name returns NULL if '-' character is not present in the filename", {
  expect_is(get_model_name("codedataregression.csv"), "NULL")
})

# Test if the function returns NULL if it is passed an empty string as filename
test_that("get_model_name returns NULL if passed an empty string", {
  expect_is(get_model_name(""), "NULL")
})

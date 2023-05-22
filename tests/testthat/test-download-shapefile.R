# Test if the output is a spatial object
test_that("output is a spatial object", {
  sp_object <- download_boundaries()
  expect_is(sp_object, "Spatial")
})

# Test if the proj parameter works as expected
test_that("proj parameter works as expected", {
  sp_object1 <- download_boundaries(proj = "EPSG:4326")
  expect_equal(sp_object1@projargs, "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")

  sp_object2 <- download_boundaries(proj = "EPSG:3857")
  expect_equal(sp_object2@projargs, "+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext +no_defs")
})

# Test if it raises an error when url is not reachable
test_that("error is raised when url is not reachable", {
  expect_error(download_boundaries(url_state = "https://www.randomurl.com"))
})


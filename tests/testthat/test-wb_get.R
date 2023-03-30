test_that("empty user or wb_path arguments produce errors", {
  expect_error(wb_get("2022-06-22/h3.tif", tempdir(), wb_path = "Raw Data/licor"))
  expect_error(wb_get("2022-06-22/h3.tif", tempdir(), user = "aragaki-kai"))
  expect_error(wb_get("2022-06-22/h3.tif", tempdir()))
})

# Test that non-404 errors are caught
# Test that recursive directories are made

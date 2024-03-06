test_that("constructor works", {
  obj <- MetaNLP("data/test_data.csv")
  expect_equal(
    nrow(obj@data_frame),
    5
  )
})

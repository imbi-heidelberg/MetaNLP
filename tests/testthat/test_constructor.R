test_that("constructor works", {
  obj <- MetaNLP("data/test_data.csv")
  obj2 <- MetaNLP("data/test_data.csv", min_appear = 1)

  expect_equal(
    nrow(obj@data_frame),
    4
  )

  expect_equal(
    obj2@data_frame$id,
    obj@data_frame$id
  )

  expect_equal(
    obj2@data_frame$decision,
    obj@data_frame$decision
  )

  expect_equal(
    obj@data_frame$decision,
    c("no", "no", "yes", "yes")
  )

  expect_true(
    ncol(obj2@data_frame) > ncol(obj@data_frame)
  )

  expect_equal(
    obj@data_frame$paper,
    c(1, 1, 1, 3)
  )
})

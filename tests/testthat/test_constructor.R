test_that("constructor works", {
  obj <- MetaNLP("data/test_data.csv")
  obj2 <- MetaNLP("data/test_data.csv", bounds = c(1, Inf))
  obj3 <- MetaNLP("data/test_data.csv", bounds = c(3,6), word_length = c(4,8))

  # rows containing na values are dropped
  expect_equal(
    nrow(obj@data_frame),
    4
  )

  # columns with id should be unchanged by min_appear and word_length
  expect_equal(
    obj2@data_frame$id,
    obj@data_frame$id
  )

  expect_equal(
    obj2@data_frame$decision,
    obj@data_frame$decision
  )

  # correct conversion from "include/exclude" to "yes/no"
  expect_equal(
    obj@data_frame$decision,
    c("no", "no", "yes", "yes")
  )

  # when we allow for words that appear at least once, the number of columns
  # should be higher
  expect_true(
    ncol(obj2@data_frame) > ncol(obj@data_frame)
  )

  # bounds and word_length stick to conditions
  expect_true(
    min(colSums(obj3@data_frame[-(1:2)]))      >= 3 &
    max(colSums(obj3@data_frame[-(1:2)]))      <= 6 &
    min(nchar(names(obj3@data_frame[-(1:2)]))) >= 4 &
    max(nchar(names(obj3@data_frame[-(1:2)]))) <= 8
  )

  # exemplary row to test correct results
  expect_equal(
    obj@data_frame$paper,
    c(1, 1, 1, 3)
  )
})

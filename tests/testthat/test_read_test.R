test_that("Test data is processed correctly", {

  # To test this function, the original data set was changed in exactly three
  # ways: First, the word "mathematics" was added twice to the first row.
  # Second, the word "hello" was added to the third row. Third, the words "account"
  # "amet" were removed.

  source_path  <- test_path("data", "test_data.csv")
  obj <- MetaNLP(source_path)

  source_path2 <- test_path("data", "test_data_changed.csv")
  test_obj <- read_test_data(obj, source_path2)

  # test 1: mathematics was detected
  expect_equal(
    test_obj@data_frame$mathemat, c(2, 0, 0, 2)
  )

  # test 2: column hello was not added, as it does not appear in training matrix
  expect_true(
    is.null(test_obj@data_frame$hello)
  )

  # test 3: column "account" and "amet" only consist of zeros, as they do not appear in test matrix
  expect_equal(
    test_obj@data_frame$account, rep(0, 4)
  )

  expect_equal(
    test_obj@data_frame$amet, rep(0, 4)
  )

  # test 4: column decision_ does not exist
  expect_true(
    is.null(test_obj@data_frame$decision_)
  )

  # test 5: training and test data are equal in the remaining columns
  data_obj <- obj@data_frame[-c(1, 2, 3, 5, 29)]
  data_test <- test_obj@data_frame[-c(1, 2, 4, 28)]

  expect_equal(
    data_obj, data_test
  )

  # test 6: read_test_data also takes MetaNLP object as second argument
  expect_equal(
    test_obj@data_frame, read_test_data(obj, MetaNLP(source_path2))@data_frame
  )
})

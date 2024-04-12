test_that("Deletion functions work", {

  # The first four words of the vector exist in (possibly modified) form in the
  # word count matrix of the test data
  obj <- MetaNLP("data/test_data.csv")
  deletion_list <- c("beautiful", "considering", "facts", "find", "algebra")
  obj_delete <- delete_words(obj, deletion_list)
  obj_stop_words <- delete_stop_words(obj)

  # delete_words and delete_stop_words return MetaNLP objects

  expect_true(
    isClass(obj_delete, MetaNLP)
  )

  expect_true(
    isClass(obj_stop_words, MetaNLP)
  )

  # the data frame in obj_delete should have 4 columns less
  expect_true(
    ncol(obj@data_frame) - 4 == ncol(obj_delete@data_frame)
  )

  # check that the correct columns have been deleted
  expect_true(
    !("beauti" %in% names(obj_delete) |
      "consid" %in% names(obj_delete) |
      "fact"   %in% names(obj_delete) |
      "find"   %in% names(obj_delete))
  )

  # check that stop words have been deleted
  expect_false(
    !(any(tm::stopwords() %in% names(obj@data_frame)))
  )

  expect_true(
    !(any(tm::stopwords() %in% names(obj_stop_words@data_frame)))
  )
})

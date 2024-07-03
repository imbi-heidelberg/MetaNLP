test_that("Deletion functions work", {

  # The first four words of the vector exist in (possibly modified) form in the
  # word count matrix of the test data
  source_path <- test_path("data", "test_data.csv")
  obj <- MetaNLP(source_path)
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

test_that("Special characters can be replaces", {

  # load french data set
  source_path_fr <- test_path("data", "french_data.csv")
  obj_fr <- MetaNLP(source_path_fr, bounds = c(1, Inf), language = "french")

  # add a column name that contains all possible special characters
  obj_fr@data_frame <- data.frame(obj_fr@data_frame,
                                  "äàáâãåăëèéêîïíöôóõüùúûßçñ" = c(0, 1))
  obj_fr_rep <- replace_special_characters(obj_fr)

  # check that all characters were replaced
  expect_true(
    all(unlist(strsplit(colnames(obj_fr_rep@data_frame[-c(1, 2)]), "")) %in% letters)
  )

  expect_true(
    !is.null(obj_fr_rep@data_frame$aaaaaaaeeeeiiioooouuuusscn)
  )
})

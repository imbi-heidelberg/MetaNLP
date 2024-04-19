test_that("Summary works", {

  source_path <- test_path("data", "test_data_large.csv")
  obj <- MetaNLP(source_path)
  summ <- summary(obj)

  # test 1: summary returns list
  expect_true(
    is.list(summ)
  )

  # test 2: list hast 3 entries
  expect_true(
    length(summ) == 3
  )

  # test 3: entries are called Total, Exclude and Include
  expect_equal(
    names(summ), c("Total", "Exclude", "Include")
  )

  # test 4:  summary adheres to argument n
  expect_true(
    all(c(ncol(summ$Total)   == 5,
          ncol(summ$Exclude) == 5,
          ncol(summ$Include) == 5))
  )

  summ_long <- summary(obj, n = 10)

  expect_true(
    all(c(ncol(summ_long$Total)   == 10,
          ncol(summ_long$Exclude) == 10,
          ncol(summ_long$Include) == 10))
  )

  # test 5: stop words are not deleted when stop_words = TRUE

  summ2 <- summary(obj, stop_words = TRUE)

  expect_false(
    any(c(colnames(summ$Total)[c(1, 2)]   == c("the", "and"),
          colnames(summ$Exclude)[c(1, 2)] == c("the", "and"),
          colnames(summ$Include)[c(1, 2)] == c("the", "and")))
  )

  expect_true(
    all(c(colnames(summ2$Total)[c(1, 2)]   == c("the", "and"),
          colnames(summ2$Exclude)[c(1, 2)] == c("the", "and"),
          colnames(summ2$Include)[c(1, 2)] == c("the", "and")))
  )



})

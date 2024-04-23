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
    obj2@data_frame$id_,
    obj@data_frame$id_
  )

  expect_equal(
    obj2@data_frame$decision_,
    obj@data_frame$decision_
  )

  # correct conversion from "include/exclude" to "yes/no"
  expect_equal(
    obj@data_frame$decision_,
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

  # test language support: expect no error
  source_path_de <- test_path("data", "german_data.csv")
  source_path_fr <- test_path("data", "french_data.csv")
  source_path_es <- test_path("data", "spanish_data.csv")
  source_path_pt <- test_path("data", "portuguese_data.csv")
  source_path_ru <- test_path("data", "russian_data.csv")
  source_path_it <- test_path("data", "italian_data.csv")
  source_path_sv <- test_path("data", "swedish_data.csv")

  expect_no_error(
    MetaNLP(source_path_de, bounds = c(1, Inf), language = "german",
            stringsAsFactors=FALSE, fileEncoding = "latin1")
  )

  expect_no_error(
    MetaNLP(source_path_fr, bounds = c(1, Inf), language = "french",
            stringsAsFactors=FALSE, fileEncoding = "latin1")
  )

  expect_no_error(
    MetaNLP(source_path_es, bounds = c(1, Inf), language = "spanish",
            stringsAsFactors=FALSE, fileEncoding = "latin1")
  )
  expect_no_error(
    MetaNLP(source_path_pt, bounds = c(1, Inf), language = "portuguese",
            stringsAsFactors=FALSE, fileEncoding = "latin1")
  )
  expect_no_error(
    MetaNLP(source_path_ru, bounds = c(1, Inf), language = "russian",
            stringsAsFactors=FALSE, fileEncoding = "latin1")
  )
  expect_no_error(
    MetaNLP(source_path_it, bounds = c(1, Inf), language = "italian",
            stringsAsFactors=FALSE, fileEncoding = "latin1")
  )
  expect_no_error(
    MetaNLP(source_path_sv, bounds = c(1, Inf), language = "swedish",
            stringsAsFactors=FALSE, fileEncoding = "latin1")
  )
})

path <- test_path("data", "test_data.csv")
obj <- MetaNLP(path)

test_that("constructor works", {
  obj2 <- MetaNLP(path, bounds = c(1, Inf))
  obj3 <- MetaNLP(path, bounds = c(3,6), word_length = c(4,8))

  # special characters are encoded correctly and do not lead to NA entries
  expect_equal(
    nrow(obj@data_frame),
    4
  )

  # rows containing na values lead to warning
  path_na <- test_path("data", "test_data_na.csv")
  expect_warning(
    obj_na <- MetaNLP(path_na)
  )

  expect_equal(
    nrow(obj_na@data_frame),
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

  # correct conversion to "include/exclude"
  expect_equal(
    obj@data_frame$decision_,
    c("exclude", "exclude", "include", "include")
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

  # data frame can be read directly
  csv <- read.csv2(path, header = TRUE)
  expect_equal(
    MetaNLP(csv), obj
  )

  # data frame can be created when column decision does not exist
  source_path <- test_path("data", "test_data_changed.csv")
  obj_no_dec <- MetaNLP(source_path)

  expect_true(
    is.null(obj_no_dec@data_frame$decision_)
  )
  expect_true(
    !is.null(obj@data_frame$decision_)
  )

  # throw an error if one column is missing
  csv1 <- csv[-1]
  csv2 <- csv[-4]
  csv3 <- csv[-5]

  expect_error(
    MetaNLP(csv1)
  )
  expect_error(
    MetaNLP(csv2)
  )
  expect_error(
    MetaNLP(csv3)
  )

  # test language support: expect no error
  source_path_de <- test_path("data", "german_data.csv")
  source_path_fr <- test_path("data", "french_data.csv")
  source_path_es <- test_path("data", "spanish_data.csv")
  source_path_ru <- test_path("data", "russian_data.csv")

  expect_no_error(
    MetaNLP(source_path_de, bounds = c(1, Inf), language = "german",
            encoding = "UTF-8")
  )

  expect_no_error(
    MetaNLP(source_path_fr, bounds = c(1, Inf), language = "french",
            encoding = "UTF-8")
  )

  expect_no_error(
    MetaNLP(source_path_es, bounds = c(1, Inf), language = "spanish",
            encoding = "UTF-8")
  )

  expect_no_error(
    MetaNLP(source_path_ru, bounds = c(1, Inf), language = "russian",
            encoding = "UTF-8")
  )

})

test_that("print methods work", {
  # test print method
  expect_equal(
    print(obj), "MetaNLP<nrow=4,ncol=54>"
  )
  expect_equal(
    paste0(capture.output(show(obj)), collapse = "\n\r"),
    "MetaNLP<nrow=4,ncol=54>"
  )
})

test_that("plot method works", {
  # normal plot without stratification
  plt <- function() {
    old <- .Random.seed
    set.seed(42)
    on.exit( {.Random.seed <<- old})
    plot(obj)
  }
  vdiffr::expect_doppelganger(
    "wordcloud",
    plt()
  )

  # filter by exclude
  plt_exclude <- function() {
    old <- .Random.seed
    set.seed(42)
    on.exit( {.Random.seed <<- old})
    plot(obj, decision = "exclude")
  }
  vdiffr::expect_doppelganger(
    "wordcloud_exclude",
    plt_exclude()
  )

  # filter by include
  plt_include <- function() {
    old <- .Random.seed
    set.seed(42)
    on.exit( {.Random.seed <<- old})
    plot(obj, decision = "include")
  }
  vdiffr::expect_doppelganger(
    "wordcloud_include",
    plt_include()
  )

  # if no decision column exists, plot should be like "no stratification"
  obj_nodec <- obj
  obj_nodec@data_frame$decision_ <- NULL
  plt_nodec <- function() {
    old <- .Random.seed
    set.seed(42)
    on.exit( {.Random.seed <<- old})
    plot(obj_nodec, decision = "include")
  }
  expect_warning(
    vdiffr::expect_doppelganger(
    "wordcloud",
    plt_nodec()
  ))


})


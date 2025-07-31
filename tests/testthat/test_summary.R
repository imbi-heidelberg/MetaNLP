source_path <- test_path("data", "test_data_large.csv")
obj <- MetaNLP(source_path)

test_that("Summary works", {


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

  # test 6: when tf-idf weighting is used, the summary does not show relative frequencies

  obj2 <- MetaNLP(source_path, bounds = c(0, Inf), weighting = "tf-idf")
  summ3 <- summary(obj2)

  expect_true(
    all(c(rownames(summ3$Total) == "TF-IDF",
          rownames(summ3$Include) == "TF-IDF",
          rownames(summ3$Exclude) == "TF-IDF"))
  )



})


test_that("wordcloud method works", {
  # normal plot without stratification
  plt <- function() {
    old <- .Random.seed
    set.seed(42)
    on.exit( {.Random.seed <<- old})
    wordcloud(obj)
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
    wordcloud(obj, decision = "exclude")
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
    wordcloud(obj, decision = "include")
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
    wordcloud(obj_nodec, decision = "include")
  }
  expect_warning(
    vdiffr::expect_doppelganger(
      "wordcloud",
      plt_nodec()
    ))


  # inclusion of stopwords
  plt_sw_T <- function() {
    old <- .Random.seed
    set.seed(42)
    on.exit( {.Random.seed <<- old})
    wordcloud(obj, stop_words = TRUE)
  }
  vdiffr::expect_doppelganger(
    "wordcloud_sw_T",
    plt_sw_T()
  )

  # exclusion of stopwords
  plt_sw_F <- function() {
    old <- .Random.seed
    set.seed(42)
    on.exit( {.Random.seed <<- old})
    wordcloud(obj, stop_words = FALSE)
  }
  vdiffr::expect_doppelganger(
    "wordcloud_sw_F",
    plt_sw_T()
  )


})

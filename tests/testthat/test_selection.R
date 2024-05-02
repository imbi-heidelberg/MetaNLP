test_that("Feature selection works", {

  source_path <- test_path("data", "test_data_large.csv")
  obj <- MetaNLP(source_path)

  # test 1: select_features returns object of class MetaNLP
  sel_feat <- select_features(obj)

  expect_true(
    isClass(sel_feat, MetaNLP)
  )

  # test 2: select_features reduces number of columns
  expect_lt(
    ncol(sel_feat@data_frame), ncol(obj@data_frame)
  )

  # test 3: higher alpha lead to fewer columns, smaller alpha to more columns
  sel_feat_a_low  <- select_features(obj, alpha = 0.2, lambda = 0.05)
  sel_feat_a_high <- select_features(obj, alpha = 0.7, lambda = 0.05)

  expect_lt(
    ncol(sel_feat_a_high@data_frame), ncol(sel_feat_a_low@data_frame)
  )

  # test 4: higher lambda leads to fewer columns, smaller lambda to more columns
  sel_feat_l_low  <- select_features(obj, alpha = 0.8, lambda = 0.05)
  sel_feat_l_high <- select_features(obj, alpha = 0.8, lambda = 0.1)

  expect_lt(
    ncol(sel_feat_l_high@data_frame), ncol(sel_feat_l_low@data_frame)
  )

  # test 5: setting a seed leads to replicable outcomes
  sel_feat_const <- select_features(obj, lambda = "min", seed = 42)
  temp <- rep(0, 10)

  for(i in 1:10){
    sel_feat_temp <- select_features(obj, lambda = "min", seed = 42)
    data <- sel_feat_temp@data_frame
    temp[i] <- all(colnames(data) == colnames(sel_feat_const@data_frame)) & ncol(data) == 29
  }
  temp <- as.logical(temp)

  expect_true(
    all(temp)
  )

  # test 6: is a seed is set, "lambda = avg" and "lambda = min" return the same results

  sel_feat_const_avg <- select_features(obj, lambda = "avg", seed = 42)
  expect_equal(
    sel_feat_const@data_frame, sel_feat_const_avg@data_frame
  )

  # test 7: the option "1se" leads to fewer columns than "min"
  sel_feat_1se <- select_features(obj, lambda = "1se", seed = 42)

  expect_lt(
    ncol(sel_feat_1se@data_frame), ncol(sel_feat_const@data_frame)
  )
  })

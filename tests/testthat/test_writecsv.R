test_that("write_csv works correctly", {

  source_path  <- test_path("data", "test_data.csv")
  obj <- MetaNLP(source_path)

  # test 1: when no path is specified, file is saved in working directory
  write_csv(obj)

  expect_true(
    file.exists("train_wcm.csv")
  )

  # test 2: default file name is correct
  write_csv(obj, path = test_path("data"))
  source_path2 <- test_path("data", "train_wcm.csv")
  expect_no_error(
    read.csv2(source_path2)
  )

  # test 3: file name can be changed
  write_csv(obj, path = test_path("data"), type = "test")
  source_path3 <- test_path("data", "test_wcm.csv")
  expect_no_error(
    read.csv2(source_path3)
  )

  # test 4: correct data frame is saved
  data_read <- data.frame(read.csv2(source_path3))
  expect_true(
    all(obj@data_frame == data_read)
  )

  # test 5: default file name can be overwritten
  source_path4 <- test_path("data", "written.csv")
  write_csv(obj, source_path4)

  expect_no_error(
    read.csv2(source_path4)
  )

  # remove all the files
  file.remove("train_wcm.csv")
  file.remove(source_path2)
  file.remove(source_path3)
  file.remove(source_path4)
})

test_that("write_csv works correctly", {

  source_path  <- test_path("data", "test_data.csv")
  obj <- MetaNLP(source_path)
  temp_path <- tempdir()

  # test 1: when no path is specified, an error should occur
  expect_error(
    write_csv(obj)
  )

  # test 2: default file name is correct
  write_csv(obj, path = temp_path)
  source_path2 <- file.path(temp_path, "train_wcm.csv")
  expect_true(
    file.exists(source_path2)
  )

  # test 3: file name can be changed
  write_csv(obj, path = temp_path, type = "test")
  source_path3 <- file.path(temp_path, "test_wcm.csv")
  expect_true(
    file.exists(source_path3)
  )

  # test 4: correct data frame is saved
  data_read <- data.frame(read.csv2(source_path3))
  expect_true(
    all(obj@data_frame == data_read)
  )

  # test 5: default file name can be overwritten
  source_path4 <- file.path(temp_path, "written.csv")
  write_csv(obj, source_path4)

  expect_true(
    file.exists(source_path4)
  )

  # remove all the files
  file.remove(source_path2)
  file.remove(source_path3)
  file.remove(source_path4)
})

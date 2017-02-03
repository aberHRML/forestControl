context("forestControl")

test_that("parameter-extraction",{


  rf_no_forest <- randomForest::randomForest(iris[,-5], factor(iris$Species), ntree = 100, keep.forest = FALSE)
  rf_w_forest <- randomForest::randomForest(iris[,-5], factor(iris$Species), ntree = 100, keep.forest = TRUE)

  ranger_no_forest <- ranger::ranger(factor(iris$Species) ~., iris[,-5], write.forest = FALSE)
  ranger_w_forest <- ranger::ranger(factor(iris$Species) ~., iris[,-5], write.forest = TRUE)


  expect_true(is.list(extract_fpr_params(rf_w_forest)))
  expect_true(is.list(extract_fpr_params(ranger_w_forest)))

  expect_error(extract_fpr_params(rf_no_forest))
  expect_error(extract_fpr_params(ranger_no_forest))


})

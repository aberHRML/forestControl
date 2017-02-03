context("forestControl")

test_that("parameter-extraction",{


  rf_no_forest <- randomForest::randomForest(iris[,-5], factor(iris$Species), ntree = 100, keep.forest = FALSE)
  rf_w_forest <- randomForest::randomForest(iris[,-5], factor(iris$Species), ntree = 100, keep.forest = TRUE)

  ranger_no_forest <- ranger::ranger(factor(iris$Species) ~., iris[,-5], write.forest = FALSE,num.trees = 100)
  ranger_w_forest <- ranger::ranger(factor(iris$Species) ~., iris[,-5], write.forest = TRUE, num.trees = 100)


  expect_true(is.list(extract_params(rf_w_forest)))
  expect_true(is.list(extract_params(ranger_w_forest)))

  expect_error(extract_params(rf_no_forest))
  expect_error(extract_params(ranger_no_forest))


})

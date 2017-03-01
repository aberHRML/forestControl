context("selection-freqs")

test_that("selection-freqs",{


  rf_no_forest <- randomForest::randomForest(iris[,-5], factor(iris$Species), ntree = 100, keep.forest = FALSE)
  set.seed(1234)
  rf_w_forest <- randomForest::randomForest(iris[,-5], factor(iris$Species), ntree = 100, keep.forest = TRUE)

  ranger_no_forest <- ranger::ranger(factor(iris$Species) ~., iris[,-5], write.forest = FALSE,num.trees = 100)
  set.seed(1234)
  ranger_w_forest <- ranger::ranger(factor(iris$Species) ~., iris[,-5], write.forest = TRUE, num.trees = 100)


  expect_true(is.data.frame(selecFreqs(rf_w_forest)))
  expect_true(is.data.frame(selecFreqs(ranger_w_forest)))

  expect_error(selecFreqs(rf_no_forest))
  expect_error(selecFreqs(ranger_no_forest))


})

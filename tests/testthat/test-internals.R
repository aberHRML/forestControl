context("internals")

test_that("package-internals",{

  expect_false(forestControl:::is.rf(iris))
  expect_false(forestControl:::is.ranger(iris))

  expect_true(is.numeric(forestControl:::nCm_ratio(1,2,3,4)))
  expect_true(is.numeric(forestControl:::prob_Ckt(1,2,3,4,1)))
  expect_true(is.numeric(forestControl:::prob_Cft(1,2,3)))

  }
)


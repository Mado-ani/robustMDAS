test_that("mcd_estimator works on normal data", {
  set.seed(123)
  X <- matrix(rnorm(500), ncol = 5)
  result <- mcd_estimator(X, alpha = 0.75)
  
  expect_equal(length(result$center), 5)
  expect_equal(dim(result$cov), c(5, 5))
  expect_equal(result$breakdown, 0.25)
})

test_that("mcd_estimator handles invalid alpha", {
  X <- matrix(rnorm(100), ncol = 2)
  expect_error(mcd_estimator(X, alpha = 0.4))
  expect_error(mcd_estimator(X, alpha = 1.2))
})

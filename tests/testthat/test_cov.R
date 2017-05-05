context("cov")

mat <- unclass(cov(diag(1, 2)))
attributes(mat) <- attributes(mat)["dim" == names(attributes(mat))]

test_that("cov function produces covariance matrix", {
  expect_equal(mat,
               matrix(c(0.5, -0.5, -0.5, 0.5), nrow = 2))
})



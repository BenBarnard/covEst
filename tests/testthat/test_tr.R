context("tr")

test_that("cov function produces covariance matrix", {
  expect_equal(covEst:::tr(diag(1, 2)), 2)
})



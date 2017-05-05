context("cov")

mat <- unclass(cov(diag(1, 2)))
attributes(mat) <- attributes(mat)["dim" == names(attributes(mat))]

test_that("cov function produces covariance matrix", {
  expect_equal(mat,
               matrix(c(0.5, -0.5, -0.5, 0.5), nrow = 2))
})

mat <- unclass(cov(as.data.frame(diag(1, 2))))
attributes(mat) <- attributes(mat)["dim" == names(attributes(mat))]

test_that("cov function produces covariance matrix", {
  expect_equal(mat,
               matrix(c(0.5, -0.5, -0.5, 0.5), nrow = 2))
})

df <- as.data.frame(cbind(rbind(diag(1, 2),
                                diag(1, 2)),
                          c(1, 1, 2, 2)))
mat <- lapply(cov(df, group = V3), function(x){
  mat <- unclass(x)
  attributes(mat) <- attributes(mat)["dim" == names(attributes(mat))]
  mat
})[[1]]

test_that("cov function produces covariance matrix", {
  expect_equal(mat,
               matrix(c(0.5, -0.5, -0.5, 0.5), nrow = 2))
})

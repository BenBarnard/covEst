context("degreesFreedom")

test_that("degreesFreedom function produces Degrees of Freedom", {
  expect_equal(degreesFreedom(cov(iris[,1:4])), 149)
})


test_that("multiplication works", {
  expect_equal(2 * 2, 4)
})
test_that("linreg returns an S3 object with correct class", {
  fit <- linreg(mpg ~ wt + hp, data = mtcars)
  expect_s3_class(fit, "linreg")
})

test_that("coef() matches lm()", {
  lm0 <- lm(mpg ~ wt + hp, data = mtcars)
  fit <- linreg(mpg ~ wt + hp, data = mtcars)
  expect_equal(as.numeric(coef(fit)), as.numeric(coef(lm0)), tolerance = 1e-7)
})

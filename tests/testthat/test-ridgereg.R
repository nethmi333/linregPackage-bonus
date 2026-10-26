library(testthat)
library(datasets)

test_that("ridgereg object structure and class work correctly", {
  data(iris)
  mod <- ridgereg(Petal.Length ~ Sepal.Length + Sepal.Width, data = iris, lambda = 1)

  expect_s3_class(mod, "ridgereg")

  expect_true("coefficients" %in% names(mod))
  expect_true("fitted_values" %in% names(mod))
  expect_true("residuals" %in% names(mod))
})

test_that("S3 methods (coef, print, predict) work without errors", {
  data(iris)
  mod <- ridgereg(Petal.Length ~ Sepal.Length + Sepal.Width, data = iris, lambda = 5)

  expect_type(coef(mod), "double")
  expect_equal(length(coef(mod)), 3) # Intercept + 2 predictors

  expect_output(print(mod))

  preds_train <- predict(mod)
  expect_equal(length(preds_train), nrow(iris))

  new_df <- data.frame(Sepal.Length = c(5.0, 6.0), Sepal.Width = c(3.0, 3.2))
  preds_new <- predict(mod, newdata = new_df)
  expect_equal(length(preds_new), 2)
})

test_that("ridgereg with lambda = 0 gives equivalent predictions to standard OLS", {
  data(iris)

  fit_lm <- lm(Petal.Length ~ Sepal.Length + Sepal.Width, data = iris)
  fit_ridge <- ridgereg(Petal.Length ~ Sepal.Length + Sepal.Width, data = iris, lambda = 0)

  expect_equal(predict(fit_ridge), unname(predict(fit_lm)), tolerance = 1e-4)
})

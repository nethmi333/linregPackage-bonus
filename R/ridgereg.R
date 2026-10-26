#' Ridge Regression Model
#'
#' Fits a ridge regression model using linear algebra with L2 regularization.
#'
#' @param formula A formula object (e.g., \code{Petal.Length ~ Species}).
#' @param data A data frame containing the variables.
#' @param lambda A numeric value for the ridge penalty.
#'
#' @return An object of class \code{ridgereg}.
#' @importFrom stats model.frame model.matrix
#' @export
#' @examples
#' mod <- ridgereg(Petal.Length ~ Species, data = iris, lambda = 1)
#' print(mod)
ridgereg <- function(formula, data, lambda) {
  mf <- model.frame(formula, data)
  X <- model.matrix(formula, mf)
  Y <- data[[all.vars(formula)[1]]]

  for (i in seq_len(ncol(X))){
    col_name <- colnames(X)[i]

    if (col_name != "(Intercept)") {
      x_vals <- X[, i]
      x_bar <- mean(x_vals)
      v_x <- var(x_vals)

      if (v_x > 0) {
        X[, i] <- (x_vals - x_bar) / sqrt(v_x)
      }
    }
  }

  p <- ncol(X)
  I_matrix <- diag(p)

  if ("(Intercept)" %in% colnames(X)) {
    I_matrix[1, 1] <- 0
  }

#Ridge regression using the QR decomposition
  X_augmented <- rbind(X, sqrt(lambda) * I_matrix)
  Y_augmented <- c(Y, rep(0, p))

  qr_decomp <- qr(X_augmented)
  Q <- qr.Q(qr_decomp)
  R <- qr.R(qr_decomp)

  beta <- backsolve(R, t(Q) %*% Y_augmented)

  rownames(beta) <- colnames(X)
########################################################

  XtX <- t(X) %*% X
  Xty <- t(X) %*% Y

  beta <- solve(XtX + (lambda * I_matrix)) %*% Xty
########################################################


  fitted_values <- X %*% beta
  residuals <- Y - fitted_values

  out <- list(
    formula = formula,
    data_name = deparse(substitute(data)),
    lambda = lambda,
    coefficients = beta,
    fitted_values = fitted_values,
    residuals = residuals
  )

  class(out) <- "ridgereg"

  return(out)
}

################################################################################################################

#' Methods for ridgereg objects
#'
#' Prints the coefficients and coefficient names of a fitted \code{ridgereg} model,
#'
#' @param x An object of class \code{ridgereg}.
#' @param ... Further arguments.
#'
#' @return Called for its side effect of printing to the console.
#' @export
print.ridgereg <- function(x, ...) {
  cat("Call:\n")
  cat("ridgereg(formula = ", deparse(x$formula), ", data = ", x$data_name, ", lambda = ", x$lambda, ")\n\n", sep = "")

  cat("Coefficients:\n")

  print(x$coefficients)
}

#' @export
coef.ridgereg <- function(object, ...) {
  return(object$coefficients)
}

#' @export
predict.ridgereg <- function(object, newdata = NULL, ...) {
  if (is.null(newdata)) {
    return(as.vector(object$fitted_values))
  } else {

    tt <- terms(object$formula)
    tt <- delete.response(tt)

    mf <- model.frame(tt, newdata)
    X_new <- model.matrix(tt, mf)

    return(as.vector(X_new %*% object$coefficients))
  }
}


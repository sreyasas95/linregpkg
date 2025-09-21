#' Fit a linear regression model (OLS)
#'
#' Computes Ordinary Least Squares (OLS) estimates for a linear model
#' specified by a formula and a data frame.
#'
#' @param formula A model formula, e.g. \code{y ~ x1 + x2}.
#' @param data A \code{data.frame} containing the variables in the model.
#'
#' @return An object of class \code{"linreg"} with elements:
#' \itemize{
#'   \item \code{coefficients} Named numeric vector of estimates.
#'   \item \code{fitted} Fitted values.
#'   \item \code{residuals} Residuals (\eqn{y - \hat{y}}).
#'   \item \code{df} Residual degrees of freedom (\eqn{n - p}).
#'   \item \code{sigma2} Residual variance estimate.
#'   \item \code{var_beta} Variance–covariance matrix of coefficients.
#'   \item \code{call} Matched call.
#'   \item \code{X}, \code{y}, \code{formula}, \code{data} (convenience fields).
#' }
#'
#' @examples
#' mod <- linreg(mpg ~ wt + hp, data = mtcars)
#' pred(mod)           # in-sample predictions
#' resid(mod)[1:3]
#' coef(mod)
#' print(mod)
#'
#' @importFrom stats model.frame model.matrix model.response setNames terms
#' @export
linreg <- function(formula, data) {
  # Build model matrix X and response y
  mf <- stats::model.frame(formula, data)
  X  <- stats::model.matrix(stats::terms(mf), mf)
  y  <- stats::model.response(mf)

  # Coefficients: beta_hat = (X'X)^{-1} X'y
  XtX <- t(X) %*% X
  Xty <- t(X) %*% y
  beta_hat <- solve(XtX, Xty)

  # Fitted values and residuals
  y_hat     <- as.vector(X %*% beta_hat)
  residuals <- as.vector(y - y_hat)

  # Degrees of freedom
  n  <- nrow(X)
  p  <- ncol(X)
  df <- n - p

  # Residual variance sigma^2
  sigma2 <- as.numeric(t(residuals) %*% residuals / df)

  # Variance–covariance of beta_hat
  var_beta <- sigma2 * solve(XtX)

  # Return object
  out <- list(
    coefficients = stats::setNames(as.numeric(beta_hat), colnames(X)),
    fitted       = y_hat,
    residuals    = residuals,
    df           = df,
    sigma2       = sigma2,
    var_beta     = var_beta,
    call         = match.call(),
    formula      = formula,
    data         = data,
    X            = X,
    y            = as.vector(y)
  )
  class(out) <- "linreg"
  out
}

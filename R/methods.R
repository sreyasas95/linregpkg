#' @keywords internal
#' @importFrom stats resid
#' @importFrom utils globalVariables
NULL

# Silence NSE NOTES for ggplot2 aesthetics
utils::globalVariables(c("fitted", "resid", "std_resid"))

#' Print method for linreg objects
#' @param x An object of class linreg
#' @param ... Additional arguments
#' @export
print.linreg <- function(x, ...) {
  cat("linreg(formula =", deparse(x$formula), ")\n\n")
  cat("Coefficients:\n")
  print(x$coefficients)
  invisible(x)
}

#' Plot method for linreg objects
#' Produces Residuals vs Fitted and Scale-Location plots.
#' @param x An object of class linreg
#' @param ... Additional arguments
#' @export
plot.linreg <- function(x, ...) {
  df <- data.frame(
    fitted    = x$fitted,
    resid     = x$residuals,
    std_resid = x$residuals / sqrt(x$sigma2)
  )

  p1 <- ggplot2::ggplot(df, ggplot2::aes(x = fitted, y = resid)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::labs(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals")

  p2 <- ggplot2::ggplot(df, ggplot2::aes(x = fitted, y = std_resid)) +
    ggplot2::geom_point() +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::labs(title = "Scale-Location", x = "Fitted values", y = "Standardized residuals")

  print(p1)
  print(p2)
}

#' Residuals method for linreg objects
#' @param object A linreg object
#' @param ... Additional arguments
#' @export
#' @method resid linreg
resid.linreg <- function(object, ...) {
  object$residuals
}

# --------------------------
# Prediction generic + method
# --------------------------

#' Prediction generic
#' @param object A model object
#' @param ... Additional arguments
#' @export
pred <- function(object, ...) {
  UseMethod("pred")
}

#' Prediction method for linreg objects
#' @param object A linreg object
#' @param ... Additional arguments
#' @export
#' @method pred linreg
pred.linreg <- function(object, ...) {
  object$fitted
}

#' Coefficients method for linreg objects
#' @param object A linreg object
#' @param ... Additional arguments
#' @export
coef.linreg <- function(object, ...) {
  object$coefficients
}

#' Summary method for linreg objects
#' @param object A linreg object
#' @param ... Additional arguments
#' @export
summary.linreg <- function(object, ...) {
  se   <- sqrt(diag(object$var_beta))
  tval <- object$coefficients / se
  pval <- 2 * (1 - stats::pt(abs(tval), df = object$df))

  results <- data.frame(
    Estimate     = object$coefficients,
    `Std. Error` = se,
    `t value`    = tval,
    `Pr(>|t|)`   = pval
  )

  cat("linreg(formula =", deparse(object$formula), ")\n\n")
  stats::printCoefmat(results)
  cat("\nResidual standard error:", sqrt(object$sigma2),
      "on", object$df, "degrees of freedom\n")

  invisible(results)
}

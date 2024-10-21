#' Confidence Interval
#' return the 95% CI of odds ratio in a string
#' @param coef Coefficient from logistic regression
#' @param se Standard error from logistic regression
#' @param siglevel Designated significance level
#' @param roundto Decimal places to round to
#'
#' @return ORresult
#' @export
#'
#' @examples
#' OR_95CI(0,1,0.95,2)
#' 1.00 (0.94, 1.06)
OR_95CI <- function(coef, se, siglevel, roundto){
  q <- 1 - siglevel / 2
  OR <- exp(coef)
  ORlcl <- exp(coef - qnorm(q) * se)
  ORucl <- exp(coef + qnorm(q) * se)
  ORresult <- paste0(format(round(OR, roundto), nsmall = roundto),
                     " (",
                     format(round(ORlcl, roundto), nsmall = roundto),
                     ", ",
                     format(round(ORucl, roundto), nsmall = roundto),
                     ")"
  )
  return(ORresult)
}



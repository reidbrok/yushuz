#' @title Create Odds Ratio and Confidence Interval
#' @description A function that returns the odds ratio and confidence interval
#' @param coef coefficient of one variable
#' @param se standard deviation of one variable
#' @param siglevel significant level for confidence interval, range from 0 to 1
#' @param roundto a numeric value indicate the obtained decimal length
#' @return ORresult
#' @author Yushu Zou
#' @examples
#' OR_95CI(0,1,0.95,2)
#' @export
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

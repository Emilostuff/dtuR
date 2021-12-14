#' Variance From a Percentage Distribution
#' @description computes the variance given only the relative distribution between a set of values
#'
#' @param values a vector of values
#' @param distribution a vector representing the relative distribution of the values
#'
#' @return
#' @export
#'
var_percent <- function(values, distribution) {
  if (sum(distribution) != 1) {
    stop("Distribution does not add up to 1")
  } else {
    sum(sapply(1:length(values),function(x) (values[x]-mean(values))^2*distribution[x]))
  }
}

#' Variance Confidence Interval
#'
#' @param x a data vector
#' @param c confidence (between 0..1)
#'
#' @return
#' @export
#'
var_ci <- function(x, c = 0.95) {
    var_ci_nv(sd(x), length(x), c = c)
}

#' Variance Confidence Interval - No Vector
#'
#' @param s standard deviation
#' @param n number of observations
#' @param c confidence (between 0..1)
#'
#' @return
#' @export
#'
var_ci_nv <- function(s, n, c = 0.95) {
  c((n-1)*s^2/qchisq((1+c)/2, df=(n-1)), (n-1)*s^2/qchisq(1-(1+c)/2, df=(n-1)))
}

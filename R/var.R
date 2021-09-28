#' Variance From a Percentage Distribution
#' @description computes the variance given only the relative distribution between a set of values
#'
#' @param values a vector of values
#' @param distribution a vector representing the relative distribution of the values
#'
#' @return
#' @export
#'
#' @examples
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
#' @examples
var_ci <- function(x, c = 0.95) {
  c((length(x)-1)*var(x)/qchisq((1+c)/2, df=(length(x)-1)), (length(x)-1)*var(x)/qchisq(1-(1+c)/2, df=(length(x)-1)))
}


#' Mean Confidence Interval
#'
#' @param x a data vector
#' @param c confidence (between 0..1)
#'
#' @return
#' @export
#'
mean_ci <- function(x, c = 0.95) {
  mean_ci_nv(mean(x), sd(x), length(x), c)
}


#' Mean Confidence Interval - No Vector
#'
#' @param mu mean
#' @param s standard deviation
#' @param n number of observations
#' @param c confidence (between 0..1)
#'
#' @return
#' @export
#'
mean_ci_nv <- function(mu, s, n, c = 0.95) {
  mu + c(-1, 1) * qt((1+c)/2, df = n - 1) * s / sqrt(n)
}


#' Mean Confidence Interval - Converter significance level
#'
#' @param ci confidence interval vector
#' @param df degrees of freedom
#' @param from initial significance level
#' @param to desired significance level
#'
#' @return
#' @export
#'
mean_ci_convert <- function(ci, df, from = 0.95, to) {
  mu <- mean(ci)
  dev <- abs(mu-ci[1])
  mu + c(-1, 1) * dev / qt(mean(c(1, from)), df = df) * qt(mean(c(1, to)), df= df)
}





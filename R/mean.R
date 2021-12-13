
#' Mean Confidence Interval
#'
#' @param x a data vector
#' @param c confidence (between 0..1)
#'
#' @return
#' @export
#'
#' @examples
mean_ci <- function(x, c = 0.95) {
  mean_ci_nv(mean(x), sd(x), length(x), c)
}


#' Mean Confidence Interval - No Vector
#'
#' @param mu mean
#' @param s standard deviation
#' @param n degrees of freedom
#' @param c confidence (between 0..1)
#'
#' @return
#' @export
#'
#' @examples mean_ci_nv(mu = -0.1181818, s = 0.05884899, n = 22)
mean_ci_nv <- function(mu, s, n, c = 0.95) {
  mu + c(-1, 1) * qt((1+c)/2, df = n - 1) * s / sqrt(n)
}





#' Standard Deviation Scaling
#'
#' @param sd a given standard deviation
#' @param k scaling constant
#'
#' @return
#' @export
#'
sd_scale <- function(sd, k) {
  sqrt((sd**2)*k)
}

#' Standard Deviation Confidence Interval
#'
#' @param x a data vector
#' @param c confidence (between 0..1)
#'
#' @return
#' @export
#'
sd_ci <- function(x, c = 0.95) {
  sqrt(var_ci(x, c))
}

#' Standard Deviation Confidence Interval - No Vector
#'
#' @param s standard deviation
#' @param n number of observations
#' @param c confidence (between 0..1)
#'
#' @return
#' @export
#'
sd_ci_nv <- function(s, n, c = 0.95) {
  sqrt(var_ci_nv(s, n, c = 0.95))
}

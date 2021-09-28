#' Standard Deviation Scaling
#'
#' @param sd a given standard deviation
#' @param k scaling constant
#'
#' @return
#' @export
#'
#' @examples
sd_scale <- function(sd, k) {sqrt((sd**2)*k)}

#' Standard Deviation Confidence Interval
#'
#' @param x a data vector
#' @param c confidence (between 0..1)
#'
#' @return
#' @export
#'
#' @examples
sd_ci <- function(x, c = 0.95) {sqrt(var_ci(x, c))}

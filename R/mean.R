#' Mean Confidence Interval Deviation
#'
#' @param x a data vector
#' @param c confidence (between 0..1)
#'
#' @return
#' @export
#'
#' @examples
mean_ci_dev <- function(x, c = 0.95) {
  qt((1+c)/2, df=length(x)-1)*std_error(x)
}


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
  c(mean(x)-mean_ci_dev(x, c), mean(x)+mean_ci_dev(x, c))
}

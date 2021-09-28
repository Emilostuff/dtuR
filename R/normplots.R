#' QQ Plotter
#' @description Makes a QQ-plot that can easily be adjusted to look nice
#'
#' @param x a data vector
#' @param padding amount of extra vertical range (default = 0.2)
#'
#' @return
#' @export
#'
#' @examples
qq_plot <- function(x, padding=0.2) {
  range <- max(x)-min(x)
  qqnorm(x, ylim=c(min(x)-padding*range,max(x)+padding*range))
  qqline(x)
}

#' ECDF Plotter
#' @description Plot the empirical cumulative distribution vs. the best fitting normal distribution
#'
#' @param x a data vector
#'
#' @return
#' @export
#'
#' @examples
ecdf_plot <- function(x) {
  plot(ecdf(x), verticals=TRUE)
  xseq <- seq(0.9*min(x), 1.1*max(x), length.out=10000)
  lines(xseq, pnorm(xseq, mean(x), sd(x)))
}



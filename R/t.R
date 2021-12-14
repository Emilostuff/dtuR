#' Critical t-value
#'
#' @param c confidence
#' @param df degrees of freedom
#'
#' @return
#' @export
#'
t_critical <- function(c, df) {
  qt((1+c)/2, df=df)
}




#' Two Sample t-value (Welch Method)
#'
#' @param x1 sample mean group 1
#' @param x2 sample mean group 2
#' @param s1 sample sd group 1
#' @param s2 sample sd group 2
#' @param n1 number of samples in group 1
#' @param n2 number of samples in group 2
#' @param delta0 offset
#'
#' @return
#' @export
#'
t_obs_2s_welch <- function(x1, x2, s1, s2, n1, n2, delta0 = 0) {
  ((x1 - x2) - delta0) / sqrt(s1^2/n1 + s2^2/n2)
}



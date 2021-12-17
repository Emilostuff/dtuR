
#' Bonferroni Corrected Alpha
#'
#' @param alpha significance level
#' @param groups number of groups to be post-hos tested
#'
#' @return
#' @export
#'
alpha_bonf <- function(alpha, groups) {
  alpha / choose(groups, 2)
}


#' Least Significant Difference
#'
#' @param alpha bonferroni corrected alpha value
#' @param df degrees of freedom in ANOVA
#' @param MSE MSE from ANOVA
#' @param m number of observations in each treatment group
#'
#' @return
#' @export
#'
LSD <- function(alpha, df, MSE, m) {
  qt(mean(c(1, 1-alpha)), df = df)*sqrt(2*MSE/m)
}

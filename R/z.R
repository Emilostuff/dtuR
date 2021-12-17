#' Z-obs for 1 proportion (Method 7.11)
#'
#' @param x proportion count
#' @param n total count
#' @param p0 proportion hypothesis
#'
#' @return
#' @export
#'
z_obs_1 <- function(x, n, p0) {
  (x - n*p0)/sqrt(n*p0*(1-p0))
}

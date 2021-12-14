#' Standard Error
#'
#' @description Computes the standard error
#' @param x a data vector
#'
#' @return
#' @export
#'
std_error <- function(x) {
  sd(x)/sqrt(length(x))
}

#' IQR as done in the book
#'
#' @param x a data vector
#'
#' @return
#' @export
#'
IQR <-  function(x) {
  as.numeric(quantile(x, type=2)[4] - quantile(x, type=2)[2])
}

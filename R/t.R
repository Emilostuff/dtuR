#' Critical t-value
#'
#' @param c confidence
#' @param df degrees of freedom
#'
#' @return
#' @export
#'
#' @examples
t_critical <- function(c, df) {
  qt((1+c)/2, df=df)
}


#' Standard Error
#'
#' @description Computes the standard error
#' @param x a data vector
#'
#' @return
#' @export
#'
#' @examples
std.error <- function(x) {
  sd(x)/sqrt(length(x))
}

#' Mean Confidence Interval Deviation
#'
#' @param x a data vector
#' @param c confidence (between 0..1)
#'
#' @return
#' @export
#'
#' @examples
ci.mean.dev <- function(x, c = 0.95) {
  qt((1+c)/2, df=length(x)-1)*std.error(x)
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
ci.mean <- function(x, c = 0.95) {
  c(mean(x)-ci.mean.dev(x, c), mean(x)+ci.mean.dev(x, c))
}



#' Variance Confidence Interval
#'
#' @param x a data vector
#' @param c confidence (between 0..1)
#'
#' @return
#' @export
#'
#' @examples
ci.var <- function(x, c = 0.95) {
  c((length(x)-1)*var(x)/qchisq((1+c)/2, df=(length(x)-1)), (length(x)-1)*var(x)/qchisq(1-(1+c)/2, df=(length(x)-1))) # 6.27333 50.46495
}

ci.var(c(1,2,3,4,5))

#' Standard Deviation Confidence Interval
#'
#' @param x a data vector
#' @param c confidence (between 0..1)
#'
#' @return
#' @export
#'
#' @examples
ci.sd <- function(x, c = 0.95) {sqrt(ci.var(x, c))}

#' Standard Deviation Scaling
#'
#' @param sd a given standard deviation
#' @param k scaling constant
#'
#' @return
#' @export
#'
#' @examples
sd.scale <- function(sd, k) {sqrt((sd**2)*k)}

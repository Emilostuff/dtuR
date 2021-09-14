#' @title Interval Solve
#' @description Find the x that makes \code{f(x) = target}
#'
#' @param f a function of 1 variable
#' @param target the desired output value
#' @param from start of search interval (default 0)
#' @param to end of search interval (default 10000)
#' @param steps number of discrete steps in search interval (default 10)
#' @param runs number of recursive calls (default 20)
#'
#' @return
#' @export
#'
#' @examples
#' isolve(sqrt, 17.42)
isolve <- function(f, target, from = 0, to = 10000, steps = 10, runs = 20) {
  if (target-f(from) == 0 || runs < 1) {
    from
  } else {
    answer <- NA
    x <- seq(from, to, length.out=steps)
    values <- sapply(x, f)
    for(i in 1:(length(x)-1)) {
      if (sign(target-values[i]) != sign(target-values[i+1])) {
        answer <- isolve(f, target, x[i], x[i+1], steps, runs-1)
        break
      }
    }
    answer
  }
}

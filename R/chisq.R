#' Explicit Chi^2 test-value terms
#'
#' @param observed a matrix with observed values
#'
#' @description Each cell holds the value of its contribution to the test statistic. Thus the sum of the test statistic
#'
#' @return
#' @export
#'
chisq_explicit <- function(observed) {
  expected <- chisq.test(observed, correct = FALSE)$expected
  result <- observed

  for (i in 1:length(observed[,1])) {
    for (j in 1:length(observed[1,])) {
      result[i,j] <- (observed[i,j]-expected[i,j])^2/expected[i,j]
    }
  }
  result
}

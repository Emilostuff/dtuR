#' Data From Count
#' @description Creates a data vector with each label appearing the number of times specified in the count vector
#'
#' @param labels data labels (can be anytning)
#' @param count occurrences of each label
#'
#' @return
#' @export
#'
#' @examples
data_from_count <- function(labels, count) {
  if (length(labels) != length(count)) {
    NA
  } else {
    as.vector(unlist(sapply(1:length(labels),function(x) rep(labels[x], count[x]))))
  }
}


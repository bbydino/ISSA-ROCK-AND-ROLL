#' Get the Statistical Mode
#'
#' Given a vector, return the mode (measure of central tendency). MATH 4753 Ass 1
#'
#' @param v a vector to calculate the statistical mode of.
#'
#' @return the mode of the give vector
#' @examples
#' getmode(cars$speed)
#' getmode(c(1, 2, 2, 3, 3, 3, 4, 4, 4, 4))
#'
#' @export
getmode <- function(v) {
  uniqv <- unique(v)
  return (uniqv[which.max(tabulate(match(v, uniqv)))])
}

#' Simulate a binomial experiment
#'
#' Simulates a binomial experiment with given
#' iteration, sample size, and probability of success.
#' Outputs a barplot and returns a table of the results.
#'
#' From: MATH 4753 Lab 5
#'
#' @param iter number of iterations (default iter=100)
#' @param n sample size (default n=10)
#' @param p probability of success (default p=0.5)
#'
#' @importFrom grDevices rainbow
#' @importFrom graphics barplot
#'
#' @return table of relative frequencies
#'
#' @examples
#' sim100 = mybin(iter = 100, n = 10, p = 0.7)
#' sim1000 = mybin(iter = 1000, n = 10, p = 0.7)
#'
#' @export
mybin <- function(iter = 100,
                 n = 10,
                 p = 0.5) {
  # matrix to hold the samples, init with NA's
  sam.mat = matrix(NA,
                   nrow = n,
                   ncol = iter,
                   byrow = TRUE)
  # vector to hold the # of successes in each trial
  succ = c()
  for (i in 1:iter) {
    # fill each column with a new sample
    sam.mat[, i] = sample(c(1, 0),
                          n,
                          replace = TRUE,
                          prob = c(p, 1 - p))

    # calculate a statistic from sample (this case is sum)
    succ[i] = sum(sam.mat[, i])
  }

  # make a table of successes
  succ.tab = table(factor(succ, levels = 0:n))

  # make a barplot of the proportions
  barplot(
    succ.tab / iter,
    col = rainbow(n + 1),
    ylim = c(0, 1.1 * max(succ.tab / iter)),
    main = paste("Binomial simulation: n=",
                 n,
                 ", p=",
                 p,
                 ", iter=",
                 iter,
                 sep = ""),
    xlab = "Number of successes"
  )

  return (succ.tab / iter)
}

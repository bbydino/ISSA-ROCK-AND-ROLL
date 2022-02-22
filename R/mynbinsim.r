#' Simulate a negative binomial experiment
#'
#' Simulates a negative binomial experiment with given
#' iteration, and probability of success for each trial.
#' Outputs a barplot and returns a table of the results.
#'
#' From: MATH 4753 Lab 5
#'
#' @param iter number of iterations (default iter=100)
#' @param p probability of success for each trial (default p=0.5)
#'
#' @importFrom grDevices rainbow
#' @importFrom graphics barplot
#' @return table of relative frequencies of probabilities
#'
#' @examples
#' sim100 = mynbinsim(100, 0.5)
#' sim1000 = mynbinsim(1000, 0.5)
#'
#' @export
mynbinsim = function(iter = 100, p = 0.5) {
  # matrix to hold the samples, init with NA's
  sam.mat = matrix(NA,
                   ncol = iter,
                   byrow = TRUE)

  # vector to hold the # of trials
  trials = c()

  for (i in 1:iter) {
    # number of trials before success
    # aka number failures til success
    Y = 0

    # repeat until we have a success
    while (sample(c(1, 0), 1, prob = c(p, 1 - p)) != 1) {
      Y = Y + 1
    }

    sam.mat[, i] = Y
    trials[i] = sum(sam.mat[, i])
  }

  # table of number of failures til success
  trials.tab = table(factor(trials,
                            levels = 0:max(trials)))

  # make a barplot of the proportions
  barplot(
    trials.tab / iter,
    col = rainbow(length(trials.tab) + 1),
    ylim = c(0, 1.1 * max(trials.tab / iter)),
    main = paste("Negative Binomial simulation: iter=",
                 iter,
                 ", p=",
                 p,
                 sep = ""),
    xlab = "Random Variable"
  )

  return (trials.tab / iter)
}

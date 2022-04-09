#' Maximum Likelihood Function for Univariate Likelihoods
#'
#' For repeated sampling from same distribution.
#'
#' From: MATH 4753 Lab 10
#'
#' @importFrom grDevices rainbow
#' @importFrom graphics barplot
#'
#' @param lfun likelihood function
#' @param x input vector
#' @param param parameters to the lfun function
#' @param ... additional plot parameters
#'
#' @return a list containing:
#' the index of the max likelihood,
#' the parameter at the index of the max likelihood,
#' the max likelihood,
#' the slope
#'
#' @importFrom graphics axis points
#'
#' @example
#' logbin = function(x, param)
#' log(dbinom(x,prob=param,size=20))
#' mymaxlik(x=c(3,3,4,3,4,5,5,4),param=seq(0,1,length=1000),lfun=logbin,xlab=expression(pi),main="Binomial",cex.main=2)
#'
#' @export
mymaxlik = function(lfun, x, param, ...) {
  # how many param values are there?
  np = length(param)
  # outer -- notice the order, x then param
  # this produces a matrix -- try outer(1:4,5:10,function(x,y) paste(x,y,sep=" "))   to understand
  z = outer(x, param, lfun) # A
  # z is a matrix where each x,param is replaced with the function evaluated at those values
  y = apply(z, 2, sum)

  # y is a vector made up of the column sums
  # Each y is the log lik for a new parameter value
  plot(param,
       y,
       col = "Blue",
       type = "l",
       lwd = 2,
       ...)
  # which gives the index for the value of y == max.
  # there could be a max between two values of the parameter, therefore 2 indices
  # the first max will take the larger indice
  i = max(which(y == max(y))) # B
  abline(v = param[i], lwd = 2, col = "Red")

  # plots a nice point where the max lik is
  points(param[i],
         y[i],
         pch = 19,
         cex = 1.5,
         col = "Black")
  axis(3, param[i], round(param[i], 2))
  #check slopes. If it is a max the slope shoud change sign from + to
  # We should get three + and two -vs
  ifelse(i - 3 >= 1 & i + 2 <= np,
         slope <-(y[(i - 2):(i + 2)] - y[(i - 3):(i + 1)]) / (param[(i - 2):(i + 2)] - param[(i - 3):(i + 1)]),
         slope <- "NA")

  return(list(
    i = i,
    parami = param[i],
    yi = y[i],
    slope = slope
  ))
}

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


#' Maximum Likelihood using Newton-Raphson Algorithm
#'
#' Maximum Likelihood using Newton-Raphson Algorithm on the derivative
#'
#' From: MATH 4753 Lab 10
#'
#' @importFrom grDevices rainbow
#' @importFrom graphics barplot
#'
#' @param x0 initial x-value for algorithm
#' @param delta delta for algorithm, default 0.001
#' @param llik likelihood function
#' @param xrange x range for algorithm
#' @param parameter parameter of interest to the lfun function
#'
#' @return a list containing:
#' x values from algorithm
#' y values from algorithm
#'
#' @importFrom graphics axis points
#'
#' @example
#' lfun = function(x) log(dpois(4,x)*dpois(6,x)*dpois(7,x)*dpois(6,x)*dpois(5,x))
#' myNRML(x0=1,delta=0.000001,llik=lfun,xrange=c(0,20),parameter="lambda")
#' @export
myNRML = function(x0,
                  delta = 0.001,
                  llik,
                  xrange,
                  parameter = "param") {
  f = function(x)
    (llik(x + delta) - llik(x)) / delta
  fdash = function(x)
    (f(x + delta) - f(x)) / delta
  d = 1000
  i = 0
  x = c()
  y = c()
  x[1] = x0
  y[1] = f(x[1])
  while (d > delta & i < 100) {
    i = i + 1
    x[i + 1] = x[i] - f(x[i]) / fdash(x[i])
    y[i + 1] = f(x[i + 1])
    d = abs(y[i + 1])
  }
  layout(matrix(
    1:2,
    nrow = 1,
    ncol = 2,
    byrow = TRUE
  ), widths = c(1, 2))
  curve(
    llik(x),
    xlim = xrange,
    xlab = parameter,
    ylab = "log Lik",
    main = "Log Lik"
  )
  curve(
    f(x),
    xlim = xrange,
    xaxt = "n",
    xlab = parameter,
    ylab = "derivative",
    main =  "Newton-Raphson Algorithm \n on the derivative"
  )
  points(x,
         y,
         col = "Red",
         pch = 19,
         cex = 1.5)
  axis(1, x, round(x, 2), las = 2)
  abline(h = 0, col = "Red")

  segments(x[1:(i - 1)], y[1:(i - 1)], x[2:i], rep(0, i - 1), col = "Blue", lwd =2)
  segments(x[2:i], rep(0, i - 1), x[2:i], y[2:i], lwd = 0.5, col = "Green")

  return(list(x = x, y = y))
}

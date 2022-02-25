#' Plot normal curve and probability region P(X <= a)
#'
#' Display the curve,
#' shaded area between the curve and x axis from −∞ to x=a,
#' and calculate the area (probability, P(X ≤ a)),
#' which is released to the command-line in a list.
#' From MATH 4753 Lab 6.
#'
#' @param mu the mean of the normal distribution
#' @param sigma the standard deviation of the normal distribution
#' @param a for calculating the area (probability P(X <= a))
#'
#' @return a list containing probability P(X <= a)
#'
#' @examples
#' myncurve(mu = 10, sigma = 5, a = 6)
#'
#' @export
myncurve = function(mu, sigma, a) {
  prob = normregioncurve(
    mean = mu,
    sd = sigma,
    from = NULL,
    to = a
  )
  return (list(prob))
}


#' Plot normal curve and probability region
#'
#' Plots a normal distribution curve given
#' mean, stdev, probability region start, probability region end.
#' From MATH 4753 Lab 6.
#'
#' @param mean mean of the normal distribution
#' @param sd standard deviation of the normal distribution
#' @param from start of probability region. set to NULL if -inf
#' @param to end of probability region. set to NULL if inf
#' @param col color of the shaded region. default magenta.
#' @param lower.tail if true, get P(X<a). if false, get P(X>a). default to TRUE
#'
#' @return probability of region
#'
#' @importFrom stats dnorm pnorm
#' @importFrom graphics curve polygon text
#'
#' @examples
#' normregioncurve(mean = 0, sd = 1, from = 2, to = NULL, col = "Cyan", lower.tail = FALSE)
#' normregioncurve(mean = 4, sd = 2, from = 1, to = 5, col = "Cyan")
#'
#' @export
normregioncurve = function(mean,
                           sd,
                           from,
                           to,
                           col = "Magenta",
                           lower.tail = TRUE) {
  # DRAW NORMAL CURVE
  curve(dnorm(x, mean, sd), xlim = c(mean - 3 * sd, mean + 3 * sd))

  # AREA:
  # NOTE: for normal distributions, the probabilities of:
  # <= IS SAME AS <
  # >= IS SAME AS >
  if (is.null(from)) {
    # if start is beginning of curve
    area = pnorm(to, mean, sd, lower.tail = lower.tail)
  } else if (is.null(to)) {
    # if destination is end of curve
    area = pnorm(from, mean, sd, lower.tail = lower.tail)
  } else {
    # otherwise take the difference
    area = pnorm(to, mean, sd) - pnorm(from, mean, sd)
  }

  # DRAW REGION
  # fill in to/from for drawing polygon if they are undefined
  if (is.null(to))
    to = mean + 3 * sd
  if (is.null(from))
    from = mean - 3 * sd

  # x values corresponding to x-cords of points on the curve
  xcurve = seq(from, to, length = 1000)

  # Y values corresponding to the x values
  ycurve = dnorm(xcurve, mean, sd)

  # Fill in the polygon with the given vertices
  polygon(c(from, xcurve, to), c(0, ycurve, 0), col = col)

  # Round area
  areap = round(area, 4)

  # Paste the text onto the graph, centered in curve
  text(mean, 0.5 * dnorm(mean, mean, sd), paste0("Area = ", areap))

  # return probability
  return (area)
}

#' Plot gamma distribution and probability region
#'
#' Given shape, scale, and probability region start and end,
#' plot gamma distribution and probability region.
#' From MATH 4753 Lab 6.
#'
#' @param shape shape parameter of gamma distribution
#' @param scale scale parameter of gamma distribution
#' @param from start of probability region
#' @param to end of probability region
#' @param xlim vector for the x-axis limits
#' @param col color of the shaded region. default magenta.
#'
#' @importFrom stats dgamma pgamma
#' @importFrom graphics curve polygon text
#'
#' @examples
#' gammaregioncurve(shape=3, scale=2, from=2, to=5, xlim=c(-.05, 20))
#' gammaregioncurve(shape=6, scale=3, from=1, to=4, xlim=c(-.05, 20), col="Red")
#'
#' @export
gammaregioncurve = function(shape, scale, from, to, xlim, col = "Magenta") {
  # DRAW CURVE
  curve(dgamma(x, shape = shape, scale = scale), xlim = xlim)

  # DRAW REGION
  # x values corresponding to x-cords of points on the curve
  xcurve = seq(from, to, length = 1000)

  # Y values corresponding to the x values
  xcurve = seq(from, to, length = 1000)
  ycurve = dgamma(xcurve, shape = shape, scale = scale)

  # Fill in the polygon with the given vertices
  polygon(c(from, xcurve, to), c(0, ycurve, 0), col = col)

  # AREA:
  area =
    pgamma(to, shape = shape, scale = scale) -
    pgamma(from, shape = shape, scale = scale)
  areap = round(area, 4)

  # Text for Area
  text(mean(xlim),
       0.5 * dgamma(shape, shape = shape, scale = scale),
       paste0("Area = ", areap))
}

#' Plot chi-squared distribution and probability region
#'
#' Given df and probability region start and end,
#' plot chi-squared distribution and probability region.
#' From MATH 4753 Lab 6.
#'
#' @param df df parameter of chisq distribution
#' @param from start of probability region
#' @param to end of probability region
#' @param xlim vector for the x-axis limits
#' @param ylim vector for the y-axis limits
#' @param col color of the shaded region. default magenta.
#'
#' @importFrom stats dchisq pchisq
#' @importFrom graphics curve polygon text
#'
#' @examples
#' chisqregioncurve(df=2, from=2, to=4, xlim=c(-0.5, 20), ylim=c(0, 0.5), col="Pink")
#'
#' @export
chisqregioncurve = function(df, from, to, xlim, ylim, col = "Magenta") {
  # DRAW CURVE
  curve(
    dchisq(x, df = df),
    xlim = xlim,
    ylim = ylim,
    main = paste("chisq(df=", df, ")", sep = "")
  )

  # DRAW REGION
  # x values corresponding to x-cords of points on the curve
  xcurve = seq(from, to, length = 1000)

  # Y values corresponding to the x values
  xcurve = seq(from, to, length = 1000)
  ycurve = dchisq(xcurve, df = df)

  # Fill in the polygon with the given vertices
  polygon(c(from, xcurve, to), c(0, ycurve, 0), col = col)

  # AREA:
  area = pchisq(to, df = df) - pchisq(from, df = df)
  areap = round(area, 4)

  # Text for Area
  text(mean(xlim), mean(ylim), paste0("Area = ", areap))
}

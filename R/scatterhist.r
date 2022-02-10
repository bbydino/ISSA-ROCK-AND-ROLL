#' Create Scatter Histogram
#'
#' Plots a scatterplot and histogram. From MATH 4753 Lab 1
#'
#' @param x x coordinates for the plot
#' @param y y coordinates for the plot
#' @param xlab a label for the x axis, defaults to empty string
#' @param ylab a label for the y axis, defaults to empty string
#' @param col color for the scatter plot
#'
#' @examples
#' Speed <- cars$speed
#' Distance <- cars$dist
#' scatterhist(Speed, Distance, xlab = "Speed", ylab = "Distance")
#'
#' @export
scatterhist <- function(x, y, xlab = "", ylab = "", col = "black") {
  zones = matrix(c(2, 0, 1, 3), ncol = 2, byrow = TRUE)
  layout(zones,
         widths = c(4 / 5, 1 / 5),
         heights = c(1 / 5, 4 / 5))
  xhist = hist(x, plot = FALSE)
  yhist = hist(y, plot = FALSE)
  top = max(c(xhist$counts, yhist$counts))
  par(mar = c(3, 3, 1, 1))
  plot(x, y, col = col)
  par(mar = c(0, 3, 1, 1))
  barplot(
    xhist$counts,
    axes = FALSE,
    ylim = c(0, top),
    space = 0,
    col = rainbow(length(xhist$counts))
  )
  par(mar = c(3, 0, 1, 1))
  barplot(
    yhist$counts,
    axes = FALSE,
    xlim = c(0, top),
    space = 0,
    horiz = TRUE,
    col = rainbow(length(xhist$counts))
  )
  par(oma = c(3, 3, 0, 0))
  mtext(
    xlab,
    side = 1,
    line = 1,
    outer = TRUE,
    adj = 0,
    at = 0.8 * (mean(x) - min(x)) / (max(x) - min(x))
  )
  mtext(
    ylab,
    side = 2,
    line = 1,
    outer = TRUE,
    adj = 0,
    at = (.8 * (mean(y) - min(y)) / (max(y) - min(y)))
  )
}

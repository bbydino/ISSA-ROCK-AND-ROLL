% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scatterhist.r
\name{scatterhist}
\alias{scatterhist}
\title{Create Scatter Histogram}
\usage{
scatterhist(x, y, xlab = "", ylab = "", col = "black")
}
\arguments{
\item{x}{x coordinates for the plot}

\item{y}{y coordinates for the plot}

\item{xlab}{a label for the x axis, defaults to empty string}

\item{ylab}{a label for the y axis, defaults to empty string}

\item{col}{color for the scatter plot}
}
\description{
Plots a scatterplot and histogram. From MATH 4753 Lab 1
}
\examples{
Speed <- cars$speed
Distance <- cars$dist
scatterhist(Speed, Distance, xlab = "Speed", ylab = "Distance")

}

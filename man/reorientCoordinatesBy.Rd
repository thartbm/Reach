% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/localization.R
\name{reorientCoordinatesBy}
\alias{reorientCoordinatesBy}
\title{Rotate coordinates in a data frame relative to reference angle.}
\usage{
reorientCoordinatesBy(df, X, Y, orient, CCW = FALSE)
}
\arguments{
\item{df}{Data frame with at least 2 columns.}

\item{X}{name of column with X coordinates.}

\item{Y}{name of column with Y coordinates.}

\item{orient}{Either numeric specifying one angle to use for all coordinates
or a string specifying the column with reference angles.}

\item{CCW}{(boolean)}
}
\value{
Data frame with X and Y rotated by the angles in \code{orient}.
}
\description{
Rotate coordinates in a data frame relative to reference angle.
}

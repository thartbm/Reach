% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reaches.R
\name{getSplinedVelocity}
\alias{getSplinedVelocity}
\title{Get velocity profile after spline interpolation}
\usage{
getSplinedVelocity(x, y, t, spar = 0.01)
}
\arguments{
\item{x}{X-coordinates of a trajectory}

\item{y}{Y-coordinates of a trajectory}

\item{t}{Timestamps of the X and Y coordinates}

\item{spar}{Smoothing parameter for the spline, default: 0.01 (0.00-1.00)}
}
\value{
This function returns a data frame with spline interpolated velocity
and time for the trajectory given by x, y and t. The first velocity sample
will always be zero.
}
\description{
Returns velocity based on spline-smoothed trajectory.
}
\details{
#
}
\examples{
#
}

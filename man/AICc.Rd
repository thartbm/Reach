% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/models.R
\name{AICc}
\alias{AICc}
\title{Calculate AIC based on MSE, corrected for low parameter models.}
\usage{
AICc(MSE, k, N)
}
\arguments{
\item{MSE}{A vector of mean squared errors between data and model predictions.}

\item{k}{A vector of the number of free parameters for each model.}

\item{N}{The number of observations in the data set.}
}
\value{
A vector of AIC values for each model, corrected for low parameter numbers.
}
\description{
...
}
\details{
#
}
\examples{
#
}

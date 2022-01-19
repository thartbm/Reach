
#' @title Get a confidence interval for a summary statistic of a numeric vector.
#' @param data A numeric vector.
#' @param variance Set the variance (e.g. when the population variance is known). 
#' Only used for method='t-distr'.
#' @param conf.level Set the confidence level (.50 < conf.level < 1.00, default: 0.95).
#' @param method One of 't-distr' or 'bootstrap' to either use the sample 
#' t-distribution to _calculate_ the confidence interval or to bootstrap it by
#' resampling from the sample in 'data'. Using the sample t-distribution can 
#' be much faster and allows overriding the sample variance, and according to 
#' the central limit theorom, the distribution of sample means is normal, so that is fine.
#' Bootstrapping is slower but works on any data distribution and allows other
#' descriptors like the median, or variance by setting FUN.
#' @param resamples The number of samples to draw with resplacement from the data
#' to bootstrap values of the descriptor.
#' @param FUN The function to use as descriptor for every sample when bootstrapping. 
#' @return A vector with the upper and lower bound of the confidence interval.
#' @description Calculate or bootstrap a confidence interval.
#' @details Tell a story here.
#' @examples
#' # for normally distributed data, using a t-interval is fine:
#' normal <- rnorm(1000)
#' getConfidenceInterval(normal)
#' getConfidenceInterval(normal, variance=1)
#' 
#' # but perhaps we want to bootstrap it for other distributions:
#' exponential <- rexp(1000)
#' getConfidenceInterval(exponential)
#' getConfidenceInterval(exponential,method='bootstrap')
#' 
#' @export
getConfidenceInterval <- function(data, variance = stats::var(data, na.rm=TRUE), conf.level = 0.95, method='t-distr', resamples=1000, FUN=mean) {
  
  data <- data[which(!is.na(data))]
  
  if (method %in% c('t-distr','t')) {
    
    z = stats::qt((1 - conf.level)/2, df = length(data) - 1, lower.tail = FALSE)
    
    xbar = base::mean(data)
    sdx  = base::sqrt(variance/length(data))
    
    return(c(xbar - z * sdx, xbar + z * sdx))
    
  }
  
  # add sample z-distribution?
  
  if (method %in% c('bootstrap','b')) {
    
    data <- data[which(is.finite(data))] #need is.finite due to NA values
    
    samplematrix <- base::matrix(base::sample(data, size = resamples*length(data), replace = TRUE), nrow = resamples)
    BS <- apply(samplematrix, c(1), FUN=FUN) 
    
    lo <- (1-conf.level)/2.
    hi <- 1 - lo
    
    return(stats::quantile(BS, probs = c(lo,hi)))
    
  }
  
}
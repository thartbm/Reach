
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

#' @title Get an eta-squared effect size to accompany a t-test.
#' @param g1 A numeric vector (scores for group 1).
#' @param g2 A numeric vector (scores for group 2).
#' @param mu A number for a single-sample t-test (default: 0).
#' @param na.rm Boolean: remove NA's from g1 and g2 (default: TRUE).
#' @return A number: the eta squared (effect size).
#' @description Calculate or bootstrap a confidence interval.
#' @details Eta-squared is a measure of effect size that expresses the 
#' propportion of variance explained by an effect. For a t-test, that 'effect'
#' is the separation of the data into two groups.
#' There is a way to do this for two groups, as well as for a single sample,
#' comparing the mean to a given number. There is no way (now) to do this for a
#' two-sample t-test comparing the difference in means to a hypothetical value,
#' nor does this function differentiate between single-sided or two-sided 
#' t-tests.
#' @examples
#' 
#' @export
etaSquaredTtest <- function(g1,g2=NA,mu=0,na.rm=TRUE) {
  
  doOneSample <- FALSE
  doTwoSample <- FALSE
  
  if (length(g2) == 1) {
    if (is.na(g2)) {
      doOneSample <- TRUE
    } else {
      # set mu to the single value in g2 and do a one sample one anyway?
    }
  } else {
    doTwoSample <- TRUE
  }
  
  if (doOneSample) {
    
    # compare group 1 mean with mu as explanation
    SStotal <- sum((g1-mean(g1,na.rm=na.rm))^2)
    SSeffect <- sum(((mean(g1, na.rm=na.rm) - mu)^2)*length(g1))
    # 
    # 
    return(SSeffect / SStotal)
    
  }
  
  if (doTwoSample) {
    
    overallmean <- mean(c(g1,g2),na.rm=na.rm)
    # compare overall mean with group means as explanation
    SStotal <- sum((c(g1,g2) - overallmean)^2, na.rm=na.rm)
    SSeffect <- sum(length(g1)*(mean(g1,na.rm=na.rm)-overallmean)^2, length(g2)*(mean(g2,na.rm=na.rm)-overallmean)^2)
    return(SSeffect / SStotal)
    
  }
  
}

# #' @title Orthogonal Distance Regression.
# #' @param X A predictor matrix (variables in columns, N observations in rows).
# #' @param y Column vector (N rows) with dependent variable.
# #' @return A list with: `beta` (row vector of weights) and `intercept`. 
# #' @description Perform orthogonal distance regression with multiple predictors.
# #' @details This function uses PCA to perform orthogonal distance regression
# #' or "total least squares regression") on y based on one or more predictors
# #' in X.
# #' Code taken from https://stats.stackexchange.com/questions/13152/how-to-perform-orthogonal-regression-total-least-squares-via-pca
# #' @examples
# #' # we'll predict petal width from other iris virginica properties:
# #' y <- iris$Petal.Width[101:150]
# #' X <- as.matrix(iris[101:150,1:3])
# #' pw.odr <- ODR(X,y)
# #' 
# #' # let's have a look at how well that predicts petal width:
# #' y_hat <- (X %*% pw.odr$beta) + pw.odr$intercept
# #' plot(y,y_hat,asp=1,xlim=c(0,3),ylim=c(0,3))
# #' @export
# ODR <- function(X,y) {
#   
#   v <- stats::prcomp(cbind(X,matrix(y)))$rotation
#   # v <- eigen(cov(cbind(x, y)))$vectors  # this supposedly faster for larger data sets
#   
#   # get coefficients and slope:
#   coeff <- -v[-ncol(v),ncol(v)] / v[ncol(v),ncol(v)]
#   intercept <- (mean(y) - (colMeans(X) %*% beta))[1,1]
#   
#   # get residuals:
#   y_hat <- cbind(X, 1) %*% c(coeff, intercept)
#   res   <- y - y_hat
#   
#   return(list('coeff'=coeff,
#               'intercept'=intercept,
#               'res'=res))
#   
# }

# 

#' @title Orthogonal Distance Regression.
#' @param x A 'predictor' matrix (variables in columns, N observations in rows).
#' @param y Column vector (N rows) with 'dependent' variable.
#' @return A list with: `coeff` (coefficients), `yfit` (fitted values), `err` (errors),
#' `resd` (residuals), `ssq` (sum of squares), `normal` (normal) and `pve` (proportion
#' variance explained)
#' @description Perform (multiple) orthogonal distance regression.
#' @details 
#' This function is copied from  the `pracma` package by Hans W. Borchers:
#' https://CRAN.R-project.org/package=pracma
#' 
#' I've added a proportion variance explained output to it.
#' 
#' @examples
#' 
#' @export
odregress <- function(x, y) {
  stopifnot(is.numeric(x), is.numeric(y))

  Z <- cbind(x, y)
  n <- nrow(Z)      # no. of data points
  m <- ncol(Z) - 1  # no. of independent variables
  
  # this line is different, because we don't want `repmat()`
  meanZ <- matrix(1, n, 1) %x% matrix(apply(Z, 2, mean), nrow=1, ncol=m+1)
  
  svdZ <- svd(Z - meanZ)
  V <- svdZ$v
  
  # coefficients (a) and intercept (b)
  a <- -V[1:m, m+1] / V[m+1, m+1]
  b <- mean(Z %*% V[, m+1]) / V[m+1, m+1]

  # Fitted values
  yfit <- cbind(x, 1) %*% c(a, b)
  resd <- y - yfit

  # orthogonal distance
  normal <- V[, m+1]
  err <- abs((Z - meanZ) %*% normal)
  ssq <- sum(err^2)
  
  # proportion of variance explained:
  # variance explained by first principle component / total variance in data
  # total_variance <- sum(apply(scale(Z), 2, var))
  # eig <- eigen(cov(scale(Z)))
  
  # this stays in the same scale, which I think we need:
  total_variance <- sum(apply(Z, 2, var))
  eig <- eigen(cov(Z))
  # to calculate the proportion variance explained:
  pve <- (max(eig$values)/total_variance)

  return( list(coeff = c(a, b), ssq = ssq, err = err,
               fitted = yfit, resid = resd, normal = normal, pve = pve) )
}
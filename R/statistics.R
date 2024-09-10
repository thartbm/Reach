
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
#' #
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
#' `resd` (residuals), `ssq` (sum of squares) and `normal` (normal).
#' @description Perform (multiple) orthogonal distance regression.
#' @details 
#' This function is copied from  the `pracma` package by Hans W. Borchers:
#' https://CRAN.R-project.org/package=pracma
#' 
#' For the 2D case, a pearson correlation coefficient can be returned.
#' No usable coefficient of determination seems to exist for other cases.
#' 
#' @examples
#' # we get data from the iris data set that is pretty correlated
#' # but since sepal length does not cause petal length,
#' # we use orthogonal distance regression:
#' x <- as.matrix(iris[101:150,1])
#' y <- as.matrix(iris[101:150,3])
#' odr <- odregress(x,y)
#' 
#' plot(x,y)
#' abline(a=odr$coeff[2], b=odr$coeff[1], col='red')
#' 
#' # compare to ordinary least squares:
#' ols <- lm(y ~ x)
#' abline(a=ols$coefficients[1], b=ols$coefficients[2], col='blue')
#' @export
odregress <- function(x, y) {
  stopifnot(is.numeric(x), is.numeric(y))

  Z <- cbind(x, y)  # bind data in one matrix
  n <- nrow(Z)      # no. of data points
  m <- ncol(Z) - 1  # no. of independent variables
  
  # this line is different, because we don't want `repmat()` as dependency
  meanZ <- matrix(1, n, 1) %x% matrix(apply(Z, 2, mean), nrow=1, ncol=m+1)
  
  svdZ <- svd(Z - meanZ) # singular value decomposition
  V <- svdZ$v # eigen vectors
  
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
  
  # R-squared: look at pls::pcr()
  # the below stuff is incorrect
  
  
  # # R-squared... well, not really
  # eigenvalues <- eigen(cov(Z))$values
  # explained_variance <- max(eigenvalues)
  # unexplained_variance <- sum(eigenvalues) - explained_variance
  # R.squared <- 1 - (unexplained_variance/explained_variance)
  
  
  if (dim(Z)[2] == 2) {
    
    sZ <- scale(Z, scale=FALSE)
    r <-  sum(apply(sZ, 1, prod)) / prod( sqrt( colSums(sZ^2) ) )
    
    return( list(coeff = c(a, b), ssq = ssq, err = err,
                 fitted = yfit, resid = resd, normal = normal,
                 r = r
    ) )
    
  }
  
  return( list(coeff = c(a, b), ssq = ssq, err = err,
               fitted = yfit, resid = resd, normal = normal
               ) )
}

# here is how to get regression confidence intervals:
# https://rpubs.com/aaronsc32/regression-confidence-prediction-intervals
# but I think this only works for regular regression, not ODR or (PCA based regression)

#' @title Latin Square
#' @param order An integer value of 2 or higher, setting the size of the latin square.
#' @param seed Optional seed for the random number generator
#' @return A matrix of size order x order
#' @description Returns a latin square where each index 1:order occurs in each row
#' as well as in each column.
#' @details 
#' If you want the same latin square repeatedly, set the seed argument.
#' @examples
#' latinSquare(5)
#' @export
latinSquare <- function(order, seed=NULL) {
  
  # order has to be an integer value of at least 2
  # and up to some maximum that the machine can handle
  if (order < 2) {
    cat('order must be 2 or higher, quitting\n')
    return(NA)
  }
  
  # order has to be an integer  
  o <- as.integer(order)
  if (o != order) {
    cat(sprintf('order must be an integer, rounding %0.8f to %d\n', order, o))
  }
  
  # make an empty matrix to hold the latin square:
  # it should have order rows and order columns
  lsq <- matrix(data=NA, nrow = o, ncol = o)
  
  # first row will be filled with the numbers 1 through to order
  onerow <- c(1:o)
  # we make sure each row and column is unique
  for (rown in c(1:o)) {
    lsq[rown,] <- onerow
    onerow <- c(onerow[2:o], onerow[1])
  }
  
  # now we shuffle, using the RNG seed, if provided:
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # first shuffle the rows:
  lsq <- lsq[sample(c(1:o)),]
  # then shuffle columns:
  lsq <- lsq[,sample(c(1:o))]
  
  return(lsq)
  
}

# Two-Rate Model -----

# fit a state-space model with a slow and fast process to learning data
# see Smith et al. (2006; Plos Comp Biol)

#' @title Evaluate the two-rate model given parameters and a reach deviation
#' schedule.
#' @param par A named vector with the four model parameters (see details).
#' @param schedule A vector of length N with the perturbation schedule.
#' @return A data frame with three columns: `slow`, `fast` and `total` and N 
#' rows, so that each row has the output of the slow and fast process on each
#' trials as well as the total system output.
#' @description This function is part of a set of functions to fit and 
#' evaluate the two-rate model of motor learning.
#' @details The `par` argument is a named numeric vector that should have the
#' following elements:
#' - Ls: the slow learning rate
#' - Lf: the fast learning rate
#' - Rs: the slow retention rate
#' - Rf: the fast retention rate 
#' @examples
#' # write example...
#' @export
twoRateModel <- function(par, schedule) {
  
  # thse values should be zero at the start of the loop:
  Et <- 0 # previous error: none
  St <- 0 # state of the slow process: aligned
  Ft <- 0 # state of the fast process: aligned
  
  # we'll store what happens on each trial in these vectors:
  slow <- c()
  fast <- c()
  total <- c()
  
  # now we loop through the perturbations in the schedule:
  for (t in c(1:length(schedule))) {
    
    # first we calculate what the model does
    # this happens before we get visual feedback about potential errors
    St <- (par['Rs'] * St) - (par['Ls'] * Et)
    Ft <- (par['Rf'] * Ft) - (par['Lf'] * Et)
    Xt <- St + Ft
    
    # now we calculate what the previous error will be for the next trial:
    if (is.na(schedule[t])) {
      Et <- 0
    } else {
      Et <- Xt + schedule[t]
    }
    
    # at this point we save the states in our vectors:
    slow <- c(slow, St)
    fast <- c(fast, Ft)
    total <- c(total, Xt)
    
  }
  
  # after we loop through all trials, we return the model output:
  return(data.frame(slow,fast,total))
  
}







#' @title Get the MSE for how well the two-rate model fits reaches.
#' @param par A named numeric vector with the four model parameters (see 
#' twoRateModel).
#' @param schedule A numeric vector of length N with the perturbation schedule.
#' @param reaches A numeric vector of length N with reach deviations matching
#' the perturbation schedule.
#' @param checkStability Only stable solutions will be permitted.
#' @return A float: the mean squared error between the total model output and
#' the reach deviations.
#' @description This function is part of a set of functions to fit and 
#' evaluate the two-rate model of motor learning.
#' @details The `par` argument is a named numeric vector that should have the
#' following elements:
#' - Ls: the slow learning rate
#' - Lf: the fast learning rate
#' - Rs: the slow retention rate
#' - Rf: the fast retention rate 
#' @examples
#' # write example...
#' @export
twoRateMSE <- function(par, schedule, reaches, checkStability=FALSE) {
  
  bigError <- mean(schedule^2, na.rm=TRUE) * 10
  
  # learning and retention rates of the fast and slow process are constrained:
  if (par['Ls'] > par['Lf']) {
    return(bigError)
  }
  if (par['Rs'] < par['Rf']) {
    return(bigError)
  }
  
  
  if (checkStability) {
    aa <- ((par['Rf'] - par['Lf']) * (par['Rs'] - par['Ls'])) - (par['Lf'] * par['Ls'])
    if (aa <= 0) {
      return(bigError)
    }
    
    p <- par['Rf'] - par['Lf'] - par['Rs'] + par['Ls']
    q <- p^2 + (4 * par['Lf'] * par['Ls'])
    bb <- ((par['Rf'] - par['Lf'] + par['Rs'] - par['Ls'])  +  sqrt(q))
    if (bb >= 2) {
      return(bigError)
    }
    
  }
  
  return( mean((twoRateModel(par, schedule)$total - reaches)^2, na.rm=TRUE) )
  
}



#' @title Fit the two-rate model to reach deviations.
#' @param schedule A vector of length N with the perturbation schedule.
#' @param reaches A vector of length N with reach deviation data.
#' @param gridpoints Number of values used for each parameters in a gridfit.
#' @param gridfits Number of best gridfits to use in MSE fit.
#' @param checkStability Only stable solutions will be allowed.
#' @return A named numeric vector with the optimal parameters that fit the two
#' rate model to the data as best as possible, with these elements:
#' - Ls: the slow learning rate
#' - Lf: the fast learning rate
#' - Rs: the slow retention rate
#' - Rf: the fast retention rate 
#' @description This function is part of a set of functions to fit and 
#' evaluate the two-rate model of motor learning.
#' @details
#' ?
#' @examples
#' # there is example data in this package:
#' data("tworatedata")
#' 
#' # first we baseline it, and get a median for every trial:
#' baseline <- function(reachvector,blidx) reachvector - mean(reachvector[blidx], na.rm=TRUE)
#' tworatedata[,4:ncol(tworatedata)] <- apply(tworatedata[,4:ncol(tworatedata)], 
#'                                            FUN=baseline, 
#'                                            MARGIN=c(2), 
#'                                            blidx=c(17:32))
#' reaches <- apply(tworatedata[4:ncol(tworatedata)], FUN=median, MARGIN=c(1), na.rm=TRUE)
#' 
#' # and we extract the schedule:
#' schedule <- tworatedata$schedule
#' 
#' # now we can fit the model to the reaches, given the schedule:
#' par = twoRateFit(schedule, reaches)
#' 
#' # and plot that:
#' model <- twoRateModel(par=par, schedule=schedule)
#' plot(reaches,
#'      type='l', col='#333333',
#'      xlab='trial', ylab='reach deviation [deg]',
#'      xlim=c(0,165), ylim=c(-35,35),
#'      bty='n', ax=FALSE)
#' lines(c(1,33,33,133,133,145,145),c(0,0,30,30,-30,-30,0),col='#AAAAAA')
#' lines(c(145,164),c(0,0),col='#AAAAAA',lty=2)
#' lines(model$slow,col='blue')
#' lines(model$fast,col='red')
#' lines(model$total,col='purple')
#' axis(1,c(1,32,132,144,164),las=2)
#' axis(2,c(-30,-15,0,15,30))
#' 
#' @export
twoRateFit <- function(schedule, reaches, gridpoints=6, gridfits=6, checkStability=FALSE) {
  
  parvals <- seq(1/gridpoints/2,1-(1/gridpoints/2),1/gridpoints)
  
  searchgrid <- expand.grid('Ls'=parvals,
                            'Lf'=parvals,
                            'Rs'=parvals,
                            'Rf'=parvals)
  # evaluate starting positions:
  MSE <- apply(searchgrid, FUN=twoRateMSE, MARGIN=c(1), schedule=schedule, reaches=reaches, checkStability=checkStability)
  
  # fit them:
  allfits <- do.call("rbind",
                     apply( searchgrid[order(MSE)[1:gridfits],],
                            MARGIN=c(1),
                            FUN=optimx::optimx,
                            fn=twoRateMSE,
                            method='L-BFGS-B',
                            lower=c(0,0,0,0),
                            upper=c(1,1,1,1),
                            schedule=schedule,
                            reaches=reaches,
                            checkStability=checkStability) )
  
  
  # pick the best fit:
  win <- allfits[order(allfits$value)[1],]

  return(unlist(win[1:4]))
  
}

# # Asymptotic Decay Model - - - - -

#' # fit a single exponential to learning data, with two parameters:
#' # - a learning rate
#' # - an asymptote (for incomplete learning)
#' 
#' 
#' #' @title Execute a simple model given parameters and a reach
#' #' deviation schedule.
#' #' @param par A named vector with the model parameter (see details).
#' #' @param schedule A vector of length N with the perturbation schedule.
#' #' @return A data frame with one column: `output`, and N rows, so that each row
#' #' has the output of the modeled process on each trials.
#' #' @description This function is part of a set of functions to fit and
#' #' evaluate an exponential decay model with asymptote.
#' #' @details The `par` argument is a named numeric vector that should have the
#' #' following element:
#' #' - lambda: learning rate
#' #' - N0: asymptote
#' #'
#' #' The schedule usually consists of a sequence of ones. It will be multiplied
#' #' by the asymptote.
#' #' @examples
#' #' ?
#' #' @export
#' asymptoticDecayModel <- function(par, schedule) {
#' 
#'   # the process and error states are initialized at 0:
#'   Pt <- 0
#'   Et <- 0
#' 
#'   # the total output is stored here:
#'   output <- c()
#' 
#'   for (t in c(1:length(schedule))) {
#' 
#'     Pt <- Pt - (par['lambda'] * Et)
#' 
#'     # now we calculate what the previous error will be for the next trial:
#'     if (is.na(schedule[t])) {
#'       Et <- 0
#'     } else {
#'       Et <- Pt + (schedule[t] * par['N0'])
#'     }
#' 
#'     # at this point we save the process state in our vector:
#'     output <- c(output, Pt)
#' 
#'   }
#' 
#'   return(data.frame(output))
#' 
#' }
#' 
#' #' @title Get the MSE for how well an asymptotic decay model fits reaches.
#' #' @param par A named numeric vector with the model parameter (see
#' #' asymptoticDecayModel).
#' #' @param schedule A numeric vector of length N with the perturbation schedule.
#' #' @param signal A numeric vector of length N with reach deviations matching
#' #' the perturbation schedule.
#' #' @return A float: the mean squared error between the total model output and
#' #' the reach deviations.
#' #' @description This function is part of a set of functions to fit and
#' #' evaluate exponential decay model with asymptote..
#' #' @details The `par` argument is a named numeric vector that should have the
#' #' following element:
#' #' - lambda: the learning rate
#' #' - N0: the asymptote
#' #'
#' #' The schedule is usually a sequence of ones, which is multiplied by the
#' #' asymptote in the function.
#' #' @examples
#' #' @export
#' asymptoticDecayMSE <- function(par, schedule, signal) {
#' 
#'   MSE <- mean((asymptoticDecayModel(par, schedule)$output - signal)^2, na.rm=TRUE)
#' 
#'   return( MSE )
#' 
#' }
#' 
#' #' @title Fit an asymptotic decay model to reach deviations.
#' #' @param schedule A vector of length N with the perturbation schedule.
#' #' Usually a sequence of ones: `c(1,1,1,1,...)`.
#' #' @param signal A vector of length N with reach deviation data.
#' #' @param gridpoints Number of values for rate of change and asymptote, that
#' #' are tested in a grid.
#' #' @param gridfits Number of best results from gridsearch that are used for
#' #' optimizing a fit.
#' #' @return A named numeric vector with the optimal parameter that fits a simple
#' #' rate model to the data as best as possible, with these elements:
#' #' - lambda: the rate of change
#' #' - N0: the asymptote
#' #' @description This function is part of a set of functions to fit and
#' #' evaluate a simple learning rate model of motor learning.
#' #'
#' #' The schedule should usually be a sequence of ones. The reach deviations have
#' #' to be baselined (but the baseline is cut from the data).
#' #' @details
#' #' ?
#' #' @examples
#' #' # write example!
#' #' @import optimx
#' #' @export
#' asymptoticDecayFit <- function(schedule, signal, gridpoints=11, gridfits=10) {
#' 
#'   # set the search grid:
#'   parvals <- seq(1/gridpoints/2,1-(1/gridpoints/2),1/gridpoints)
#' 
#'   maxAsymptote <- 2*max(abs(signal), na.rm=TRUE)
#' 
#'   # define the search grid:
#'   searchgrid <- expand.grid('lambda' = parvals,
#'                             'N0'     = parvals * maxAsymptote)
#' 
#'   # evaluate starting positions:
#'   MSE <- apply(searchgrid, FUN=asymptoticDecayMSE, MARGIN=c(1), schedule=schedule, signal=signal)
#' 
#'   # run optimx on the best starting positions:
#'   allfits <- do.call("rbind",
#'                      apply( data.frame(searchgrid[order(MSE)[1:gridfits],]),
#'                             MARGIN=c(1),
#'                             FUN=optimx::optimx,
#'                             fn=asymptoticDecayMSE,
#'                             method='L-BFGS-B',
#'                             lower=c(0,0),
#'                             upper=c(1,maxAsymptote),
#'                             schedule=schedule,
#'                             signal=signal ) )
#'   
#'   # pick the best fit:
#'   win <- allfits[order(allfits$value)[1],]
#'   
#'   # return the best parameters:
#'   return(unlist(win[1:2]))
#'   
#' }


# Exponential Function -----

# fit a single exponential to learning data, with two parameters:
# - a learning rate
# - an asymptote (for incomplete learning)

# will replace asymptotic decay, but should do the same thing
# except that's it's slightly closer to an actual exponential
# and uses it behind the scenes, so that:
# it should run faster
# people can use the output for maths


#' @title Run an exponential function given parameters and a reach
#' deviation schedule. Errors decay exponentially.
#' @param par A named vector with the model parameter (see details).
#' @param timepoints An integer indicating the number of trials (N), or a vector
#' with N trial numbers (these can have missing values or be fractions). If an
#' integer, the timepoints at which the exponential will be evaluated is:
#' 0, 1 ... N-2, N-1
#' @param mode String: "learning" or "washout", sets the function's direction.
#' @param setN0 NULL or number, if the asymptote is known, it can be set here.
#' @return A data frame with two columns: `timepoint` and `output`, and N rows,
#' so that each row has the output of the modeled process on each trial.
#' @description This function is part of a set of functions to fit and
#' evaluate an exponential decay model with asymptote.
#' @details The `par` argument is a named numeric vector that should have the
#' following element:
#' - lambda: learning rate
#' - N0: asymptote
#' @examples
#' exponentialModel(par=c('lambda'=0.2, 'N0'=25), timepoints=100)
#' @export
exponentialModel <- function(par, timepoints, mode='learning', setN0=NULL) {
  
  if (length(timepoints) == 1) {
    timepoints <- c(0:(timepoints-1))
  }
  
  if (is.numeric(setN0)) {
    par['N0'] = setN0
  }

  if (mode == 'learning') {
    output = par['N0'] - ( par['N0'] * (1-par['lambda'])^timepoints )
  }
  if (mode == 'washout') {
    output = par['N0'] * (par['lambda'])^timepoints
  }
  
  return(data.frame(trial=timepoints,
                    output=output))
  
}

#' @title Get the MSE between an exponential and a series of reach deviations.
#' @param par A named numeric vector with the model parameters (see
#' exponentialModel).
#' @param signal A numeric vector of length N with reach deviations matching
#' the perturbation schedule.
#' @param timepoints Either an integer with the number of trials (N) or a vector
#' with N trial numbers (this can have missing values or fractions). The 
#' exponential will be evaluated at those timepoints.
#' @param mode String: "learning" or "washout", sets the function's direction.
#' @return A float: the mean squared error between the total model output and
#' the reach deviations.
#' @param setN0 NULL or number, if the asymptote is known, it can be set here.
#' @description This function is part of a set of functions to fit and
#' evaluate an exponential function to describe a series of reach deviations.
#' @details The `par` argument is a named numeric vector that should have the
#' following element:
#' - lambda: the learning rate
#' - N0: the asymptote
#' @examples
#' data(tworatedata)
#' learning <- rowMeans(tworatedata[which(tworatedata$block == 2),c(4:20)], na.rm=TRUE)
#' exponentialMSE(par=c('lambda'=0.2, 'N0'=25), signal=learning)
#' @export
exponentialMSE <- function(par, signal, timepoints=c(0:(length(signal)-1)), mode='learning', setN0=NULL) {
  
  MSE <- mean((Reach::exponentialModel(par, timepoints, mode=mode, setN0=setN0)$output - signal)^2, na.rm=TRUE)
  
  return( MSE )
  
}

#' @title Fit an asymptotic decay model to reach deviations.
#' @param signal A vector of length N with reach deviation data. These should
#' start around 0 and go up (ideally they are baselined).
#' @param timepoints NULL or a vector of length N with the timepoints at which
#' to evaluate the exponential. If NULL, the N values in `signal` are placed
#' at: 0, 1, ... N-2, N-1.
#' @param mode A string, one of "learning" or "washout". For "learning" the
#' signal starts at 0 and increases with exponentially decaying errors, going
#' towards asymptote ("N0"), and for "washout" it starts at "N0" and approaches
#' 0 over time.
#' @param gridpoints Number of values for rate of change and asymptote, that
#' are tested in a grid.
#' @param gridfits Number of best results from gridsearch that are used for
#' optimizing a fit.
#' @param setN0 NULL or number, if the asymptote is known, it can be set here.
#' @param asymptoteRange NULL or a vector specifying the upper and lower bound for
#' the asymptote (N0). If NULL, the range will be (-1,2) * max(signal) which may be 
#' too wide for very noisy data.
#' @return A named numeric vector with the optimal parameter that fits a simple
#' rate model to the data as best as possible, with these elements:
#' - lambda: the rate of change in the range [0,1]
#' - N0: the asymptote (or starting value) in the unit of the signal
#' @description This function is part of a set of functions to fit and
#' evaluate a simple exponential function to reach deviations.
#' @details
#' ?
#' @examples
#' data(tworatedata)
#' learning <- rowMeans(tworatedata[which(tworatedata$block == 2),c(4:20)], na.rm=TRUE)
#' par <- exponentialFit(signal=learning)
#' par
#' plot(c(0:99), learning, ylim=c(0,35))
#' expfit <- Reach::exponentialModel(par=par, timepoints=seq(0,99,0.5))
#' lines(expfit, col='red')
#' @import optimx
#' @export
exponentialFit <- function(signal, timepoints=length(signal), mode='learning', gridpoints=11, gridfits=10, setN0=NULL,asymptoteRange=NULL) {
  
  # set the search grid:
  parvals <- seq(1/gridpoints/2,1-(1/gridpoints/2),1/gridpoints)

  if (is.null(asymptoteRange)) {
    # set a wiiiiide range... especially for single participants, the range may or may not work depending on how noisy their data is
    asymptoteRange <- c(-1,2)*max(abs(signal), na.rm=TRUE)
  }
  
  # define the search grid:
  # if (is.numeric(setN0)) {
  #   searchgrid <- expand.grid('lambda' = parvals)
  #   lo <- c(0)
  #   hi <- c(1)
  # }
  if (is.null(setN0)) {
    searchgrid <- expand.grid('lambda' = parvals,
                              'N0'     = parvals * diff(asymptoteRange) + asymptoteRange[1] )
    lo <- c(0,asymptoteRange[1])
    hi <- c(1,asymptoteRange[2])
  } else {
    searchgrid <- expand.grid('lambda' = parvals,
                              'N0'     = setN0)
    lo <- c(0,setN0)
    hi <- c(1,setN0)
  }
  # evaluate starting positions:
  MSE <- apply(searchgrid, FUN=exponentialMSE, MARGIN=c(1), signal=signal, timepoints=timepoints, mode=mode, setN0=setN0)
  
  # if (is.null(setN0)) {
  #   X <- data.frame(searchgrid[order(MSE)[1:gridfits],])
  # } else {
  #   X <- data.frame('lambda'=searchgrid[order(MSE)[1:gridfits],])
  # }

  # run optimx on the best starting positions:
  allfits <- do.call("rbind",
                     apply( data.frame(searchgrid[order(MSE)[1:gridfits],]),
                            MARGIN=c(1),
                            FUN=optimx::optimx,
                            fn=Reach::exponentialMSE,
                            method     = 'L-BFGS-B',
                            lower      = lo,
                            upper      = hi,
                            timepoints = timepoints,
                            signal     = signal,
                            mode       = mode,
                            setN0      = setN0 ) )
  
  # pick the best fit:
  win <- allfits[order(allfits$value)[1],]
  
  if (is.null(setN0)) {
    winpar <- unlist(win[1:2])
  } else {
    winpar <- c( 'lambda' = unlist(win[1]), 
                 'N0'     = setN0)
    names(winpar) <- c('lambda', 'N0')
  }
  
  # return the best parameters:
  return(winpar)
  
}


# Offset error decay -----

#' @title Values of an offset exponential decay given parameters and time points
#' @param par A named vector with the function parameter (see details).
#' @param timepoints An integer indicating the number of trials (N), or a vector
#' with N trial numbers (these can have missing values or be fractions) with the
#' first timepoint having index 0.
#' If this is an integer, the timepoints at which the exponential will be 
#' evaluated is: 0, 1 ... N-2, N-1
#' @return A data frame with two columns: `trial` and `value`, and N rows,
#' so that each row has the output of the modeled process on each trial.
#' @description This function is part of a set of functions to fit and
#' evaluate an exponential decay model with asymptote and offset.
#' @details The `par` argument is a named numeric vector that should have the
#' following elements:
#' - r: rate (of decay/learning)
#' - s: span (difference between starting point and asymptote)
#' - o: offset (added to the whole function)
#' @export
offsetErrorDecayModel <- function(par, timepoints) {
  
  # parameters:
  # r: decay rate
  # s: span of the function
  # o: offset of the 
  
  if (length(timepoints) == 1) {
    timepoints <- c(0:(timepoints-1))
  }
  
  return( data.frame( trial = timepoints, 
                      value = (((1-par['r'])^timepoints) * par['s']) + par['o'] )
             )
  
}


#' @title Get the MSE between offset exponential decay and a time series
#' @param par A named numeric vector with the model parameters (see details).
#' @param signal A numeric vector of length N with reach deviations matching
#' the perturbation schedule.
#' @param timepoints Either an integer with the number of trials (N) or a vector
#' with N trial numbers (this can have missing values or fractions). The 
#' exponential will be evaluated at those timepoints.
#' @return A float: the mean squared error between the total model output and
#' the time series.
#' @description This function is part of a set of functions to fit and
#' evaluate an offset exponential decay function to a time series.
#' @details The `par` argument is a named numeric vector that should have the
#' following elements:
#' - r: rate (of decay/learning)
#' - s: span (difference between starting point and asymptote)
#' - o: offset (added to the whole function)
#' @export
offsetErrorDecayMSE <- function(par, signal, timepoints=c(0:(length(signal)-1))) {
  
  MSE <- mean((Reach::offsetErrorDecayModel(par, timepoints)$value - signal)^2, na.rm=TRUE)
  
  return( MSE )
  
}



#' @title Fit an asymptotic decay model to reach deviations.
#' @param signal A vector of length N with reach deviation data. These should
#' start around 0 and go up (ideally they are baselined).
#' @param timepoints NULL or a vector of length N with the timepoints at which
#' to evaluate the exponential. If NULL, the N values in `signal` are placed
#' at: 0, 1, ... N-2, N-1.
#' @param gridpoints Number of values for rate of change and asymptote, that
#' are tested in a grid.
#' @param gridfits Number of best results from gridsearch that are used for
#' optimizing a fit.
#' @param spanRange The boundaries for the fit, specifying the minimum and 
#' maximum difference between the starting value and asympotic level of the 
#' exponential decay function.
#' @return A named numeric vector with the optimal parameter that fits an
#' offset exponential decay function to the given timeseries. It hes these
#' parameters:
#' `r`: rate (of decay / learning) of the function
#' `s`: the span of the function
#' `o`: offset of the function from zero
#' The starting point of the function is the offset + span. For any other points
#' you can run the `offsetErrorDecayModel()` with the time points you're interested
#' in, as well as the fitted parameters.
#' @description This function is part of a set of functions to fit and
#' evaluate an exponential error decay function to reach errors.
#' @import optimx
#' @export
offsetErrorDecayFit <- function(signal, timepoints=length(signal), gridpoints=11, gridfits=10, spanRange=NULL) {
  
  # set the search grid:
  parvals <- seq(1/gridpoints/2,1-(1/gridpoints/2),1/gridpoints)
  
  if (is.null(spanRange)) {
    # set a wiiiiide range... especially for single participants, the range may or may not work depending on how noisy their data is
    spanRange <- c(0.5,1.5)*diff(range(signal, na.rm=TRUE))
  }
  
  offsetRange <- c(0,1)*stats::median(signal, na.rm=TRUE) # this can NOT go below 0... 
  # if the timeseries goes below 0, the error metric is nonsensical
  
  
  searchgrid <- expand.grid('r' = parvals,
                            's' = (parvals * diff(spanRange))+min(spanRange),
                            'o' = parvals * diff(offsetRange))
  lo <- c(0,spanRange[1],offsetRange[1])
  hi <- c(1,spanRange[2],offsetRange[2])
  # print(lo)
  # print(hi)
  
  # evaluate starting positions:
  MSE <- apply(searchgrid, FUN=Reach::offsetErrorDecayMSE, MARGIN=c(1), signal=signal, timepoints=timepoints)
  
  # run optimx on the best starting positions:
  allfits <- do.call("rbind",
                     apply( data.frame(searchgrid[order(MSE)[1:gridfits],]),
                            MARGIN=c(1),
                            FUN=optimx::optimx,
                            fn=offsetErrorDecayMSE,
                            method     = 'L-BFGS-B',
                            lower      = lo,
                            upper      = hi,
                            timepoints = timepoints,
                            signal     = signal
                            ) )
  
  # pick the best fit:
  win <- allfits[order(allfits$value)[1],]
  
  
  winpar <- unlist(win[1:3])

  # return the best parameters:
  return(winpar)
  
}



# multi-modal -----

#' @title Probability of data points given a multi-modal distribution.
#' @param par A named vector with the model parameter (see details).
#' @param x A sequence of numbers to evaluate the probability of.
#' @return A vector of probabilities according to the multi modal normal distribution.
#' @description This function is part of a set of functions to fit and
#' evaluate multi modal (normal) distribution of data points.
#' @details This is an experimental work-in-progress model that is supposed to fit a
#' multi-modal distribution. That is: I'm not sure how well it works, and its' a 
#' little rough around the edges. For one, the output parameters from the optimal 
#' fit are in a different format from what the other functions expect. Second, the 
#' output is usually not a probability function that sums to 1. Finally, it might 
#' not return the actual best fit.
#' 
#' That said, the `par` argument is a data frame (or named list) with columns:
#' - m: mean
#' - s: standard deviation
#' - w: weight
#' for every normal distribution of the model
#' @export
multiModalModel <- function(par, x) {
  
  probs <- rep(0, length(x))
  
  m <- par$m # means
  s <- par$s # standard deviations
  w <- par$w # weights
  
  # weights should add up to 1:
  w <- w / sum(w)
  
  for (nd in c(1:length(m))) {
    
    probs <- probs + w[nd] * stats::dnorm(  x    = x,
                                            mean = m[nd],
                                            sd   = s[nd]  )
    
  }
  
  return(probs) # this is the sum likelihood of each x, given par
  
}

#' @title Likelihood of data given a multi-modal probability distribution.
#' @param par A named vector with the model parameter (see details).
#' @param x A sequence of numbers to evaluate the probability of.
#' @return The sum of the log of the probabilities returned by `multiModalModel()`.
#' @description This function is part of a set of functions to fit and
#' evaluate multi modal (normal) distribution of data points.
#' @details The `par` argument is a data frame (or named list) with columns:
#' - m: mean
#' - s: standard deviation
#' - w: weight
#' for every normal distribution of the model
#' @export
multiModalModelNLL <- function(par, x) {
  
  n = length(par)/3
  # print(list(c(1:n),c('m','s','w')))
  
  par <- data.frame(matrix(par,byrow=TRUE,ncol=3,dimnames=list(c(1:n),c('m','s','w'))))
  
  probs <- multiModalModel(par, x)
  
  probs[which((probs-1) == 0)] <- .Machine$double.eps

  nll <- -1 * sum(log(probs))
  
  if (!is.finite(nll)) {
    nll <- -1 * sum(log(rep(.Machine$double.eps, length(x)))) # minimum probability
  }
  
  return(nll)
  
}

#' @title Grid search to find likely N-modal distributions parameters given data. 
#' @param x A sequence of numbers to evaluate the probability of.
#' @param n The number of normal distributions to consider.
#' @param points The number of points to search in each dimension of the search grid.
#' @param best Return the parameters for the `best` best fits.
#' @return The best parameters for N-modal distributions for a data set x.
#' @description This function is part of a set of functions to fit and
#' evaluate multi-modal (normal) distribution of data points.
#' @examples
#' multiModalGridSearch(x=c(rnorm(50,0,2),rnorm(100,10,4)), n=2, points=7, best=10)
#' @export
multiModalGridSearch <- function(x, n=2, points=7, best=10) {
  
  # all parameter values to expand will be stored here:
  v <- list()
  # add n * points:
  for (i in c(1:n)) {
    # add means:
    v[[sprintf('m%d', i)]] <- seq(min(x),max(x),length.out=points)
    # add standard deviations:
    v[[sprintf('s%d', i)]] <- seq(min(abs(diff(x)))/2, abs(diff(range(x))), length.out=points)
    # add weights:
    v[[sprintf('w%d', i)]] <- seq(0.001,0.999,length.out=points)
  }

  df_grid <- expand.grid(v)
  
  # get all the likelihoods:
  likelihoods <- apply(X=df_grid, MARGIN=c(1), FUN=multiModalModelNLL, x=x)
  
  # indexes of the best ones:
  idx <- order(unlist(likelihoods))[1:best]
  
  # return the best parameter sets:
  return(df_grid[idx,])
  
}

#' @title Fit function to optimize an N-modal distributions to given data. 
#' @param x A sequence of numbers to evaluate the probability of.
#' @param n The number of normal distributions to consider.
#' @param points The number of points to search in each dimension of a search grid.
#' @param best Return the parameters for the `best` best fits.
#' @return The best parameters for N-modal distributions for a data set x.
#' @description This function is part of a set of functions to fit and
#' evaluate multi-modal (normal) distribution of data points.
#' @examples
#' multiModalFit(x=c(rnorm(50,0,2),rnorm(100,10,4)), n=2, points=7, best=10)
#' @export
multiModalFit <- function(x, n=2, points=9, best=9) {

  x <- sort(x)
  
  top <- Reach::multiModalGridSearch(x, n, points=points, best=best)
  
  
  # # add means:
  # v[[sprintf('m%d', i)]] <- seq(min(x),max(x),length.out=points)
  # # add standard deviations:
  # v[[sprintf('s%d', i)]] <- seq(min(abs(diff(x)))/2, abs(diff(range(x))), length.out=points)
  # # add weights:
  # v[[sprintf('w%d', i)]] <- seq(0.001,0.999,length.out=points)
  

  lo <- rep( c(min(x), min(abs(diff(x)))/2, 0.0001), n)
  hi <- rep( c(max(x), abs(diff(range(x))), 0.9999), n)
  
  print(data.frame(lo,hi))
  
  # control <- list('maximize'=FALSE) # optimx
  control <- list() # optim
  # run optimx on the best starting positions:
  allfits <- do.call("rbind",
                     apply( top,
                            MARGIN=c(1),
                            # FUN=optimx::optimx,
                            FUN = stats::optim,
                            fn=multiModalModelNLL,
                            method     = 'L-BFGS-B', # get rid of warnings?
                            lower      = lo,
                            upper      = hi,
                            control    = control,
                            x          = x) )
  
  print(allfits)
  
  # return(allfits)
  # 
  # as.data.frame(uno)[order(unlist(as.data.frame(uno)$value))[1],]
  
  allfits <- as.data.frame(allfits)
  
  print(allfits)
  
  # pick the best fit:
  win <- allfits[order(unlist(allfits$value))[1],]
  
  print(win)
  
  winpar <- as.numeric(unlist(win)[1:(3*n)])
  
  print(winpar)
  
  # outpar <- list('m'=c(), 's'=c(), 'w'=c())
  # for (pidx in c(1:3)) {
  #   for (midx in c(1:n)) {
  #     outpar[[ c('m', 's', 'w')[pidx] ]].append( winpar[((midx-1)*3)+pidx] )
  #   }
  # }
  
  # one-liner from likelihood fuction:
  dfpar <- data.frame(matrix(winpar,byrow=TRUE,ncol=3,dimnames=list(c(1:n),c('m','s','w'))))
  
  # return the best parameters:
  return(dfpar)
  
}



# Model Evaluation -----


#' @title Calculate AIC based on MSE.
#' @param MSE A vector of Mean Squared Errors, one for each model.
#' @param k A vector of the number of free parameters for each model.
#' @param N The number of observations in the data set.
#' @return A vector of AIC values for each model.
#' @description This function 
#' @details
#' #
#' @examples
#' #
#' @export
AIC <- function(MSE, k, N) {
  return( (N * log(MSE)) + (2 * k) )
}

#' @title Calculate AIC based on MSE, corrected for low parameter models.
#' @param MSE A vector of mean squared errors between data and model predictions.
#' @param k A vector of the number of free parameters for each model.
#' @param N The number of observations in the data set.
#' @return A vector of AIC values for each model, corrected for low parameter numbers.
#' @description ...
#' @details
#' #
#' @examples
#' #
#' @export
AICc <- function(MSE, k, N) {
  return( AIC(MSE, k, N) * (((2*k^2) + 2*k) / (N - k - 1)) )
}

#' @title Calculate relative likelihood.
#' @param crit Vector of criterion values for models.
#' @return Vector of likelihoods. The best model will have a likelihood of 1, and
#' models with likelihoods lower than 0.05 could be considered "significantly" worse
#' than the best model.
#' @description This function calculate a relative log likelihood using criterion values
#' for a set of models. The criterion should indicate a better model for a lower value.
#' @details If the input vector is named, the output vector will also be named.
#' @examples #
#' @export
relativeLikelihood <- function(crit) {
  return( exp( ( min( crit  ) - crit  ) / 2 ) )
}
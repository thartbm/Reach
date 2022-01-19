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
#' ?
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
#' ?
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
#' tworatedata[,4:ncol(tworatedata)] <- apply(tworatedata[,4:ncol(tworatedata)], FUN=baseline, MARGIN=c(2), blidx=c(17:32))
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
#' plot(reaches,type='l',col='#333333',xlab='trial',ylab='reach deviation [deg]',xlim=c(0,165),ylim=c(-35,35),bty='n',ax=FALSE)
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
  
  # USE OPTIMX !
  
  #optimxInstalled <- require("optimx")
  # if (optimxInstalled) {
  #   optimFUN <- optimx
  #   method <- 'L-BFGS-B'
  # } else {
  #   optimFUN <- optim
  #   method <- 'Nelder-Mead'
  # }

  
  # if (optimxInstalled) {
  #   # run optimx on the best starting positions:
  #   allfits <- do.call("rbind",
  #                      apply( searchgrid[order(MSE)[1:gridfits],],
  #                             MARGIN=c(1),
  #                             FUN=optimx::optimx,
  #                             fn=twoRateMSE,
  #                             method='L-BFGS-B',
  #                             lower=c(0,0,0,0),
  #                             upper=c(1,1,1,1),
  #                             schedule=schedule,
  #                             reaches=reaches,
  #                             checkStability=checkStability) )
  #   
  #   # pick the best fit:
  #   win <- allfits[order(allfits$value)[1],]
  #   
  #   # return the best parameters:
  #   return(unlist(win[1:4]))
  #   
  # } else {
  #   
  #   cat('(consider installing optimx, falling back on optim now)\n')
  #   
  #   # use optim with Nelder-Mead after all:
  #   allfits <- do.call("rbind",
  #                      apply( searchgrid[order(MSE)[1:gridfits],],
  #                             MARGIN=c(1),
  #                             FUN=stats::optim,
  #                             fn=twoRateMSE,
  #                             method='Nelder-Mead',
  #                             schedule=schedule,
  #                             reaches=reaches,
  #                             checkStability=checkStability) )
  #   
  #   # pick the best fit:
  #   win <- allfits[order(unlist(data.frame(allfits)[,'value']))[1],]
  #   
  #   # return the best parameters:
  #   return(win$par)
  #   
  # }
  
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

# Asymptotic Decay Model -----

# fit a single exponential to learning data, with two parameters:
# - a learning rate
# - an asymptote (for incomplete learning)


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
#' #' ?
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
#'   # testing if optimx is installed and making it available it so:
#'   optimxInstalled <- require("optimx")
#'   
#'   if (optimxInstalled) {
#'     
#'     # run optimx on the best starting positions:
#'     allfits <- do.call("rbind",
#'                        apply( data.frame(searchgrid[order(MSE)[1:gridfits],]),
#'                               MARGIN=c(1),
#'                               FUN=optimx::optimx,
#'                               fn=asymptoticDecayMSE,
#'                               method='L-BFGS-B',
#'                               lower=c(0,0),
#'                               upper=c(1,maxAsymptote),
#'                               schedule=schedule,
#'                               signal=signal ) )
#'     
#'     # pick the best fit:
#'     win <- allfits[order(allfits$value)[1],]
#'     
#'     # return the best parameters:
#'     return(unlist(win[1:2]))
#'     
#'   } else {
#'     
#'     cat('(consider installing optimx, falling back on optim now)\n')
#'     
#'     # use optim with Nelder-Mead after all:
#'     allfits <- do.call("rbind",
#'                        apply( data.frame(searchgrid[order(MSE)[1:gridfits],]),
#'                               MARGIN=c(1),
#'                               FUN=optim,
#'                               fn=asymptoticDecayMSE,
#'                               method='Nelder-Mead',
#'                               schedule=schedule,
#'                               signal=signal ) )
#'     
#'     # pick the best fit:
#'     win <- allfits[order(unlist(data.frame(allfits)[,'value']))[1],]
#'     
#'     # return the best parameters:
#'     return(win$par)
#'     
#'   }
#'   
#' }

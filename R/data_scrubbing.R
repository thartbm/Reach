# training data =====

getTrainingReachDeviations <- function(groups='all', sessions=c('rotated','aligned'), at='maxvel') {
  
  # at:
  # - maxvel_screened (indicated in screening)
  # - maxvel_velprof (grabbed from velocity profile)
  # - perc33 (at 33 percent of home-target distance, can be other percentages)
  
  pfiles <- read.csv('data/files.csv', stringsAsFactors = F)
  fileURLs <- read.csv('data/urls.csv', stringsAsFactors = F)
  
  if (groups[1] == 'all') {
    groups <- unique(fileURLs$group)
  }
  
  # for each group there should now be:
  # - reach deviations as a matrix or data frame
  # with for one dimension:
  # - trial number
  # - rotation
  # - block
  # - condition: aligned or rotated
  # and the other dimension:
  # - participant
  
  # (if participants are columns)
  # can we have multiple indices with the other data?
  # or should they be in another file?
  # what is the easiest / least confusing to use?

}

# no-cursor data =====

getNoCursorReachDeviations <- function(groups='all', sessions=c('rotated','aligned'), at='endpoint') {
  
  # at:
  # - endpoint
  # - maxvel_screened (indicated in screening)
  # - maxvel_velprof (grabbed from velocity profile)
  # - perc33 (at 33 percent of home-target distance, can be other percentages)
  
  pfiles <- read.csv('data/files.csv', stringsAsFactors = F)
  fileURLs <- read.csv('data/urls.csv', stringsAsFactors = F)
  
  if (groups[1] == 'all') {
    groups <- unique(fileURLs$group)
  }
  
  # for each group there should now be:
  # - reach deviations as a matrix or data frame
  # with for one dimension:
  # - trial number
  # - rotation
  # - block
  # - condition: aligned, inclusive or exclusive
  # and the other dimension:
  # - participant
  
  # (if participants are columns)
  # can we have multiple indices with the other data?
  # or should they be in another file?
  # what is the easiest / least confusing to use?
  
}



# localization data =====

# localization files are already 1 sample (x,y) / (a,r) per trial

# circle fitting ===== 

#' Shift localization responses so they are centred on the origin
#' 
#' @param df Data frame of localization data
#' @return The data frame with corrected \code{tapx_cm} and \code{tapy_cm} columns. So
#' that the localization responses fall closest to a circle with radius 12 cm and
#' origin (0,0).
#' @examples
#' 
correctArcShift <- function(df) {
  
  idx <- which(df$selected == 1)
  tapx <- df$tapx_cm[idx]
  tapy <- df$tapy_cm[idx]
  
  control <- list('maxit'=10000, 'ndeps'=1e-9 )
  par <- c('xc'=0,'yc'=0)
  sol <- optim(par=par, circleErrors, gr=NULL, tapx, tapy, r=12, control=control)
  
  # this also corrects the non-selected trials:
  df$tapx_cm <- df$tapx_cm - sol$par[['xc']]
  df$tapy_cm <- df$tapy_cm - sol$par[['yc']]
  
  return(df)
  
}

#' Get mean squared error between coordinates and a circle
#' 
#' @param par Vector with xc and yc parameters: x and y of the circle's middle
#' @param X Vector of X coordinates
#' @param Y Vector of Y coordinates
#' @param r The radius of the circle
#' @return The mean squared error between the distances of \code{X} and 
#' \code{Y} from the position in par and the radius \code{r}.
#' @examples
#' 
circleErrors <- function(par,X,Y,r) {
  
  return(mean((sqrt((X-par[['xc']])^2+(Y-par[['yc']])^2)-r)^2))
  
}

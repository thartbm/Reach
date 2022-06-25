# training data =====

# getAllTrainingReachDeviations <- function(groups='all', sessions=c('rotated','aligned'), at='maxvel') {
#   
#   # at:
#   # - maxvel_screened (indicated in screening)
#   # - maxvel_velprof (grabbed from velocity profile)
#   # - perc33 (at 33 percent of home-target distance, can be other percentages)
#   
#   utils::data('files', package='handlocs')
#   utils::data('urls', package='handlocs')
#   
#   if (groups[1] == 'all') {
#     groups <- unique(urls$group)
#   }
#   
#   # for each group there should now be:
#   # - reach deviations as a matrix or data frame
#   # with for one dimension:
#   # - trial number
#   # - rotation
#   # - block
#   # - condition: aligned or rotated
#   # and the other dimension:
#   # - participant
#   
#   # (if participants are columns)
#   # can we have multiple indices with the other data?
#   # or should they be in another file?
#   # what is the easiest / least confusing to use?
# 
# }
# 
# getFileReachDeviations <- function(filename) {
#   
#   df <- utils::read.csv(filename, stringsAsFactors = FALSE)
#   
#   df <- df[which(df$maxvelocity == 1),]
#   
#   for (target in unique(df$targetangle_deg)) {
#     
#     idx <- which(df$targetangle_deg == deg)
#     
#     df[idx,c('handx_cm','handy_cm')] <- rotateCoordinates(df[idx,c('handx_cm','handy_cm')], -target)
#     
#   }
#   
#   
#   
# }
# 
# # no-cursor data =====
# 
# getNoCursorReachDeviations <- function(groups='all', sessions=c('rotated','aligned'), at='endpoint') {
#   
#   # at:
#   # - endpoint
#   # - maxvel_screened (indicated in screening)
#   # - maxvel_velprof (grabbed from velocity profile)
#   # - perc33 (at 33 percent of home-target distance, can be other percentages)
#   
#   utils::data('files', package='handlocs')
#   utils::data('urls', package='handlocs')
#   
#   if (groups[1] == 'all') {
#     groups <- unique(urls$group)
#   }
#   
#   # for each group there should now be:
#   # - reach deviations as a matrix or data frame
#   # with for one dimension:
#   # - trial number
#   # - rotation
#   # - block
#   # - condition: aligned, inclusive or exclusive
#   # and the other dimension:
#   # - participant
#   
#   # (if participants are columns)
#   # can we have multiple indices with the other data?
#   # or should they be in another file?
#   # what is the easiest / least confusing to use?
#   
# }
# 



# utility functions =====

# This copied from github.com/thartbm/SMCL:

#' @title Rotate 2D trajectory
#' @param df A dataframe or matrix with two columns: X and Y coordinates.
#' @param angle An angle in degrees to rotate the trajectory by.
#' @param origin A vector with the coordinates to rotate around. Default (0,0)
#' @return Data frame with the rotated trajectory coordinates.
#' @description Rotate a trajectory of X,Y coordinates.
#' @details Not yet.
#' @examples
#' 
#' @export
rotateCoordinates <- function(df,angle,origin=c(0,0)) {
  
  df.names <- names(df)
  
  # create rotation matrix to rotate the X,Y coordinates
  th <- (angle/180) * pi
  R <- t(matrix(data=c(cos(th),sin(th),-sin(th),cos(th)),nrow=2,ncol=2))
  
  # put coordinates in a matrix, and subtract origin
  coordinates <- sweep(as.matrix(df), 2, origin)
  
  # rotate the coordinates, add the origin back in
  df <- as.data.frame(sweep(coordinates %*% R, 2, origin*-1))
  
  # restore column names
  names(df) <- df.names
  
  # return the rotated coordinates
  return(df)
  
}

#' @title Angular deviation of a reach from target at a specific point. 
#' @param trialdf Data frame representing the reach.
#' @param location String specifying which location to use (see details).
#' @param posunit String with the unit of x,y coordinates (pix, cm, ...)
#' @param timeunit String with the unit of time (s, ms, ...)
#' @param device String saying what position to use (hand, mouse, robot...)
#' @param holdvelocity Maximum velocity for a hold.
#' @param holdduration Minimum duration for a hold.
#' @return Matrix with 1 row, 5 columns:
#' 1: angular deviation
#' 2: target angle
#' 3: x position of location
#' 4: y position of location
#' 5: time of location
#' The idea is to combine this into a larger matrix (or data frame) with
#' multiple others, and then do further processing on that.
#' @description
#' ?
#' @details
#' The `location` argument specifies which point along the trajectory to use 
#' for a reach deviation (in degrees). It canhgave several (string) values:
#' 
#' - 'maxvel', requires a column 'maxvel' in the data frame with a single
#' value of 1 (and 0 otherwise). The coordinates at this sample are used.
#' - 'endpoint', uses the last sample of the trajectory, useful for
#' no-cursor trials with the return trajectory removed
#' - 'prX', uses the first sample at or beyond a proportion of distance from
#' the target, given by X (a float), the default setting: 'pr0.33333'
#' 
#' There are other options, but they are not consolidated yet.
#' @examples
#' ?
#' @export
getReachAngleAt <- function(trialdf, location='pr0.33333', posunit='pix', timeunit='ms', device='hand', holdvelocity=NA, holdduration=NA) {
  
  # location (string) determines where the angle of the reach is determined, it is one of:
  
  # maxvel: maximum velocity
  # endpoint: end of the reach (only makes sense after selection)
  # cmX: the last sample before this distance from home, where X is replaced by a numeral (deprecated: use prX)
  # prX: first sample at or beyond a proportion of distance from home to target, given by X (e.g. 'pr0.333333')
  # hold: at a hold point; also specify how long and at what maximum velocity the hold has to be in other arguments
  # smvX: first velocity peak in spline-smoothed trajectory, beyond a percentage distance from home to target given by X (e.g. 'smv0.10')
  
  
  # return a matrix of two numbers:
  reachangle = matrix(data=NA,nrow=1,ncol=5)
  colnames(reachangle) <- c( 'reachdeviation_deg', 
                             'targetangle_deg', 
                             sprintf('%sx_%s',device,posunit), 
                             sprintf('%sy_%s',device,posunit), 
                             sprintf('time_%s',timeunit) )
  
  
  # extract the relevant reach information
  x <- trialdf[,sprintf('%sx_%s',device,posunit)]
  y <- trialdf[,sprintf('%sy_%s',device,posunit)]
  t <- trialdf[,sprintf('time_%s',timeunit)]
  
  angle <- trialdf[1,'targetangle_deg']
  target <- as.numeric(trialdf[1,c(sprintf('targetx_%s',posunit),sprintf('targety_%s',posunit))])
  
  # always return the target angle?
  reachangle[1,2] <- angle
  
  # rotate the trajectory:
  # - avoids problems with atan2 angles around 180 / -180
  # - puts the target at 0, so angular deviation is easy to get
  trajectory <- Reach::rotateCoordinates(data.frame(x,y),-1*angle)
  x <- trajectory[,1]
  y <- trajectory[,2]
  
  # now try find the specified location in this reach:
  # if we can't find it, we need to know
  invalidlocation <- TRUE
  
  # maximum velocity, should be a column in the data...
  # this only happens with preprocessing or manual screening
  # use 'smv' if this is not the case...
  if (location == 'maxvel') {
    MV <- trialdf[,'maxvel']
    rown <- which(MV == 1)
    if (length(rown) > 1) {
      rown <- rown[1]
    }
    if (length(rown) == 0) {
      # no maximum velocity defined!
      return(reachangle)
    }
    invalidlocation <- FALSE
  }
  
  # end point, just the last point in the reach
  if (location == 'endpoint') {
    rown <- length(x)
    invalidlocation <- FALSE
  }
  
  # cutoff in centimers, the last sample before this cutoff distance is reached
  # this assumes that people don't go back, or that there is only one movement from home to target
  if (substring(location,1,2) == 'cm') {
    distance <- as.numeric(substring(location, 3))
    
    # get the distance from home:
    dist <- sqrt(x^2 + y^2)
    
    # if there are no selected samples below 3 cm: return NAs
    if (length(which(dist < distance)) == 0) {
      return(reachangle)
    }
    
    # find the last sample, where dist < 3
    rown <- max(which(dist < distance))
    invalidlocation <- FALSE
  }
  
  # cutoff at a percentage from home to target in whatever unit is used
  if (substring(location,1,2) == 'pr') {
    distance <- as.numeric(substring(location, 3))
    #distance <- distance * sqrt(trialdf$targetx_pix[1]^2 + trialdf$targety_pix[1]^2)
    distance <- distance * sqrt(sum(target^2))
    
    # get the distance from home:
    dist <- sqrt(x^2 + y^2)
    
    # if there are no selected samples above 3 cm: return NAs
    if (length(which(dist > distance)) == 0) {
      return(reachangle)
    }
    
    # find the first sample, where dist > X
    rown <- min(which(dist > distance))
    invalidlocation <- FALSE
  }
  
  # find the first 'hold':
  if (substring(location,1,4) == 'hold') {
    holddistance <- as.numeric(substring(location, 5))
    #holdvelocity
    #holdduration # in timeunit: 's' or 'ms'?
    
    
    
  }
  
  # use smooth-splined trajectory to get angle at *first* velocity peak:
  if (substring(location,1,3) == 'smv') {
    
    # how far does the vleocity peak have to be away from the home position
    # (as percentage of home-target distance)
    if (nchar(location) > 3) {
      distance <- as.numeric(substring(location, 4))
    } else {
      distance <- 0.05
    }
    distance <- distance * sqrt(sum(target^2))
    
    dist <- sqrt(x^2 + y^2)
    
    VT <- getSplinedVelocity(x, y, t, spar=0.20)
    v <- c(0, 0, VT$velocity)
    
    peaks <- which(diff(sign(diff(v))) == -2 & dist > distance)
    if (length(peaks) > 0) {
      rown <- peaks[1]
      invalidlocation <- FALSE
    }
    
  }
  
  
  
  # if we don't have a valid location, we can't calculate an angle
  if (invalidlocation) {
    return(reachangle)
  }
  
  # calculate the angle at that point for the rotated trajectory
  # this is the angular deviation we are looking for
  angulardeviation <- (atan2(y[rown],x[rown]) / pi) * 180
  
  # put the result in the little matrix:
  reachangle[1,1] <- angulardeviation
  reachangle[1,2] <- angle
  reachangle[1,3] <- trialdf[rown,sprintf('%sx_%s',device,posunit)]
  reachangle[1,4] <- trialdf[rown,sprintf('%sy_%s',device,posunit)]
  reachangle[1,5] <- t[rown]
  
  return(reachangle)
  
}

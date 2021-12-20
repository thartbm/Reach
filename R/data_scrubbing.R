# training data =====

getAllTrainingReachDeviations <- function(groups='all', sessions=c('rotated','aligned'), at='maxvel') {
  
  # at:
  # - maxvel_screened (indicated in screening)
  # - maxvel_velprof (grabbed from velocity profile)
  # - perc33 (at 33 percent of home-target distance, can be other percentages)
  
  utils::data('files', package='handlocs')
  utils::data('urls', package='handlocs')
  
  if (groups[1] == 'all') {
    groups <- unique(urls$group)
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

getFileReachDeviations <- function(filename) {
  
  df <- utils::read.csv(filename, stringsAsFactors = FALSE)
  
  df <- df[which(df$maxvelocity == 1),]
  
  for (target in unique(df$targetangle_deg)) {
    
    idx <- which(df$targetangle_deg == deg)
    
    df[idx,c('handx_cm','handy_cm')] <- rotateCoordinates(df[idx,c('handx_cm','handy_cm')], -target)
    
  }
  
  
  
}

# no-cursor data =====

getNoCursorReachDeviations <- function(groups='all', sessions=c('rotated','aligned'), at='endpoint') {
  
  # at:
  # - endpoint
  # - maxvel_screened (indicated in screening)
  # - maxvel_velprof (grabbed from velocity profile)
  # - perc33 (at 33 percent of home-target distance, can be other percentages)
  
  utils::data('files', package='handlocs')
  utils::data('urls', package='handlocs')
  
  if (groups[1] == 'all') {
    groups <- unique(urls$group)
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


# circle fitting ===== 

#' Shift localization responses so they are centred on the origin
#' 
#' @param df Data frame with localization coordinates (X,Y).
#' @param var Variable of coordinates (default: 'tap')
#' @param unit Unit of the coordinates (default: 'cm')
#' @param r Radius of the circle the coordinates should be on (default: 1).
#' @return The data frame with corrected \code{tapx_cm} and \code{tapy_cm} 
#' columns. The corrected localization responses fall closest to a circle with
#' radius \code{r} (in \code{unit}) and origin (0,0). Only response with
#' \code{df$selected == 1} are used for this correction.
#' @details The parameters \code{var} and \code{unit} are combined with a lower
#' case \code{x} and \code{y}: \code{tapx_cm} and \code{tapy_cm} with default
#' settings. These should be columns in the data frame (\code{df}).
#' @export
circleCorrect <- function(df, unit='cm', var='tap', r=1) {
  
  if ('selected' %in% names(df)) {
    idx <- which(df$selected == 1)
  } else {
    idx <- seq(1,dim(df)[1])
  }
  
  tapx <- df[idx,sprintf('%sx_%s',var,unit)]
  tapy <- df[idx,sprintf('%sy_%s',var,unit)]
  
  control <- list('maxit'=10000, 'ndeps'=1e-9 )
  par <- c('xc'=0,'yc'=0)
  sol <- stats::optim(par=par, circleErrors, gr=NULL, tapx, tapy, r=r, control=control)
  
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
#' @export
circleErrors <- function(par,X,Y,r) {
  
  return(mean((sqrt((X-par[['xc']])^2+(Y-par[['yc']])^2)-r)^2))
  
}

#' Get the standard deviation of localization using a spline as 'mean'.
#' 
#' @param df Data frame with localization data.
#' @param unit The unit of position data (default: \code{cm}).
#' @param locvar The variable name of localization responses (default: \code{tap}).
#' @param handvar The variable name of hand position (default: \code{hand}).
#' @param r Radius of localization arc (default: 12).
#' @param CC (boolean) Perform circle correction (default: \code{TRUE}).
#' @param spar Smoothing parameter for the spline (0,1) (default: \code{0.85}).
#' @param rm.Extr (boolean) Remove the highest and lowest reach angles before 
#' spline interpolation (after fitting the spline). 
#' @return A spline object that predicts localization errors over reach angle.
#' @export
localizationSD <- function(df, unit='cm', locvar='tap', handvar='hand', r=1, CC=TRUE, spar=0.99, rm.Extr=FALSE) {
  
  # get data in useful form:
  df <- prepareSplineVariables(df, CC=CC, handvar=handvar, locvar=locvar, unit=unit, r=r)
  
  # fit smoothed spline to data:
  spl <- getLocalizationSpline(df, spar=spar)
  
  if (rm.Extr) {
    idx <- which(df$reachangle_deg > min(df$reachangle_deg) & df$reachangle_deg < max(df$reachangle_deg))
  } else {
    idx <- seq(1:length(df$reachangle_deg))
  }
  
  # predict localization error based on fitted smooth spline:
  PredLoc <- predict(spl, x=df$reachangle_deg[idx])$y
  PredLocError <- PredLoc - df$localizationerror_deg[idx]
  
  # pseudo standard deviation:
  return( sqrt( ( sum( PredLocError^2 ) / length(PredLocError) ) ) )
  
}

#' Get a smoothed spline object fit on localization errors over reach angles.
#' 
#' @param df Data frame with localization data, this contains at least the
#' variables: \code{reachangle_deg} and \code{localizationerror_deg}.
#' @param spar Smoothing parameter for the spline (0,1) (default: \code{0.50}). 
#' @return A spline object that predicts localization errors over reach angle.
#' @export
getLocalizationSpline <- function(df, spar=0.50) {
  
  # fit a smooth spline to predict localization error by reach angle:
  spl <- smooth.spline(x=df$reachangle_deg, y=df$localizationerror_deg, spar=spar, keep.data=F )
  
  return(spl)
  
}

#' Prepare data for smoothed spline fitting.
#' 
#' @param df Data frame with localization data, this contains at least the
#' variables: \code{reachangle_deg} and \code{localizationerror_deg}.
#' @param CC (boolean) Apply circleCorrect or not (default: \code{TRUE}).
#' @param handvar Part of column names specifying reach endpoints (default: \code{hand}).
#' @param locvar Part of column names specifying hand location (default: \code{tap}).
#' @param unit Unit of the four location variables (default: \code{cm}).
#' @param r Radius for the circleCorrect procedure (default: \code{1})
#' @return Hand lcalization data frame extended with columns to be used for
#' smooth spline fitting.
#' @details 
#' @export
prepareSplineVariables <- function(df, CC=TRUE, handvar='hand', locvar='tap', unit='cm',r=1) {
  
  # removed non-good data:
  if ('selected' %in% names(df)) {
    df <- df[which(df$selected == 1),]
  }
  
  # perform circle correction if specified:
  if (CC) {
    df <- circleCorrect( df, 
                         unit=unit, 
                         var=locvar, 
                         r=r)
  }
  
  # assemble hand/reach column names:
  handy <- df[,sprintf('%sy_%s',handvar,unit)]
  handx <- df[,sprintf('%sx_%s',handvar,unit)]
  
  # get reach endpoint angles:
  reachangle_deg <- (atan2(handy,handx)/pi)*180
  df$reachangle_deg <- reachangle_deg
  
  # get the localization coordinates in a separate data frame:
  tempdf <- data.frame('X'      = df[,sprintf('%sx_%s',locvar,unit)],
                       'Y'      = df[,sprintf('%sy_%s',locvar,unit)],
                       'relang' = reachangle_deg)
  
  # rotate localization coordinates relative to reach angle
  # (such that reach angle is 0 for all localizations)
  tempdf <- reorientCoordinatesBy(tempdf,X='X',Y='Y',orient='relang',CCW=FALSE)
  
  # get localization error as angle relative to reach angle:
  localizationerror_deg <- (atan2(tempdf$Y,tempdf$X)/pi)*180
  
  # do we need this at all?
  df$localizationangle_deg <- df$reachangle_deg + localizationerror_deg
  
  # get localization error:
  df$localizationerror_deg <- localizationerror_deg
  
  return(df)
  
}


#' Rotate coordinates in a data frame relative to reference angle.
#' 
#' @param df Data frame with at least 2 columns.
#' @param X name of column with X coordinates.
#' @param Y name of column with Y coordinates.
#' @param orient Either numeric specifying one angle to use for all coordinates
#' or a string specifying the column with reference angles.
#' @param CCW (boolean)
#' @return Data frame with X and Y rotated by the angles in \code{orient}.
#' @export
reorientCoordinatesBy <- function(df,X,Y,orient,CCW=FALSE) {
  
  if (!CCW) {
    multiply <- -1
  } else {
    multiply <-  1
  }
  
  if (is.character(orient)) {
    if (!orient %in% names(df)) {
      cat(sprintf('"%s" needs to be a column in the data frame\n',orient))
    } else if (! is.numeric(df[,orient])) {
      cat(sprintf('column "%s" needs to be numeric\n',orient))
    } else {
      
      orientvals <- unique(df[,orient])
      for (orientval in orientvals) {
        or.idx <- which(df[,orient] == orientval)
        coords <- rotateCoordinates(data.frame('X'=df[or.idx,X],'Y'=df[or.idx,Y]),
                                    angle=orientval*multiply)
        df[or.idx,X] <- coords$X
        df[or.idx,Y] <- coords$Y
      }
      
    }
  } else if (is.numeric(orient)) {
    coords <- rotateCoordinates(data.frame('X'=df[,X],'Y'=df[,Y]),
                                angle=orient*multiply)
    df[,X] <- coords$X
    df[,Y] <- coords$Y
  }
  
  return(df)
  
}
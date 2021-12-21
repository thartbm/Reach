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
#' @param spar Smoothing parameter for the spline (0,1) (default: \code{0.50}). 
#' @return A spline object that predicts localization errors over reach angle.
#' @export
localizationSD <- function(df, unit='cm', locvar='tap', handvar='hand', r=1, CC=TRUE, spar=0.50) {

  df <- prepareSplineVariables(df, CC=CC, handvar=handvar, locvar=locvar, unit=unit, r=r)
  
  spl <- getLocalizationSpline(df, spar=spar)
  
  # predict
  # get differences with prediction
  # convert to "standard deviation"
  
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

prepareSplineVariables <- function(df, CC=TRUE, handvar='hand', locvar='tap', unit='cm',r=1) {
  
  # removed non-good data:
  if ('selected' %in% names(df)) {
    df <- df[which(df$selected == 1),]
  }
  
  # perform circle correction if specified:
  if (CC) {
    df <- circleCorrect(df, unit=unit, var=locvar,r=r)
  }
  
  # get reach endpoint angles:
  handyvar <- sprintf('%sy_%s',handvar,unit)
  handxvar <- sprintf('%sx_%s',handvar,unit)
  df$reachangle_deg <- (atan2(df[,handyvar],df[,handxvar])/pi)*180
  
  # get localization angle:
  locyvar <- sprintf('%sy_%s',locvar,unit)
  locxvar <- sprintf('%sx_%s',locvar,unit)
  df$localizationangle_deg <- (atan2(df[,locyvar],df[,locxvar])/pi)*180
  
  # get localization error:
  df$localizationerror_deg <- df$localizationangle_deg - df$reachangle_deg
  
  return(df)
  
}
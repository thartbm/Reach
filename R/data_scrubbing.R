


correctArcShift <- function(df) {
  
  idx <- which(df$selected == 1)
  tapx <- df$tapx_cm[idx]
  tapy <- df$tapy_cm[idx]
  
  control <- list('maxit'=10000, 'ndeps'=1e-9 )
  par <- c('xc'=0,'yc'=0)
  sol <- optim(par=par, circleErrors, gr=NULL, tapx, tapy, r=12, control=control)
  
  df$tapx_cm <- df$tapx_cm - sol$par[['xc']]
  df$tapy_cm <- df$tapy_cm - sol$par[['yc']]
  
  return(df)
  
}

circleErrors <- function(par,X,Y,r) {
  
  return(mean((sqrt((X-par[['xc']])^2+(Y-par[['yc']])^2)-r)^2))
  
}

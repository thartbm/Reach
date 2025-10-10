
#' @title Create a plot with a polar heatmap.
#' @param x Vector of M+1 angular bin edges.
#' @param y Vector of N+1 distance bin edges.
#' @param z Matrix (MxN) with two-dimensional histogram (frequency data), for 
#' example from SMCL::hist2d.
#' @param mincol Vector of length 3 with RGB values [0-1] for the minimum value
#' in z.
#' @param maxcol Vector of length 3 with RGB values [0-1] for the maximum value
#' in z. Other values in z are scaled, and each of the R, G and B values is 
#' linearly interpolated. No support for colormaps (yet). 
#' @param xlim Vector with values limiting the x axis of the resulting plot.
#' @param ylim Vector with values limiting the y axis of the resulting plot. By
#' default, the is set to c(-1,1) for both x and y, as this will include the 
#' full polar figure. Only change these values when the data do not cover a
#' circular data set.
#' @param xunit The angular unit (of the x-coordinates). Can be 'degrees' or
#' 'radians'. Will be converted to 'radians' if set in degrees.
#' @param border Draw borders around the parts of the heatmap. By default this
#' is set to NA, which means no border is drawn. If set to a numeric value, it
#' is used as the linewidth of the borders.
#' @param bordercol The color of the borders drawn around parts of the heatmap.
#' Default is 'white', can be any color accepted by base R graphics.
#' @param resolution The cicrular parts of the heatmap are approximated as a 
#' polygon, with at maximum this number of degrees between vectors. Default: 1.
#' @param alpha The alpha value of the heatmap. Does not apply to the border.
#' @param overlay Whether to put the polar heatmap into the active plot, or 
#' create a new one. Default is FALSE: create new plot.
#' @param origin The coordinates of the centre of the polar heatmap. Useful 
#' when overlay=TRUE. Defaults to (0,0).
#' @param scale The size of the polar heatmap. Useful when overlay=TRUE.
#' Defaults to 1.
#' @param main Main title when creating a new plot window.
#' @return 
#' ?
#' @description 
#' ?
#' @details 
#' ?
#' @examples
#' ?
#' @export
polarHeatMap <- function(x,y,z,mincol=c(0.94,0.98,0.99),maxcol=c(0.06,0.82,0.88),xlim=NA,ylim=NA,xunit='degrees',border=NA,bordercol='white',resolution=1,alpha=1,overlay=FALSE,origin=c(0,0),scale=1,main='') {
  
  # x: area edges in some form of angles (degrees [default] or radians)
  # y: area edges in distances from the origin
  # z: matrix with values in regions defined by x & y edges (NA to not plot the area)
  
  # maybe x/y/z can be a data frame?
  
  # mincol: color for the minimum values in z
  # maxcol: color for the maximum values in z
  
  if (any(is.na(xlim))) {
    # set xlim to fit all possible data:
    #xlim <- rep(max(abs(y)),2) * c(-1,1)
    xlim <- c(-1,1)
  }
  if (any(is.na(ylim))) {
    # set xlim to fit all possible data:
    #ylim <- rep(max(abs(y)),2) * c(-1,1)
    ylim <- c(-1,1)
  }
  
  resolution <- (resolution/180)*pi
  
  #par(mar=c(4, 4, 2, 2), pin=c(width,height))
  
  if (overlay == FALSE) {
  
    graphics::plot.new() # is this allowed... yes, but then everything else has to be manually added?
    
    graphics::plot.window(xlim = xlim, ylim = ylim, asp = 1)
    graphics::title(main = main)
    
  }
  # use `rgb()`
  
  # scale z to fit into 0-1 range:
  z <- z - min(z)
  z <- z / max(z)
  
  # convert to RGB values within the min/max colors:
  R <- (z * (maxcol[1]-mincol[1])) + mincol[1]
  G <- (z * (maxcol[2]-mincol[2])) + mincol[2]
  B <- (z * (maxcol[3]-mincol[3])) + mincol[3]
  
  # scale y to fit into the figure circle:
  y <- y / max(y)
  
  # get x to be in radians:
  if (xunit == 'degrees') {
    x <- (x / 180) * pi
  }
  
  # print(dim(z))
  # print(length(x))
  # print(length(y))
  
  allPolygons <- list()
  # create all the polygons
  for (xi in seq(dim(z)[1])) {
    for (yi in seq(dim(z)[2])) {
      if (!is.na(z[xi,yi])) {
        x1 <- x[xi]
        x2 <- x[xi+1]
        y1 <- y[yi]
        y2 <- y[yi+1]
        #print(c(x1,x2))
        xs <- seq(x1,x2,length.out=ceiling(abs(diff(c(x1,x2)))/resolution))
        X <- (c(cos(xs)*y1, rev(cos(xs)*y2)) * scale) + origin[1]
        Y <- (c(sin(xs)*y1, rev(sin(xs)*y2)) * scale) + origin[2]
        allPolygons[[length(allPolygons)+1]] <- list('x'=X, 'y'=Y, 'col'=grDevices::rgb(R[xi,yi],G[xi,yi],B[xi,yi],alpha=alpha))
      }
    }
  }
  
  for (p in allPolygons) {
    graphics::polygon(x=p$x, y=p$y, col=p$col, border=NA)
  }
  
  if (!is.na(border)) {
    for (p in allPolygons) {
      graphics::polygon(x=p$x, y=p$y, col=NA, border=bordercol, lwd=border)
    }
  }
  
  
}


#' @title Get frequencies for a 2D histogram
#' @param x Vector of X-coordinates, or a Mx2 matrix with M sets of X and Y 
#' coordinates, or data frame with x and y column.
#' @param y If X is not a matrix or data frame, but a vector this has to be a 
#' vector of equal length with Y coordinates.
#' @param nbins A numeric vector with two integers: the number of bins for X 
#' and for Y coordinates, defaults to c(25, 25).
#' @param edges A list with the edges for the X and Y coordinates respectively.
#' This allows irregular bin sizes. Note that the number of edges is the number
#' of bins plus 1. If `edges` is given it overrides `nbins`.
#' @return A list with three entries:
#' - `freq2D` numeric matrix with counts for each bin
#' - `x.edges` numeric vector with the edges used for x coordinates
#' - `y.edges` numeric vector with the edges used for y coordinates
#' @description 
#' ?
#' @details 
#' ?
#' @examples
#' ?
#' @export
hist2d <- function(x, y=NA, nbins=c(25,25), edges=NA) {
  
  if (is.data.frame(x)) {
    # check shape of x?
    df <- x
  } else if (is.matrix(x)) {
    # check shape of x?
    df <- as.data.frame(x)
  } else {
    df <- data.frame('x'=x, 'y'=y)
  }
  
  # code below, somewhat based on:
  # http://stackoverflow.com/questions/18089752/r-generate-2d-histogram-from-raw-data
  
  if (is.numeric(nbins)) {
    x.edges <- seq(floor(min(df[,1])), ceiling(max(df[,1])), length=nbins[1])
    y.edges <- seq(floor(min(df[,2])), ceiling(max(df[,2])), length=nbins[2])
  }
  
  if (is.list(edges)) {
    x.edges <- edges[[1]]
    y.edges <- edges[[2]]
  }
  
  xbincount <- findInterval(df[,1], x.edges, rightmost.closed = T, left.open = F, all.inside = F)
  ybincount <- findInterval(df[,2], y.edges, rightmost.closed = T, left.open = F, all.inside = F)
  xbincount <- factor(xbincount, levels=c(1:(length(x.edges)-1)))
  ybincount <- factor(ybincount, levels=c(1:(length(y.edges)-1)))
  
  freq2D <- as.matrix(table(xbincount,ybincount))
  dimnames( freq2D ) <- c()
  rownames( freq2D ) <- c()
  colnames( freq2D ) <- c()
  
  return(list('freq2D'=freq2D, 'x.edges'=x.edges, 'y.edges'=y.edges))
  
}


#' @title Create matrix representing 2D density.
#' @param x Vector of X coordinates for density estimation.
#' @param y Vector of Y coordinates for density estimation of the same length
#' as `x`. 
#' @param bw Width of the Gaussian kernel, can be a single number or have two
#' values, to be applied along x and y axes, respectively. In contrast to
#' `density()` there is no good default value (set to 1 for now).
#' @param weights Optional weights for each point, same length as `x` and `y`.
#' @param n Number of points at which to get density estimates, can be a single
#' integer, or two, to be applied along the x and y axes respectively.
#' @param from Start of the interval in which to do density estimation, can be
#' a single value, or two, to be applied along the x and y axes respectively.
#' @param to End of the interval in which to do density estimation, can be a
#' single value, or two, to be applied along the x and y axes respectively.
#' @param cut Used if `from` and `to` are not given: density is estimated for
#' the minimum and maximum of the `x` of the `y` vectors plus/minus cut*bw. 
#' @param na.rm Boolean: remove NA values or not.
#' @return 
#' The functions returns a list with 3 entries:
#'     x: x coordinates of density estimates
#'     y: y coordinates of density estimates
#'     z: a matrix of size (x,y) with density estimates
#' These can be used directly for input into `contour()`, `filled.contour()`,
#' `image()` and others (see `lattice` package).
#' @description 
#' For now, this function only allows a Gaussian kernel for 2D density estimates.
#' This is probably most useful in making figures describing data with too many
#' coordinates to put in a single scatter diagram (`graphics::points()`).
#' @details 
#' ?
#' @examples
#' x <- c(rnorm(n=100, mean=5), rnorm(n=200, mean=3, sd=2))
#' y <- c(rnorm(n=100, mean=2), rnorm(n=200, mean=6, sd=2))
#' dens2d <- density2D(x=x, y=y, bw=0.5)
#' contour(x=dens2d$x, y=dens2d$y, z=dens2d$z)
#' points(x,y,col='red')
#' @export
density2D <- function(x, y, bw=1, weights=NULL, n=100, from=NULL, to=NULL, cut=3, na.rm=FALSE) {
  
  # have weights:
  if (!is.null(weights)) {
    if (length(x) != length(weights)) {
      #cat('density2d error: weights need to be the same length as x and y\n')
      return()
    }
    weights <- weights / sum(weights)
  } else {
    weights <- rep(1/length(x), length(x))
  }
  
  # remove NA values, if specified:
  if (na.rm) {
    idx <- intersect( which(!is.na(x)), which(!is.na(y)) )
    x <- x[idx]
    y <- y[idx]
    weights <- weights[idx]
  }
  
  # make sure we have bandwidth in the correct shape:
  if (length(bw) == 1) {
    bw <- c(bw,bw)
  }
  
  # set up the coordinates for the grid:
  if (is.null(from) | is.null(to)) {
    from <- c(min(x)-(cut*bw[1]),min(y)-(cut*bw[2]))
    to   <- c(max(x)+(cut*bw[1]),max(y)+(cut*bw[2]))
  }
  if (length(n) == 1) {
    n = c(n,n)
  }
  
  # make the actual grid:
  X <- seq(from[1], to[1], length.out=n[1])
  Y <- seq(from[2], to[2], length.out=n[2])
  grid <- expand.grid('x'=X,'y'=Y,'z'=NA)
  
  
  # get density at the grid points:
  for (idx in c(1:dim(grid)[1])) {
    # the most horrible line of code:
    grid$z[idx] <- sum( weights * ( 1/(2*pi*bw[1]+bw[2]) * exp( -1 * ((x-grid$x[idx])^2/(2*(bw[1]^2)) + (y-grid$y[idx])^2/(2*(bw[2]^2)))) ) )
  }
  
  return( list('x'=X,
               'y'=Y,
               'z'=matrix(grid$z, nrow=length(X)) ) )
  
}
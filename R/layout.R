#' @title Calculate coordinates for guides on a poster
#' @param size The size (usually width) of the poster
#' @param outermargins The width of the outer margins
#' @param innermargins The width of the margins in between columns
#' @param ncolumns The number of columns that should appear on the poster
#' @return a vector of coordinates for guides
#' @description Given the size (width) of a poster, the size of the inner and
#' outer margins and the number of columns, this function returns the location
#' of guides. 
#' 
#' All arguments except `ncolumns` should be in the same unit.
#' @examples # the default values use the width of a landscape 'letter' in mm
#' # with outer margins of 10 mm, and inner margins of 5 mm
#' # and 3 columns
#' posterGuides()
#' @export
posterGuides <- function(size          = 279.4,
                         outermargins  = 10,
                         innermargins  = 5,
                         ncolumns      = 3) {
  
  ninnermargins <- max(0,ncolumns-1)
  columnwidth   <- (size - (outermargins * 2) - (innermargins * ninnermargins)) / ncolumns
  
  for (columnno in 1:ncolumns) {
    if (columnno == 1) {
      guides <- c( outermargins )
    } else {
      guides <- c( guides, guides[length(guides)]+innermargins )
    }
    guides <- c( guides, guides[length(guides)]+columnwidth )
  }
  
  return(guides)
  
}

# colors -----

#' @title Mix two colors
#' @param a First color
#' @param b Second color
#' @param balance Numeric vector of length 2, Weights of the two colors
#' @return A weighted average of the two colors in RGB space
#' @description Mixes two colors in RGB space using weights
#' @export
colorMix <- function(a='#41ffc9', b='#1d7791', balance=c(1,1)) {
  
  a <- grDevices::col2rgb(a,alpha=TRUE)/255
  b <- grDevices::col2rgb(b,alpha=TRUE)/255
  
  w <- balance / sum(balance)
  
  R <- (a[1]*w[1]) + (b[1]*w[2])
  G <- (a[2]*w[1]) + (b[2]*w[2])
  B <- (a[3]*w[1]) + (b[3]*w[2])
  A <- (a[4]*w[1]) + (b[4]*w[2])
  
  return(grDevices::rgb(red=R, green=G, blue=B, alpha=A))
  
}

#' @title Change the saturation of a color
#' @param col Color
#' @param sat.mult Multiply saturation with this number
#' @return The input color with the saturation multiplied.
#' @description Converts the input color to HSV space, then multiplies the saturation
#' by the saturation multiplier.
#' @export
colorSaturation <- function(col='#41ffc9', sat.mult=1.25) {
  
  inter <- grDevices::rgb2hsv(grDevices::col2rgb(col))
  
  sat.mult  <- max(0, sat.mult) 
  inter[2,] <- min(1, inter[2,] * sat.mult)
  
  return(grDevices::hsv(inter[1,], inter[2,], inter[3,]))
  
}


## Transparent colors
## Mark Gardener 2015
## www.dataanalytics.org.uk
# t_col <- function(color, percent = 50, name = NULL) {
#   #      color = color name
#   #    percent = % transparency
#   #       name = an optional name for the color
#   
#   ## Get RGB values for named color
#   rgb.val <- col2rgb(color)
#   
#   ## Make new color using input color as base and alpha set by transparency
#   t.col <- rgb(rgb.val[1], rgb.val[2], rgb.val[3],
#                max = 255,
#                alpha = (100 - percent) * 255 / 100,
#                names = name)
#   
#   ## Save the color
#   #invisible(t.col)
#   return(t.col)
# }


#' @title Set or change the alpha (transparency) of a color
#' @param col Color
#' @param alpha Alpha value to apply
#' @return An RGBA color
#' @description Sets the alpha of an RGB(A) color
#' @export
colorAlpha <- function(col, alpha = 34) {
  
  # store the names:
  colornames <- names(col)
  
  # print(colornames)
  
  # get RGB values for named color
  rgb.val <- t(grDevices::col2rgb(col))
  
  # add alpha column:
  # rgb.val <- rbind(rgb.val, rep(alpha, dim(rgb.val)[1]))
  
  ## Make new color using input color as base and alpha set by transparency
  t.col <- grDevices::rgb( rgb.val,
                           alpha         = alpha,
                           maxColorValue = 255)
  
  names(t.col) <- colornames
  
  return(t.col)
  
}

#' @title Get colors that are commonly used in the lab
#' @return A list of two vectors of colors. The list has two elements:
#' `op` and `tr`. `op` contains opaque colors, `tr` contains the same colors but
#' with transparency (alpha = 32).
#' @description This function returns a list of two vectors of colors.
#' The list has two elements:
#' `op` and `tr`. `op` contains opaque colors, `tr` contains the same colors but
#' with transparency (alpha = 32). These colors are commonly used in the lab for
#' figures on posters and in papers. The second color is a red, which is taken 
#' from the York logo. The colors are sorted from most to least bright.
#' @export
getColors <- function() {
  
  cols.op <- c(rgb(255, 147, 41,  255, maxColorValue = 255), # orange:  21, 255, 148
               rgb(229, 22,  54,  255, maxColorValue = 255), # red:    248, 210, 126
               rgb(207, 0,   216, 255, maxColorValue = 255), # pink:   211, 255, 108
               rgb(127, 0,   216, 255, maxColorValue = 255), # violet: 195, 255, 108
               rgb(0,   19,  136, 255, maxColorValue = 255)) # blue:   164, 255, 68
  
  cols.tr <- c(rgb(255, 147, 41,  32,  maxColorValue = 255), # orange:  21, 255, 148
               rgb(229, 22,  54,  32,  maxColorValue = 255), # red:    248, 210, 126
               rgb(207, 0,   216, 32,  maxColorValue = 255), # pink:   211, 255, 108
               rgb(127, 0,   216, 32,  maxColorValue = 255), # violet: 195, 255, 108
               rgb(0,   19,  136, 32,  maxColorValue = 255)) # blue:   164, 255, 68
  
  cols <- list()
  cols$op <- cols.op
  cols$tr <- cols.tr
  
  return(cols)
  
}

# figure output -----

#' @title Set up a graphics device for saving figures
#' @param target The type of file to create. One of 'pdf', 'svg', 'png', 'tiff'
#' or 'inline'. Default is 'inline', which does not open a file, but leaves use
#' to the default device (e.g. RStudio plot pane).
#' @param width Width of the figure in inches. Default is 8.
#' @param height Height of the figure in inches. Default is 6.
#' @param dpi Dots per inch (resolution) for raster formats (png, tiff). Default is 300.
#' @param filename The name of the file to create, including the path. Required if 
#' target is not 'inline'.
#' @return Does not return a value.
#' @description Boiler plate code to set up a graphics device for saving figures.
#' The function does nothing is the target is not one of the 4 file formats allowed,
#' such that the it can be both used for creating publication quality files and to
#' plot to interactive graphics devices (i.e. on the screen).
#' @importFrom grDevices png pdf tiff rgb
#' @importFrom svglite svglite
#' @export
setupFigureFile <- function(target='inline',width=8,height=6,dpi=300,filename) {
  
  filename <- sprintf('%s.%s', filename, target)
  
  if (target == 'pdf') {
    grDevices::pdf(file    = filename, 
                    width  = width, 
                    height = height)
  }
  if (target == 'svg') {
    svglite::svglite( filename = filename,
                      width = width,
                      height = height,
                      fix_text_size = FALSE) 
    # fix_text_size messes up figures on my machine... 
    # maybe it's better on yours?
  }
  if (target == 'png') {
    grDevices::png(  filename = filename,
                     width = width*dpi,
                     height = height*dpi,
                     res = dpi
    )
  }
  if (target == 'tiff') {
    grDevices::tiff(  filename = filename,
                      compression = 'lzw',
                      width = width*dpi,
                      height = height*dpi,
                      res = dpi
    )
  }
}




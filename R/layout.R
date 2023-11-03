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

`%notin%` <- Negate(`%in%`)

#' @title Expand grid with values from data frame columns
#' @param df A data frame.
#' @param columns A list of column names in the data frame.
#' @return A data frame with all combinations of values existing in the data frame
#' on the specified columns.
#' @description This is similar to expand.grid but uses the existing values in a
#' data frame, on the variables that you name. This would be most useful as a step
#' in preparing data for some other analysis.
#' @details Not yet.
#' @examples
#' #
#' @export
df.col.grid <- function(df, columns) {
  
  # we will use expand.grid in the end
  # this functions can also use a (named) list of factors
  # so we'll create that:
  grid.factors <- list()
  
  # each factor will have the name of the column / variable
  # and it will have all unique values (sorted) that the data frame
  # contains for that variable
  for (col in columns) {
    grid.factors[[col]] <- sort(unique(df[,col]))
  }
  
  # finally we get all combinations of those values,
  # and return that:
  return( expand.grid(grid.factors) )
  
}

#' @title Calculate surface area of 95% confidence ellipse on 2D data.
#' @param df A data frame, with (at least) 2 variables with the same unit.
#' @param vars A list of column to use in the data frame, if `df` has more than 
#' 2 columns.
#' @return The surface area of an ellipse that contains the mean of the data in 95%
#' of replications of the same experiment.
#' @description This function uses PCA to calculate the standard deviation across
#' the two main axes of the spread of 2D data. Multiplying that length-2 vector 
#' by `pi` and `qnorm(0.975)` gives the surface area of an ellipse that will 
#' contain the mean in 95% of cases.
#' This relies on the data being in the same unit, and makes most sense if it is real 
#' data (such as locations in a cartesian coordinate system).
#' @details Not yet.
#' @export
get95CIellipse <- function(df, vars=NULL) {
  
  if (!is.null(vars)) {
    df <- df[, vars]
  }
  
  return(stats::qnorm(0.975) * prod(stats::princomp( df )$sdev) * pi)
  
}

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
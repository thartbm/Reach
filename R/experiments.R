

#' @title Generate random participant IDs
#' @param checkFile CSV file with previous IDs that should not be duplicated
#' @param IDlength Number character pairs to use for IDs (3 is 6 characters)
#' @param number How many IDs to add to the existing list.
#' @param addToFile What CSV file to write the IDs to.
#' @return NULL
#' @description This function generates a set of random IDs for use in 
#' experiments where participant anonymity is important. The IDs are not
#' generated with R's random number generator, but uses openssl.
#' @details The function does not yet check if it is possible to generate
#' as many unique IDs as requested with the given length, so be careful not to
#' start an endless loop. (With 1 bit, you only get 256 combinations in total.)
#' 
#' When opened in spreadsheet software, some IDs could be interpreted as a type
#' of number, but when read in with most kinds of software, they work OK.
#' @examples
#' generateRandomIDs()
#' @importFrom openssl rand_bytes
#' @export
generateRandomIDs <- function(checkFile='randomIDs.csv', IDlength=3, number=100, addToFile=checkFile) {
  # This function generates a NUMBER of random IDs of length IDlength * 2, and 
  # checks if they already exist in file checkFile.
  
  # we use non-reproducible and cryptographically secure sequences
  # from the openssl package
  # library(openssl)
  
  # we need to know which ones we already generated/used
  if (file.exists(checkFile)) {
    existingList <- utils::read.csv(checkFile, stringsAsFactors=F)
  } else {
    existingList <- data.frame('randomIDs'=c())
  }
  
  # we count how many new ones we created
  newIDs <- c()
  
  # we generate new IDs until we have the required number of new, unique ones:
  while(length(newIDs) < number) {
    
    # get a random byte string from openssl:
    my_randcypher <- openssl::rand_bytes(n=IDlength)
    
    # convert to a string:
    my_randid <- ''
    for (idx in c(1:length(my_randcypher))) {
      my_randid <- sprintf('%s%s',my_randid,as.character(my_randcypher[idx]))
    }
    
    # check if it exists, and keep if it is new:
    if (my_randid %in% existingList$randomIDs) {
      # nothing to do here...
    } else {
      # put in list
      newIDs <- c(newIDs,my_randid)
    }
    
  }
  
  # convert list to data frame
  newList <- data.frame('randomIDs'=newIDs)
  
  # check if target file exists, read contents if true
  if (file.exists(addToFile)) {
    existingList <- utils::read.csv(addToFile, stringsAsFactors=F)
  } else {
    existingList <- data.frame('randomIDs'=c())
  }
  
  # combine contents:
  idList <- rbind(existingList,newList)
  
  # write to file:
  utils::write.csv(idList, file=addToFile, row.names=FALSE, quote=TRUE)
  
}


#' @title Latin Square
#' @param order An integer value of 2 or higher, setting the size of the latin square.
#' @param seed Optional seed for the random number generator
#' @return A matrix of size order x order
#' @description Returns a latin square where each index 1:order occurs in each row
#' as well as in each column.
#' @details 
#' If you want the same latin square repeatedly, set the seed argument.
#' @examples
#' latinSquare(5)
#' @export
latinSquare <- function(order, seed=NULL) {
  
  # order has to be an integer value of at least 2
  # and up to some maximum that the machine can handle
  if (order < 2) {
    cat('order must be 2 or higher, quitting\n')
    return(NA)
  }
  
  # order has to be an integer  
  o <- as.integer(order)
  if (o != order) {
    cat(sprintf('order must be an integer, rounding %0.8f to %d\n', order, o))
  }
  
  # make an empty matrix to hold the latin square:
  # it should have order rows and order columns
  lsq <- matrix(data=NA, nrow = o, ncol = o)
  
  # first row will be filled with the numbers 1 through to order
  onerow <- c(1:o)
  # we make sure each row and column is unique
  for (rown in c(1:o)) {
    lsq[rown,] <- onerow
    onerow <- c(onerow[2:o], onerow[1])
  }
  
  # now we shuffle, using the RNG seed, if provided:
  if (!is.null(seed)) {
    set.seed(seed)
  }
  
  # first shuffle the rows:
  lsq <- lsq[sample(c(1:o)),]
  # then shuffle columns:
  lsq <- lsq[,sample(c(1:o))]
  
  return(lsq)
  
}

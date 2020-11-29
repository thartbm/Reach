
# work on:
# Baseline/copyOSFdata.py

syncData <- function(groups='all', sections='all', overwrite=TRUE) {
  
  fileURLs <- read.csv('data/urls.csv', stringsAsFactors = F)
  
  if (groups[1] != 'all') {
    fileURLs <- fileURLs[which(fileURLs$group %in% groups),]
  }
  
  if (sections[1] != 'all') {
    fileURLs <- fileURLs[which(fileURLs$section %in% sections),]
  }
  
  print(fileURLs$URL)
    
}

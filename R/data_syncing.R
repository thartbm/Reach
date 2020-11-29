
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
  
  for (fileno in c(1:dim(fileURLs)[1])) {
    
    filename <- sprintf('%s_%s.zip',fileURLs$group[fileno],fileURLs$section[fileno])
    folderfilename <- sprintf('data/raw/zip/%s',filename)
    
    if (overwrite | !file.exists(folderfilename)) {
      
      url = as.character(fileURLs$URL[fileno])
      
      cat(sprintf("Downloading: '%s' from '%s'\n", folderfilename, url))
      
      download.file(url = url, 
                    destfile = folderfilename, 
                    method = 'auto', 
                    quiet = FALSE, 
                    mode = "wb")
      
    } else {
      
      cat(sprintf('"%s" already present\n',folderfilename))
      
    }
    
    unzip(zipfile = folderfilename, exdir = 'data/raw/')
    
  }
    
}

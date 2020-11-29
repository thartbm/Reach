
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
  
  checkFiles(groups=groups, sections=sections)
  
}

checkFiles <- function(groups='all', sections='sections', verbose=F) {
  
  pfiles <- read.csv('data/files.csv', stringsAsFactors = F)
  fileURLs <- read.csv('data/urls.csv', stringsAsFactors = F)
  
  if (groups[1] != 'all') {
    fileURLs <- fileURLs[which(fileURLs$group %in% groups),]
  }
  
  if (sections[1] != 'all') {
    fileURLs <- fileURLs[which(fileURLs$section %in% sections),]
  }
  
  
  sectionfiles <- list('aligned'=c('aligned_training',
                                   'aligned_nocursor',
                                   'aligned_passivelocalization',
                                   'aligned_activelocalization'),
                       'rotated'=c('rotated_training',
                                   'rotated_nocursor',
                                   'rotated_passivelocalization',
                                   'rotated_activelocalization'),
                       'localization-reaches'=c('rotated_passivereach',
                                                'rotated_activereach',
                                                'aligned_passivereach',
                                                'aligned_activereach')
  )
  
  Nmissing <- 0
  
  for (rown in c(1:dim(fileURLs)[1])) {
    
    group <- fileURLs$group[rown]
    section <- fileURLs$section[rown]
  
    filetypes <- sectionfiles[[section]]
    
    participants <- pfiles$participant[which(pfiles$group == group)]
    
    for (filetype in filetypes) {
      
      for (participant in participants) {
        
        prow <- which(pfiles$participant == participant)
        
        pfile <- pfiles[prow,filetype]
        
        if (nchar(pfile) > 0) {
          
          # this file *should* be there:
          filename <- sprintf('data/raw/%s/%s/%s',group,participant,pfile)
          
          if (!file.exists(filename)) {
            if (verbose) { cat(sprintf('WARNING: missing file? %s\n',filename)) }
            Nmissing <- Nmissing + 1
          }
          
        }
        
      }
      
    }
    
  }
  
  if (Nmissing) {
    cat(sprintf('WARNING: %d missing files!\n', Nmissing))
  }
  
}


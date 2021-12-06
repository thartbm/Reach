
# work on:
# Baseline/copyOSFdata.py


#' Download data from OSF and unzip into folder structure in \code{data/} folder.
#' 
#' @param groups vector of group names for which to download data (default: 'all')
#' @param sections vector of task sections for which to download data (default: 'all')
#' @param repository OSF repository to download from
#' @return empty
#' @examples
#' #dataDownload(groups=c('control', 'instructed'), sections=c('aligned'))
#' @export
downloadData <- function(groups='all', sections='all', overwrite=FALSE) {
  
  utils::data('urls', package='handlocs')
  
  #urls <- handlocs::urls
  
  #fileURLs <- read.csv('data/urls.csv', stringsAsFactors = F)
  
  if (groups[1] != 'all') {
    urls <- urls[which(urls$group %in% groups),]
  }
  
  if (sections[1] != 'all') {
    urls <- urls[which(urls$section %in% sections),]
  }
  
  for (fileno in c(1:dim(urls)[1])) {
    
    filename <- sprintf('%s_%s.zip',urls$group[fileno],urls$section[fileno])
    folderfilename <- sprintf('data/%s',filename)
    
    if (overwrite | !file.exists(folderfilename)) {
      
      url = as.character(urls$URL[fileno])
      
      cat(sprintf("Downloading: '%s' from '%s'\n", folderfilename, url))
      
      utils::download.file(url = url, 
                           destfile = folderfilename, 
                           method = 'auto', 
                           quiet = FALSE, 
                           mode = "wb")
      
    } else {
      
      cat(sprintf('"%s" already present\n',folderfilename))
      
    }
    
    utils::unzip(zipfile = folderfilename, exdir = 'data/')
    
  }
  
  handlocs::dataFilecheck(groups=groups, sections=sections)
  
}

#' Check if csv files from requested groups and sections exist
#' 
#' @param groups vector of group names for which to download data (default: 'all')
#' @param sections vector of task sections for which to download data (default: 'all')
#' @param verbose boolean, if \code{TRUE}: print names of all missing files
#' @return empty
#' @examples
#' #dataFilecheck(groups='all', sections=c('aligned'), verbose=TRUE)
#' @export
dataFilecheck <- function(groups='all', sections='sections', verbose=FALSE) {
  
  utils::data('files', package='handlocs')
  utils::data('urls', package='handlocs')
  
  #files <- handlocs:::files
  #urls <- handlocs:::urls

  if (groups[1] != 'all') {
    urls <- urls[which(urls$group %in% groups),]
  }
  
  if (sections[1] != 'all') {
    urls <- urls[which(urls$section %in% sections),]
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
  
  for (rown in c(1:dim(urls)[1])) {
    
    group <- urls$group[rown]
    section <- urls$section[rown]
  
    filetypes <- sectionfiles[[section]]
    
    participants <- files$participant[which(files$group == group)]
    
    for (filetype in filetypes) {
      
      for (participant in participants) {
        
        prow <- which(files$participant == participant)
        
        pfile <- files[prow,filetype]
        
        if (nchar(pfile) > 0) {
          
          # this file *should* be there:
          filename <- sprintf('data/%s/%s/%s',group,participant,pfile)
          
          if (!file.exists(filename)) {
            if (verbose) { cat(sprintf('WARNING: missing file? %s\n',filename)) }
            Nmissing <- Nmissing + 1
          }
          
        } # check existence of files that should be there
        
      } # end loop through participants
      
    } # end loop through filetypes
    
  } # end loop through section files
  
  if (Nmissing) {
    cat(sprintf('WARNING: %d missing files! (set verbose=T to see which)\n', Nmissing))
  }
  
}


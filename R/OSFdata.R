
#' Download data from OSF and unzip into folder structure in \code{data/} folder.
#' 
#' @param repository OSF repository to download from (string of 5 characters)
#' @param filelist named list where names are folders, and entries are vectors
#' of filenames to download from those folders on the OSF repository
#' @param overwrite (Boolean) Whether files should be overwritten if already there.
#' @param folder Folder in the current working directory to store files in.
#' @param unzip (Boolean) Whether or not to unzip zip files.
#' @param removezips (Boolean) Whether or not to remove zip files after unzipping.
#' @param wait (Numeric) Number of seconds to wait in between downloads.
#' @return empty
#' @import osfr
#' @export
downloadOSFdata <- function(repository,filelist,folder,overwrite=TRUE,unzip=FALSE,removezips=FALSE,wait=40) {
  
  # get the 5-character repository name:
  slash_idx <- as.numeric(gregexpr(pattern ='/',repository)[[1]])
  if (rev(slash_idx)[1] == nchar(repository)) {
    repository <- substr(repository,1,rev(slash_idx)[1]-1)
    slash_idx <- as.numeric(gregexpr(pattern ='/',repository)[[1]])
    if (length(slash_idx)>0) {
      repository <- substr(repository,rev(slash_idx)[1]+1,nchar(repository))
    }
  }
  # connect to the repository:
  mainOSFnode <- osfr::osf_retrieve_node(repository)
  
  
  if (overwrite) {
    conflict <- 'overwrite'
  } else {
    conflict <- 'skip'
  }
  
  # loop through entries in filelist
  for (folderno in c(1:length(names(filelist)))) {
    
    foldername <- names(filelist)[folderno]
    
    # foldername needs to have a trailing back slash:
    if (substr(foldername,nchar(foldername),nchar(foldername)) != "\\" ) {
      foldername <- sprintf('%s\\',foldername)
    }
    
    # list files in the OSF folder:
    files <- osfr::osf_ls_files(mainOSFnode, path=foldername, n_max=500)
    
    cat('files on OSF:\n')
    print(files)
    
    filenames <- filelist[[names(filelist)[folderno]]]
    
    cat('files to download:\n')
    print(filenames)
    
    for (filename in filenames) {
      
      cat(sprintf('making sure we have: %s\n',filename))
      
      # find which line corresponds to the file:
      idx <- which(files$name == filename)
      
      # check that the file exists on OSF, and is unique:
      # if not: skip to next file
      if (length(idx) != 1) {
        next
      }
      
      # download the file:
      if (!file.exists(sprintf('data/%s',files$name[idx])) | overwrite) {
        osfr::osf_download(x = files[idx,], 
                           path = sprintf('%s', folder), 
                           conflicts = conflict)
      }
      
      Sys.sleep(wait)
      
    }
    
  }
  
  if (unzip) {
    Reach::unzipZips(filelist=filelist,
                        folder=folder,
                        removezips=removezips)
  }
  

}

#' Download data from OSF and unzip into folder structure in \code{data/} folder.
#' 
#' @param filelist named list where names are folders, and entries are vectors
#' of filenames to download from those folders on the OSF repository
#' @param folder folder in the current working directory to store files in
#' @param removezips (Boolean) remove zip files after unzipping?
#' @return empty
#' @export
unzipZips <- function(filelist,folder,removezips=FALSE) {
  
  # loop through entries in filelist
  for (folderno in c(1:length(names(filelist)))) {
    
    foldername <- names(filelist)[folderno]
    
    # foldername needs to have a trailing back slash:
    if (substr(foldername,nchar(foldername),nchar(foldername)) != "\\" ) {
      foldername <- sprintf('%s\\',foldername)
    }
    
    filenames <- filelist[[names(filelist)[folderno]]]
    
    for (filename in filenames) {
      
      # check if it is a zip file:
      ext <- tools::file_ext(filename)
      if (ext == 'zip' & file.exists(sprintf('%s/%s',folder,filename))) {
        utils::unzip(zipfile=sprintf('%s/%s',folder,filename),
                     exdir=folder)
        if (removezips & file.exists(sprintf('%s/%s',folder,filename))) {
          file.remove(sprintf('%s/%s',folder,filename))
        }
        
      }

    }
    
  }
  
}


#' #' Check if csv files from requested groups and sections exist
#' #' 
#' #' @param groups vector of group names for which to download data (default: 'all')
#' #' @param sections vector of task sections for which to download data (default: 'all')
#' #' @param verbose boolean, if \code{TRUE}: print names of all missing files
#' #' @return empty
#' #' @examples
#' #' #dataFilecheck(groups='all', sections=c('aligned'), verbose=TRUE)
#' dataFilecheck <- function(groups='all', sections='sections', verbose=FALSE) {
#'   
#'   utils::data('files', package='handlocs')
#'   utils::data('urls', package='handlocs')
#'   
#'   if (groups[1] != 'all') {
#'     urls <- urls[which(urls$group %in% groups),]
#'   }
#'   
#'   if (sections[1] != 'all') {
#'     urls <- urls[which(urls$section %in% sections),]
#'   }
#'   
#'   
#'   sectionfiles <- list('aligned'=c('aligned_training',
#'                                    'aligned_nocursor',
#'                                    'aligned_passivelocalization',
#'                                    'aligned_activelocalization'),
#'                        'rotated'=c('rotated_training',
#'                                    'rotated_nocursor',
#'                                    'rotated_passivelocalization',
#'                                    'rotated_activelocalization'),
#'                        'localization-reaches'=c('rotated_passivereach',
#'                                                 'rotated_activereach',
#'                                                 'aligned_passivereach',
#'                                                 'aligned_activereach')
#'   )
#'   
#'   Nmissing <- 0
#'   
#'   for (rown in c(1:dim(urls)[1])) {
#'     
#'     group <- urls$group[rown]
#'     section <- urls$section[rown]
#'   
#'     filetypes <- sectionfiles[[section]]
#'     
#'     participants <- files$participant[which(files$group == group)]
#'     
#'     for (filetype in filetypes) {
#'       
#'       for (participant in participants) {
#'         
#'         prow <- which(files$participant == participant)
#'         
#'         pfile <- files[prow,filetype]
#'         
#'         if (nchar(pfile) > 0) {
#'           
#'           # this file *should* be there:
#'           filename <- sprintf('data/%s/%s/%s',group,participant,pfile)
#'           
#'           if (!file.exists(filename)) {
#'             if (verbose) { cat(sprintf('WARNING: missing file? %s\n',filename)) }
#'             Nmissing <- Nmissing + 1
#'           }
#'           
#'         } # check existence of files that should be there
#'         
#'       } # end loop through participants
#'       
#'     } # end loop through filetypes
#'     
#'   } # end loop through section files
#'   
#'   if (Nmissing) {
#'     cat(sprintf('WARNING: %d missing files! (set verbose=T to see which)\n', Nmissing))
#'   }
#'   
#' }


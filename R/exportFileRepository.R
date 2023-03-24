#' @name exportFileRepository
#' @title Export All Files From a Directory in the File Repository
#' 
#' @description Exports all of the files in a directory in the File 
#'   Repository. This is an extension of \code{exportFromFileRepository}, 
#'   which downloads a single file and may be applied recursively to download
#'   files in subdirectories as well.
#'   
#' @param rcon A \code{redcapConnection} object.
#' @param folder_id \code{integerish(0/1)} The folder ID with the files to 
#'   download. If length 0, defaults to the top-level directory.
#' @param dir \code{character(1)}. A directory on the local system to which 
#'   the file is to be saved. Defaults to the working directory.
#' @param dir_create \code{logical(1)}. Create the directory \code{dir} if it 
#'   does not already exist. Defaults to \code{FALSE}. If \code{dir} does 
#'   not exist and \code{dir_create = FALSE}, an error is thrown.
#' @param recursive \code{logical(1)}. If \code{TRUE}, export all subfolders 
#'   and their files as well
#' @param ... Additional arguments to be passed between methods
#' @param error_handling An option for how to handle errors returned by the API.
#'   see \code{\link{redcap_error}}
#' @param config \code{list} Additional configuration parameters to pass to 
#'   \code{\link[httr]{POST}}. These are appended to any parameters in 
#'   \code{rcon$config}.
#' @param api_param \code{list} Additional API parameters to pass into the
#'   body of the API call. This provides users to execute calls with options
#'   that may not otherwise be supported by \code{redcapAPI}.
#'   
#' @return 
#' Returns a data frame listing the files that were downloaded and the 
#'   directories in which they are saved.
#'   
#' @author Benjamin Nutter
#' 
#' @export

exportFileRepository <- function(rcon, 
                                 folder_id, 
                                 dir = getwd(), 
                                 dir_create = FALSE, 
                                 recursive = FALSE, 
                                 ...){
  UseMethod("exportFileRepository")
}

#' @rdname exportFileRepository
#' @export

exportFileRepository.redcapApiConnection <- function(rcon, 
                                                     folder_id = numeric(0), 
                                                     dir = getwd(), 
                                                     dir_create = FALSE, 
                                                     recursive = FALSE, 
                                                     ...,
                                                     error_handling = getOption("redcap_error_handling"),
                                                     config = list(), 
                                                     api_param = list()){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::assert_integerish(x = folder_id, 
                               max.len = 1, 
                               any.missing = FALSE,
                               add = coll)
  
  checkmate::assert_character(x = dir, 
                              len = 1, 
                              add = coll)
  
  checkmate::assert_logical(x = dir_create, 
                            len = 1, 
                            add = coll)
  
  checkmate::assert_logical(x = recursive, 
                            len = 1, 
                            add = coll)
  
  error_handling <- checkmate::matchArg(x = error_handling,
                                        choices = c("null", "error"),
                                        add = coll)
  
  checkmate::assert_list(x = config, 
                         names = "named", 
                         add = coll)
  
  checkmate::assert_list(x = api_param, 
                         names = "named", 
                         add = coll)
  
  checkmate::reportAssertions(coll)
  
  if (!dir_create){
    checkmate::assert_directory_exists(x = dir, 
                                       add = coll)
    checkmate::reportAssertions(coll)
  } 
  
  # Create the directory if it doesn't exist ------------------------
  
  if (dir_create && !file.exists(dir)) {
    dir.create(dir, 
               recursive = TRUE)
  }
  
  # Fetch the files to download -------------------------------------
  
  FileRepo <- exportFileRepositoryListing(rcon, 
                                          folder_id = folder_id, 
                                          recursive = recursive)
  
  if (nrow(FileRepo) == 0){
    message("No files or folders to download")
    return(FILE_IMPORT_EXPORT_EMPTY_FRAME)
  }
  
  # Export the File Repository --------------------------------------
  
  ExportedFiles <- vector("list", nrow(FileRepo))
  n_folder_created <- 0
  
  for (i in seq_len(nrow(FileRepo))){
    # Export a file
    if (!is.na(FileRepo$doc_id)[i]){
      this_doc <- FileRepo$doc_id[i]
      filepath <- fileRepositoryPath(doc_id = this_doc, 
                                     fileRepo = FileRepo)
      this_dir <- dirname(filepath)
      
      .exportFileRepository_createLocalDirectory(dir, this_dir)
      
      ExportedFiles[[i]] <- exportFromFileRepository(rcon, 
                                                     doc_id = this_doc, 
                                                     dir = file.path(dir, this_dir), 
                                                     dir_create = TRUE)
    } else {
      this_folder <- FileRepo$folder_id[i]
      this_dir <- fileRepositoryPath(folder_id = this_folder,
                                     fileRepo = FileRepo)
      
      .exportFileRepository_createLocalDirectory(dir, this_dir)
      
      n_folder_created <- n_folder_created + 1
      ExportedFiles[[i]] <- data.frame(directory = file.path(dir, this_dir), 
                                       filename = NA_character_, 
                                       stringsAsFactors = FALSE)
    } # end if/else
  } # end for
  
  message(sprintf("%s folders have been created", 
                  n_folder_created))
  
  do.call("rbind", ExportedFiles)
} 


# Unexported --------------------------------------------------------

.exportFileRepository_createLocalDirectory <- function(dir, subdir){
  dir_to_create <- file.path(dir, subdir)
  if (!file.exists(dir_to_create)){
    dir.create(dir_to_create, 
               recursive = TRUE)
  }
}

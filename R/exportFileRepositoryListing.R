#' @name exportFileRepositoryListing
#' @title Export a Listing of Folders and Files in the File Repository 
#' 
#' @description This method enables the user to export a list of folders
#'   and files saved to the File Repository. The listing may optionally 
#'   include contents of subfolders.
#'   
#' @inheritParams common-rcon-arg
#' @inheritParams common-dot-args
#' @inheritParams common-api-args 
#' @param folder_id `integerish(0/1)`. The 
#'   folder ID of a specific folder in the File Repository for which a list of 
#'   files and subfolders will be exported. 
#'   By default, the top-level directory of the File Repository will be used.
#' @param recursive `logical(1)`. When `TRUE`, content of subfolders
#'   will be retrieved until a full listing is produced. If `FALSE`, 
#'   only the contents of the requested folder will be returned.
#' 
#' @return 
#' Returns a data frame with the columns
#' 
#' |                 |                                                      |
#' |-----------------|------------------------------------------------------|
#' | `folder_id`     | The REDCap assigned ID value for the folder. Will be `NA` if the item is a file. |
#' | `doc_id`        | The REDCap assigned ID value for the file. Will be `NA` if the item is a folder. |
#' | `name`          | The name of the folder of file.                                                       |
#' | `parent_folder` | The ID of the parent folder of the item. The top-level folder is represented as 0.    |
#' 
#' @seealso
#' [exportFromFileRepository()], \cr
#' [importToFileRepository()], \cr
#' [deleteFromFileRepository()], \cr
#' [exportFileRepository()], \cr
#' [importFileRepository()], \cr
#' [deleteFileRepository()], \cr
#' [createFileRepositoryFolder()]
#' 
#' @examples
#' \dontrun{
#' unlockREDCap(connections = c(rcon = "project_alias"), 
#'              url = "your_redcap_url", 
#'              keyring = "API_KEYs", 
#'              envir = globalenv())
#'              
#' # Export the top-level listing of the File Repository
#' exportFileRepositoryListing(rcon)
#' 
#' # Export the complete listing of the File Repository
#' exportFileRepositoryListing(rcon, 
#'                             recursive = TRUE)
#'                             
#' # Export the listing of a subfolder in the File Repository
#' exportFileRepositoryListing(rcon, 
#'                             folder_id = 12345)
#' }
#' 
#' @export

exportFileRepositoryListing <- function(rcon, 
                                        folder_id = numeric(0), 
                                        recursive = FALSE, 
                                        ...){
  UseMethod("exportFileRepositoryListing")
}

#' @rdname exportFileRepositoryListing
#' @export

exportFileRepositoryListing.redcapApiConnection <- function(rcon, 
                                                            folder_id = numeric(0), 
                                                            recursive = FALSE, 
                                                            ...,
                                                            error_handling = getOption("redcap_error_handling"),
                                                            config = list(), 
                                                            api_param = list()){
  # Argument validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::assert_integerish(x = folder_id, 
                               max.len = 1, 
                               any.missing = FALSE,
                               add = coll)
  
  checkmate::assert_logical(x = recursive, 
                            len = 1, 
                            add = coll)
  
  error_handling <- checkmate::matchArg(x = error_handling,
                                        choices = c("null", "error"),
                                        .var.name = "error_handling",
                                        add = coll)
  
  checkmate::assert_list(x = config, 
                         names = "named", 
                         add = coll)
  
  checkmate::assert_list(x = api_param, 
                         names = "named", 
                         add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Make Body List --------------------------------------------------
  
  body <- list(content = 'fileRepository', 
               action = 'list', 
               format = 'csv', 
               returnFormat = 'csv', 
               folder_id = folder_id)
  
  body <- body[lengths(body) > 0]
  
  # Call the API ----------------------------------------------------
  
  response <- makeApiCall(rcon, 
                          body = c(body, api_param), 
                          config = config)
  
  if (response$status_code != 200){
    redcapError(response, 
                 error_handling = error_handling)
  } 
  
  # Convert result to a data frame ----------------------------------
  FileRepository <- .fileRepositoryFrame(response, 
                                         folder_id)
  
  # Recursive Call --------------------------------------------------
  
  if(recursive) {
    FileRepository <- .fileRepositoryRecursive(FileRepository, 
                                               rcon = rcon)
  }
  
  FileRepository
}


# Unexported --------------------------------------------------------

.fileRepositoryFrame <- function(response, 
                                 folder_id){
  # If folder_id has length 0, set the parent to top-level
  parent <- if (length(folder_id) == 0) 0 else folder_id
  
  
  response <- as.character(response)
  if (nchar(response) > 0){
    response <- utils::read.csv(text = response, 
                                stringsAsFactors = FALSE, 
                                na.strings = "")
    response$parent_folder <- rep(parent, 
                                  nrow(response))
  } else {
    response <- FILE_REPOSITORY_EMPTY_FRAME # defined in constants.R
  }
  response
}

.fileRepositoryRecursive <- function(FileRepository, rcon){
  # Get folder IDs
  fids <- FileRepository$folder_id  
  fids <- fids[!is.na(fids)]
  
  # Recursively call to the API for any non-missing folder_id
  if(length(fids) > 0) {
    addl <- do.call(rbind, 
                    lapply(fids, 
                           FUN = exportFileRepositoryListing, 
                           rcon = rcon, 
                           recursive = TRUE))
    FileRepository <- rbind(FileRepository, addl)
  }
  
  FileRepository
}

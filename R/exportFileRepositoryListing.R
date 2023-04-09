#' @export exportFileRepositoryListing
#' @title Export a Listing of Folders and Files in the File Repository 
#' 
#' @description Exports a list of folders and files saved to the file 
#'   repository. Includes an option to explore folders recursively.
#'   
#' @param rcon a \code{redcapConnection} object. 
#' @param folder_id \code{integerish} with maximum length 1. the 
#'   folder ID of a specific folder in the File Repository for which you 
#'   wish to export a list of its files and sub-folders. 
#'   By default, the top-level directory of the File Repository will be used.
#' @param recursive \code{logical(1)}. When \code{TRUE}, content of subfolders
#'   will be retrieved until a full listing is produced. If \code{FALSE}, 
#'   only the contents of the requested folder will be returned.
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
#' @details This method allows you to export a list of all files and 
#'   sub-folders from a specific folder in a project's File Repository. 
#'   Each sub-folder will have an associated folder_id number, and each 
#'   file will have an associated doc_id number.
#' 
#' @author Cole Beck
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
    redcap_error(response, 
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
    response <- read.csv(text = response, 
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

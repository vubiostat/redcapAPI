#' @name deleteFileRepository
#' @title Delete All Files from the File Repository
#' 
#' @description Deletes all files in a folder of the file repository. The
#'   deletion may be applied recursively to sub-folders. Deleted files 
#'   will remain in the recycling bin for up to 30 days. This function 
#'   does not provide a way to delete folders.
#'   
#' @param rcon A \code{redcapConnection} object. 
#' @param folder_id \code{integerish(1)}. The folder ID from which files
#'   should be deleted. Use \code{folder_id = 0} to delete from the top-level
#'   folder (this is different from other File Repository methods, which 
#'   default to the top-level folder. The difference here is to prevent 
#'   deleting an entire File Repository using a default argument.)
#' @param recursive \code{logical(1)}. If \code{TRUE}, files in all subfolders
#'   will be deleted as well. 
#' @param refresh \code{logical(1)}. If \code{TRUE} and the file repository
#' @param ... Arguments to pass to other methods.
#'   File Repository data on \code{rcon} will be refreshed. 
#' @param confirm \code{confirm} \code{character}. One of \code{c("ask", "no", "yes")}. 
#'   When \code{"ask"}, user will be prompted to confirm the deletion. 
#'   When \code{"no"}, the function will terminate with no action. When 
#'   \code{"yes"}, the function will proceed without confirmation (useful
#'   for automated processes).
#' @param error_handling An option for how to handle errors returned by the API.
#'   see \code{\link{redcap_error}}
#' @param config \code{list} Additional configuration parameters to pass to 
#'   \code{\link[httr]{POST}}. These are appended to any parameters in 
#'   \code{rcon$config}.
#' @param api_param \code{list} Additional API parameters to pass into the
#'   body of the API call. This provides users to execute calls with options
#'   that may not otherwise be supported by \code{redcapAPI}.
#'   
#' @author Benjamin Nutter
#' 
#' @export

deleteFileRepository <- function(rcon, 
                                 folder_id, 
                                 recursive = FALSE, 
                                 ...){
  UseMethod("deleteFileRepository")
}

#' @rdname deleteFileRepository
#' @export

deleteFileRepository.redcapApiConnection <- function(rcon, 
                                                     folder_id, 
                                                     recursive      = FALSE,
                                                     refresh        = TRUE,
                                                     ..., 
                                                     confirm        = c("ask", "no", "yes"),
                                                     error_handling = getOption("redcap_error_handling"),
                                                     config         = list(), 
                                                     api_param      = list()){
  # Argument Validation ---------------------------------------------
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::assert_integerish(x = folder_id, 
                               len = 1, 
                               any.missing = FALSE,
                               add = coll)
  
  checkmate::assert_logical(x = recursive, 
                            len = 1, 
                            add = coll)
  
  checkmate::assert_logical(x = refresh, 
                            len = 1, 
                            add = coll)
  
  confirm <- checkmate::matchArg(x = confirm, 
                                 choices = c("ask", "no", "yes"), 
                                 add = coll, 
                                 .var.name = "confirm")
  
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
  
  # Determine how many files will be deleted ------------------------
  
  if (folder_id == 0) folder_id <- numeric(0)
  
  ToDelete <- exportFileRepositoryListing(rcon, 
                                          folder_id = folder_id, 
                                          recursive = recursive)
  
  files_to_delete <- sum(!is.na(ToDelete$doc_id))
  
  if (files_to_delete == 0){
    message("No files to delete in the requested folder(s)")
    return(FILE_REPOSITORY_EMPTY_FRAME)
  }
  
  # Get confirmation ------------------------------------------------
  
  if (confirm == "ask"){
    confirm <- 
      readline(prompt = "Type 'yes' to confirm the delete action.   ")
    
    confirm = trimws(confirm)
    
    if (tolower(confirm) != "yes") confirm <- "no"
  }
  
  if (confirm == "no"){
    message("Delete action cancelled by user.")
    return(FILE_REPOSITORY_EMPTY_FRAME)
  }
  
  # Delete the files ------------------------------------------------
  
  ToDelete <- ToDelete[!is.na(ToDelete$doc_id), ]
  
  for (i in seq_len(nrow(ToDelete))){
    deleteFromFileRepository(rcon, 
                             doc_id = ToDelete$doc_id[i], 
                             refresh = FALSE)
  }
  
  if (refresh && rcon$has_fileRepository()){
    rcon$refresh_fileRepository()
  }
  
  ToDelete
}

#' @name fileRepositoryPath
#' @title Reconstruct the file repository path
#' 
#' @param doc_id \code{integerish(0/1)}. The document ID for which the 
#'   file path should be returned. Only one of \code{doc_id} or 
#'   \code{folder_id} should be specified.
#' @param folder_id \code{integerish(0/1)}. The folder ID for which the 
#'   file path should be returned. Only one of \code{doc_id} or 
#'   \code{folder_id} should be specified.
#' @param fileRepo \code{data.frame} with the file repository listing. 
#'   Typically provided by \code{rcon$fileRepository()}
#'   
#' @author Benjamin Nutter

fileRepositoryPath <- function(doc_id = numeric(0), 
                               folder_id = numeric(0), 
                               fileRepo){
  
  # Argument Validation -----------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_integerish(x = doc_id, 
                               max.len = 1, 
                               add = coll)
  
  checkmate::assert_integerish(x = folder_id, 
                               max.len = 1, 
                               add = coll)
  
  checkmate::reportAssertions(coll)
  
  if (length(doc_id) == 1 && length(folder_id) == 1){
    coll$push("Exactly one of 'doc_id' and 'folder_id' can have length > 0.")
    checkmate::reportAssertions(coll)
  }
  
  # If both have length 0, we assume we are at the top level.
  if (length(doc_id) == 0 && length(folder_id) == 0){
    return("")
  }
  
  # Establish the end of the directory path. 
  # We will start at the deepest part of the path and work our way up 
  # to the top level.
  
  Path <- 
    if (length(doc_id) > 0){
      fileRepo[fileRepo$doc_id %in% doc_id, ]
    } else {
      fileRepo[fileRepo$folder_id %in% folder_id, ]
    }
  
  # The while loop steps up the file directory and stops when it reaches
  # the top-level folder.
  parent_id <- Path$parent_folder
  
  iter <- 0
  
  while(parent_id != 0){
    next_level <- fileRepo[fileRepo$folder_id %in% Path$parent_folder[1], ]
    Path <- rbind(next_level, Path)
    parent_id <- Path$parent_folder[1]
    
    # When the File Repository listing doesn't include the top-level, this
    # loop will never terminate. This provides an escape which terminates if
    # the loop iterates more times than the we have folders in our 
    # Repository listing.
    iter <- iter + 1
    if (iter > nrow(fileRepo)){
      break 
    }
  }

  # construct the file path.
  do.call(file.path, lapply(Path$name, identity))
}
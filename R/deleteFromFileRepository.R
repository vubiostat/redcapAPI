#' @describeIn fromFileRepositoryMethods Delete a file from the file repository.
#' @order 3
#' @export

deleteFromFileRepository <- function(rcon, 
                                     doc_id, 
                                     ...){
  UseMethod("deleteFromFileRepository")
}

#' @rdname fromFileRepositoryMethods
#' @order 6
#' @export

deleteFromFileRepository.redcapApiConnection <- function(rcon, 
                                                         doc_id, 
                                                         ...)
{
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::assert_integerish(x = doc_id, 
                               len = 1, 
                               any.missing = FALSE,
                               add = coll)

  checkmate::reportAssertions(coll)
  
  FileRepo <- rcon$fileRepository()
  
  if (!doc_id %in% FileRepo$doc_id){
    coll$push(sprintf("doc_id (%s) not found in the File Repository", 
                      doc_id))
    checkmate::reportAssertions(coll)
  }
  
  # Get the file path of the file repository file -------------------
  
  file_path <- fileRepositoryPath(doc_id = doc_id, 
                                  fileRepo = FileRepo)
  
  # Build the Body List ---------------------------------------------
  
  body <- list(content = "fileRepository", 
               action = "delete", 
               returnFormat = "csv", 
               doc_id = doc_id)

  # Make the API Call -----------------------------------------------
  rcon$flush_fileRepository()
  makeApiCall(rcon, body, ...)
  
  data.frame(directory = dirname(file_path), 
             filename = basename(file_path), 
             stringsAsFactors = FALSE)
}

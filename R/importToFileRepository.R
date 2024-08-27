#' @describeIn fromFileRepositoryMethods Import a file to the file repository.
#' @order 2
#' @export

importToFileRepository <- function(rcon, 
                                   file, 
                                   folder_id = numeric(0), 
                                   ...){
  UseMethod("importToFileRepository")
}

#' @rdname fromFileRepositoryMethods
#' @order 5
#' @export

importToFileRepository.redcapApiConnection <- function(rcon, 
                                                       file, 
                                                       folder_id = numeric(0),
                                                       ...)
{
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::assert_character(x = file, 
                              len = 1, 
                              add = coll)
  
  checkmate::assert_integerish(x = folder_id, 
                               max.len = 1, 
                               any.missing = FALSE,
                               add = coll)

  checkmate::reportAssertions(coll)
  
  checkmate::assert_file_exists(x = file, 
                                add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Make the Body List ----------------------------------------------
  
  body <- list(content = "fileRepository", 
               action = "import", 
               returnFormat = "csv",
               file = .curlUploadFile(file),
               folder_id = folder_id)

  # flush the cached File Repository ------------------------------
  rcon$flush_fileRepository()
  # Make the API Call -----------------------------------------------
  response <- makeApiCall(rcon, body, ...)
  
  # Get the path of the file in the File Repository -----------------
  fileRepo <- rcon$fileRepository()
  
  file_path <- file.path(fileRepositoryPath(folder_id = folder_id, 
                                            fileRepo = fileRepo))
  
  data.frame(directory = dirname(file_path), 
             filename = basename(file), 
             stringsAsFactors = FALSE)
}

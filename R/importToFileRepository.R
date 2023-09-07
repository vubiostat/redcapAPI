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
                                                       ..., 
                                                       refresh = TRUE, 
                                                       error_handling = getOption("redcap_error_handling"),
                                                       config = list(), 
                                                       api_param = list()){
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
  
  checkmate::assert_logical(x = refresh, 
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
  
  checkmate::assert_file_exists(x = file, 
                                add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Make the Body List ----------------------------------------------
  
  body <- list(content = "fileRepository", 
               action = "import", 
               returnFormat = "csv",
               file = httr::upload_file(file), 
               folder_id = folder_id)
  
  body <- body[lengths(body) > 0]
  
  # Make the API Call -----------------------------------------------
  
  response <- makeApiCall(rcon, 
                          body = c(body, api_param), 
                          config = config)
  
  # Refresh the cached File Repository ------------------------------
  if (refresh && rcon$has_fileRepository()){
    rcon$refresh_fileRepository()
  }
  
  # Get the path of the file in the File Repository -----------------
  fileRepo <- rcon$fileRepository()
  
  file_path <- file.path(fileRepositoryPath(folder_id = folder_id, 
                                            fileRepo = fileRepo))
  message(sprintf("File saved to: %s", file_path))
  
  data.frame(directory = dirname(file_path), 
             filename = basename(file), 
             stringsAsFactors = FALSE)
}

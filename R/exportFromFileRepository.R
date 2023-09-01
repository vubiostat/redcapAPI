#' @describeIn fromFileRepositoryMethods Export a file from the file repository.
#' @order 1
#' @export

exportFromFileRepository <- function(rcon, 
                                     doc_id, 
                                     dir = getwd(), 
                                     dir_create = FALSE, 
                                     ...){
  UseMethod("exportFromFileRepository")
}

#' @rdname fromFileRepositoryMethods
#' @order 4
#' @export

exportFromFileRepository.redcapApiConnection <- function(rcon, 
                                                         doc_id, 
                                                         dir = getwd(), 
                                                         dir_create = FALSE, 
                                                         ...,
                                                         error_handling = getOption("redcap_error_handling"),
                                                         config = list(), 
                                                         api_param = list()){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::assert_integerish(x = doc_id, 
                               len = 1, 
                               any.missing = FALSE,
                               add = coll)
  
  checkmate::assert_character(x = dir, 
                              len = 1, 
                              add = coll)
  
  checkmate::assert_logical(x = dir_create, 
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
  
  if (!dir_create){
    checkmate::assert_directory_exists(x = dir, 
                                       add = coll)
    checkmate::reportAssertions(coll)
    
    # NOTE: Creation of the directory, if needed, is handled by reconstituteFileFromExport
  }
  
  # Make the Body List ----------------------------------------------
  
  body <- list(content = "fileRepository", 
               action = "export", 
               returnFormat = "csv", 
               doc_id = doc_id)
  
  body <- body[lengths(body) > 0]
  
  # Make the API Call -----------------------------------------------

  response <- makeApiCall(rcon, 
                          body = c(body, api_param), 
                          config = config)
  
  if (response$status_code != 200){
    redcapError(response, 
                 error_handling = error_handling)
  } 
  
  ExportedFile <- reconstituteFileFromExport(response = response, 
                                             dir = dir, 
                                             dir_create = dir_create)
  
  message(sprintf("File Saved: %s", 
                  file.path(ExportedFile$directory, 
                            ExportedFile$filename)))
  
  ExportedFile
}

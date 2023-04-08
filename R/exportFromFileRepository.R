#' @name exportFromFileRepository
#' @title Export a File from the File Repository
#' 
#' @description This method allows you to download a single file stored in the 
#'   File Repository by providing the file's \code{doc_id} number. For options
#'   to export multiple files, see \code{exportFileRepository}.
#'   
#' @param rcon \code{redcapApiConnection} object. 
#' @param doc_id \code{integerish(1)}. The document ID to be downloaded
#' @param dir \code{character(1)}. A directory on the local system to which 
#'   the file is to be saved. Defaults to the working directory.
#' @param dir_create \code{logical(1)}. Create the directory \code{dir} 
#'   if it does not already exist. Defaults to FALSE. 
#'   If dir does not exist and create = FALSE, an error is thrown.
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
#' @author Benjamin Nutter
#' 
#' @return Returns a \code{data.frame} with the directory and 
#'   filename of the saved file.
#' 
#' @export

exportFromFileRepository <- function(rcon, 
                                     doc_id, 
                                     dir = getwd(), 
                                     dir_create = FALSE, 
                                     ...){
  UseMethod("exportFromFileRepository")
}

#' @rdname exportFromFileRepository
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
    redcap_error(response, 
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
#' @name deleteFromFileRepository
#' @title Delete a Single File from the File Repository
#' 
#' @description Deletes a single file from the File Respository based on 
#'   its document ID.
#'
#' @param rcon A \code{redcapConnection} object. 
#' @param doc_id \code{integerish(1)} The document ID of the file to be 
#'   deleted. 
#' @param ... Arguments to pass to other methods.
#' @param refresh \code{logical(1)} When \code{TRUE} (default), the cached 
#'   File Repository data on \code{rcon} will be refreshed. 
#' @param error_handling An option for how to handle errors returned by the API.
#'   see \code{\link{redcap_error}}
#' @param config \code{list} Additional configuration parameters to pass to 
#'   \code{\link[httr]{POST}}. These are appended to any parameters in 
#'   \code{rcon$config}.
#' @param api_param \code{list} Additional API parameters to pass into the
#'   body of the API call. This provides users to execute calls with options
#'   that may not otherwise be supported by \code{redcapAPI}.
#'   
#' @details This method allows you to delete a single file in a project's 
#' File Repository. Once deleted, the file will remain in the Recycle Bin 
#' folder for up to 30 days.
#' 
#' @author Benjamin Nutter
#'
#' @export

deleteFromFileRepository <- function(rcon, 
                                     doc_id, 
                                     ...){
  UseMethod("deleteFromFileRepository")
}

#' @rdname deleteFromFileRepository
#' @export

deleteFromFileRepository.redcapApiConnection <- function(rcon, 
                                                         doc_id, 
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
  
  checkmate::assert_integerish(x = doc_id, 
                               len = 1, 
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
  
  body <- body[lengths(body) > 0]
  
  # Make the API Call -----------------------------------------------
  
  response <- makeApiCall(rcon, 
                          body = c(body, api_param), 
                          config = config)
  
  if (response$status_code != 200){
    redcap_error(response, 
                 error_handling = error_handling)
  }
  
  message(sprintf("File deleted: %s", file_path))
  
  # Refresh the cached File Repository ------------------------------
  if (refresh && rcon$has_fileRepository()){
    rcon$refresh_fileRepository()
  }
  
  data.frame(directory = dirname(file_path), 
             filename = basename(file_path), 
             stringsAsFactors = FALSE)
}

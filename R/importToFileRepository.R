#' @name importToFileRepository
#' @title Import a Single File to the File Repository
#' 
#' @description This method allows you to import a single file into a 
#'   project's File Repository. The file may be stored in a specific folder 
#'   in the File Repository if a \code{folder_id} is provided.
#'   
#' @param rcon A redcap connection object.
#' @param file \code{character(1)} A file on the local system to be imported
#'   to the File Repository.
#' @param folder_id \code{integerish(0/1)}. The ID of the folder into which 
#'   the file is to be imported. If length is zero, it is imported to the 
#'   top-level folder.
#' @param ... Additional arguments to be passed between methods
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
#' @author Benjamin Nutter
#' 
#' @export

importToFileRepository <- function(rcon, 
                                   file, 
                                   folder_id = numeric(0), 
                                   ...){
  UseMethod("importToFileRepository")
}

#' @rdname importToFileRepository
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

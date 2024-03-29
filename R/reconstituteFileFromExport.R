#' @name reconstituteFileFromExport
#' @title Save a File to a Local Directory from a Response
#' 
#' @description Converts the file from a `response` object and saves
#'   it to the local file directory. 
#'   
#' @param response An object of class `response`. 
#' @param dir `character(1)` A directory on the local file system into
#'   which the file will be saved. 
#' @param dir_create `logical(1)` If TRUE and the directory does not 
#'   exist, it will be created. Defaults to FALSE. 
#'   If dir does not exist and create = FALSE, an error is thrown.
#' @param file_prefix `character(1)` An optional prefix to prepend to
#'   the file name. This may be desirable to explicitly associate files
#'   with a record and/or event.
#' @param filename `character(0/1)` An optional filename. This is used
#'   in the case where a filename is being provided. It this has length 0, 
#'   the filename will be extracted from the API response.
#'   
#' @seealso
#' [exportFiles()], \cr
#' [exportFromFileRepository()], \cr
#' [exportFileRepository()], \cr
#' [exportPdf()] 
#' 
#' @examples
#' \dontrun{
#' unlockREDCap(connections = c(rcon = "project_alias"), 
#'              url = "your_redcap_url", 
#'              keyring = "API_KEYs", 
#'              envir = globalenv())
#'              
#' response <- makeApiCall(rcon, 
#'                         body = list(content = 'file', 
#'                         action = 'export', 
#'                         record = '1', 
#'                         field = 'file_upload_test', 
#'                         event = 'event_1_arm_1'))
#' reconstituteFileFromExport(response,  
#'                            dir = tempdir())
#' }  
#'   
#' @export

reconstituteFileFromExport <- function(response, 
                                       dir, 
                                       dir_create = FALSE, 
                                       file_prefix = "", 
                                       filename = character(0)){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = response, 
                          classes = "response", 
                          add = coll)
  
  checkmate::assert_character(x = dir, 
                              len = 1, 
                              any.missing = FALSE,
                              add = coll)
  
  checkmate::assert_logical(x = dir_create, 
                            len = 1, 
                            any.missing = FALSE,
                            add = coll)
  
  checkmate::assert_character(x = file_prefix, 
                              len = 1, 
                              any.missing = FALSE,
                              add = coll)
  
  checkmate::assert_character(x = filename, 
                              max.len = 1,
                              any.missing = FALSE,
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  if (!dir_create){
    checkmate::assert_directory_exists(x = dir, 
                                       add = coll)
    checkmate::reportAssertions(coll)
  }
  
  # Create the directory, if necessary ------------------------------
  
  if (!file.exists(dir) && dir_create){
    dir.create(dir)
  }
  
  # Extract the filename --------------------------------------------
  # Only extracted if a filename is not provided.
  if (length(filename) == 0){
    filename <- gsub(pattern = "(^[[:print:]]+; name=|\")", 
                     replacement = "", 
                     x = response$headers$'content-type')
    filename <- sub("[;]charset.+$", "", filename)
  } 
  
  # Add Prefix to the file name -------------------------------------
  
  if (nchar(file_prefix) > 0){
    filename <- sprintf("%s-%s", 
                        file_prefix, 
                        filename)
  }
  
  writeBin(object = as.vector(response$content), 
           con = file.path(dir, filename), 
           useBytes=TRUE)
  
  data.frame(directory = dir, 
             filename = filename)
  
}

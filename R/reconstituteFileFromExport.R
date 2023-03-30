#' @name reconstituteFileFromExport
#' @title Save a File to a Local Directory from a Response
#' 
#' @description Converts the file from a \code{response} object and saves
#'   it to the local file directory. 
#'   
#' @param response An object of class \code{response}. 
#' @param dir \code{character(1)} A directory on the local file system into
#'   which the file will be saved. 
#' @param dir_create \code{logical(1)} If TRUE and the directory does not 
#'   exist, it will be created. Defaults to FALSE. 
#'   If dir does not exist and create = FALSE, an error is thrown.
#' @param file_prefix \code{character(1)} An optional prefix to prepend to
#'   the file name. This may be desirable to explicitly associate files
#'   with a record and/or event.
#'   
#' @author Benjamin Nutter
#' @export

reconstituteFileFromExport <- function(response, 
                                       dir, 
                                       dir_create = FALSE, 
                                       file_prefix = ""){
  # Argument Validation ---------------------------------------------
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = response, 
                          classes = "response", 
                          add = coll)
  
  checkmate::assert_character(x = dir, 
                              len = 1, 
                              add = coll)
  
  checkmate::assert_logical(x = dir_create, 
                            len = 1, 
                            add = coll)
  
  checkmate::assert_character(x = file_prefix, 
                              len = 1, 
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
  filename <- gsub(pattern = "(^[[:print:]]+; name=|\")", 
                   replacement = "", 
                   x = response$headers$'content-type')
  filename <- sub("[;]charset.+$", "", filename)
  
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
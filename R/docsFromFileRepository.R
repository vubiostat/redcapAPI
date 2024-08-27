#' @name fromFileRepositoryMethods
#' @title Export, Import, and Delete Individual Files from the File Repository
#' 
#' @description These methods enable the user to export, import, or delete
#'   individual files from a REDCap project's file repository. 
#'
#' @inheritParams common-rcon-arg
#' @inheritParams common-dot-args
#' @inheritParams common-api-args
#' @param doc_id `integerish(1)`. The document ID to be downloaded.
#' @param folder_id `integerish(0/1)`. The ID of the folder into which 
#'   the file is to be imported. If length is zero, it is imported to the 
#'   top-level folder.
#' @param file `character(1)`. A file on the local system to be imported
#'   to the File Repository.
#' @param dir `character(1)`. A directory on the local system to which 
#'   the file is to be saved. Defaults to the working directory.
#' @param dir_create `logical(1)`. Create the directory `dir` 
#'   if it does not already exist. Defaults to `FALSE`. 
#'   If `dir` does not exist and `create = FALSE`, an error is thrown.
#' 
#' @details When a file is deleted, the file will remain in the Recycle Bin 
#' folder for up to 30 days.
#' 
#' @return 
#' `exportFromFileRepository`, `importToFileRepository`, 
#'   and `deleteFromFileRepository` each return a data frame 
#'   with the columns:
#' |             |                                           |
#' |-------------|-------------------------------------------|
#' | `directory` | The directory in which the file is saved. | 
#' | `filename`  | The name of the saved file.               |
#'
#'   
#' @seealso
#' [exportFileRepository()], \cr
#' [importFileRepository()], \cr
#' [deleteFileRepository()], \cr
#' [exportFileRepositoryListing()], \cr
#' [createFileRepositoryFolder()]
#'   
#' @examples
#' \dontrun{
#' unlockREDCap(connections = c(rcon = "project_alias"), 
#'              url = "your_redcap_url", 
#'              keyring = "API_KEYs", 
#'              envir = globalenv())
#'              
#'  
#' write_to_path <- tempdir()            
#'              
#' # Export a file from the repository
#' exportFromFileRepository(rcon, 
#'                          doc_id = 12345, 
#'                          dir = write_to_path)
#'                          
#' # Export a file and create the target directory if it does not exist
#' exportFromFileRepository(rcon, 
#'                          doc_id = 12345, 
#'                          dir = write_to_path, 
#'                          dir_create = TRUE)
#'                          
#' # Import a file to the top-level directory of the file repository
#' importFileToRepository(rcon, 
#'                        file = "file_to_import.txt")
#'                        
#' # Import a file to a specific directory of the file repository
#' importFileToRepository(rcon, 
#'                        file = "file_to_import.txt", 
#'                        folder_id = 678)
#'                        
#' # Delete a file from the file repository
#' deleteFileFromRepository(rcon, 
#'                          doc_id = 12345)
#' }
#'   
#' @usage NULL
#' @order 0

fromFileRepositoryMethods <- function(rcon, 
                                      doc_id, 
                                      folder_id,
                                      file,
                                      dir, 
                                      dir_create,
                                      ...)
{
  NULL
}

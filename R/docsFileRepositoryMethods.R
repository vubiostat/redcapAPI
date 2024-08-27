#' @name fileRepositoryMethods
#' @title Export, Import, or Delete Multiple Files from the File Repository
#' 
#' @description These methods enable the user to export, import, or delete
#'   entire folders of files from the file repository. These actions 
#'   may be done recursively to include subfolders as well. 
#'   
#' @inheritParams common-rcon-arg
#' @inheritParams common-dot-args
#' @inheritParams common-api-args
#' @param folder_id `integerish(0/1)` The folder ID with the files to 
#'   download. If length 0, defaults to the top-level directory.
#' @param dir `character(1)`. A directory on the local system to which 
#'   the files are to be saved. Defaults to the working directory.
#' @param dir_create `logical(1)`. When `TRUE` create the directory 
#'   `dir` if it does not already exist. Defaults to `FALSE`. 
#'   If `dir` does not exist and `dir_create = FALSE`, an error is thrown.
#' @param recursive `logical(1)`. When `TRUE`, export all subfolders 
#'   and their files as well.
#' @param dag_id `integerish(0/1)` The ID of a data access group. When 
#'   provided, access to the folder will be restricted to the DAG.
#' @param role_id `integerish(0/1)` The ID of a role. When provided, 
#'   access to the folder will be restricted to users with that role.
#' @param confirm `character`. One of `c("ask", "no", "yes")`. 
#'   When `"ask"`, user will be prompted to confirm the deletion. 
#'   When `"no"`, the function will terminate with no action. When 
#'   `"yes"`, the function will proceed without confirmation (useful
#'   for automated processes).
#'
#' @details `deleteFileRepository` will only delete files and cannot
#'   delete folders. 
#'   
#'   Deleted files will remain in the recycling bin for up to 30 days. 
#'   
#' @return 
#' `exportFileRepository` returns a data frame with the locations to which 
#' the files were saved on the local system. It has the columns: 
#' 
#' |             |                                           |
#' |-------------|-------------------------------------------|
#' | `directory` | The directory in which the file is saved. | 
#' | `filename`  |  The name of the saved file.              |
#' 
#' `importFileRepository` returns a data frame with the locations to which 
#' the files were saved on the local system. It has the columns: 
#' 
#' |             |                                           |
#' |-------------|-------------------------------------------|
#' | `directory` | The directory in which the file is saved. | 
#' | `filename`  |  The name of the saved file.              |
#' 
#' `deleteFileRepository` returns a data frame listing the files that 
#'   were deleted from the file repository. It has the columns:
#' 
#' |                |                                                                           |
#' |----------------|---------------------------------------------------------------------------|
#' | `folder_id`    | The REDCap assigned ID number for the folder. This will be `NA` for files.|
#' | `doc_id`       | The REDCap assigned ID number for the file.                               |
#' | `name`         | The filename of the deleted files.                                        |
#' |`parent_folder` | The folder ID of parent folder.                                           |
#'
#' 
#' @seealso 
#' [exportFromFileRepository()], \cr
#' [importToFileRepository()], \cr
#' [deleteFromFileRepository()], \cr
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
#' save_location <- tempdir()
#' 
#' # Export the top-level file repository folder
#' exportFileRepository(rcon, 
#'                      folder_id = numeric(0), 
#'                      dir = save_location)
#'                      
#' # Export the entire repository 
#' exportFileRepository(rcon, 
#'                      folder_id = numeric(0), 
#'                      dir = save_location, 
#'                      recursive = TRUE)
#'                      
#' # Export a file repository folder below the top-level
#' exportFileRepository(rcon, 
#'                      folder_id = 12345, 
#'                      dir = save_location)
#'                      
#' # Import the files from a folder to the top-level file repository
#' importFileRepository(rcon, 
#'                      dir = "path/to/folder")
#'                      
#' # Import the files from a folder to sub folder of the file repository
#' importFileRepository(rcon, 
#'                      dir = "path/to/folder", 
#'                      folder_id = 12345) 
#'                      
#' # Import the files from a folder and assign to a specific 
#' # Data Access Group
#' importFileRepository(rcon, 
#'                      dir = "path/to/folder", 
#'                      dag_id = 789)  
#'                      
#' # Delete files from the top-level folder of the file repository
#' deleteFileRepository(rcon, 
#'                      folder_id = numeric(0))
#'                      
#' # Delete all the file sfrom the file repository
#' deleteFileRepository(rcon, 
#'                      folder_id = numeric(0), 
#'                      recursive = TRUE)                   
#' }
#' 
#' @usage NULL
#' @order 0

fileRepositoryMethods <- function(rcon, 
                                  folder_id, 
                                  dir, 
                                  dir_create, 
                                  dag_id, 
                                  role_id,
                                  recursive,
                                  confirm,
                                  ...)
{
  NULL
}

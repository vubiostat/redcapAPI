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
#' @param folder_id \code{integerish(0/1)} The folder ID with the files to 
#'   download. If length 0, defaults to the top-level directory.
#' @param dir \code{character(1)}. A directory on the local system to which 
#'   the files are to be saved. Defaults to the working directory.
#' @param dir_create \code{logical(1)}. When \code{TRUE} create the directory 
#'   \code{dir} if it does not already exist. Defaults to \code{FALSE}. 
#'   If \code{dir} does not exist and \code{dir_create = FALSE}, an error is thrown.
#' @param recursive \code{logical(1)}. When \code{TRUE}, export all subfolders 
#'   and their files as well.
#' @param refresh \code{logical(1)}. When \code{TRUE} (default), the cached 
#'   File Repository data on \code{rcon} will be refreshed.
#' @param dag_id \code{integerish(0/1)} The ID of a data access group. When 
#'   provided, access to the folder will be restricted to the DAG.
#' @param role_id \code{integerish(0/1)} The ID of a role. When provided, 
#'   access to the folder will be restricted to users with that role.
#' @param confirm \code{character}. One of \code{c("ask", "no", "yes")}. 
#'   When \code{"ask"}, user will be prompted to confirm the deletion. 
#'   When \code{"no"}, the function will terminate with no action. When 
#'   \code{"yes"}, the function will proceed without confirmation (useful
#'   for automated processes).
#'
#' @details \code{deleteFileRepository} will only delete files and cannot
#'   delete folders. 
#'   
#'   Deleted files will remain in the recycling bin for up to 30 days. 
#'   
#' @return 
#' \code{exportFileRepository} and \code{importFileRepository} display messages 
#' giving the directory to which files were saved to the local machine and 
#' File Repository, respectively. They also return a data frame with the columns: 
#' 
#' \tabular{ll}{
#'  \code{directory} \tab The directory in which the file is saved. \cr 
#'  \code{filename} \tab The name of the saved file.
#' }
#' 
#' \code{deleteFileRepository} displays a message giving the files that were 
#' deleted from the File Repository. It also returns a data frame with the 
#' columns:
#' 
#' \tabular{ll}{
#'  \code{folder_id} \tab The REDCap assigned ID number for the folder.
#'      This will be \code{NA} for files. \cr
#'  \code{doc_id} \tab The REDCap assigned ID number for the file. \cr
#'  \code{name} \tab The filename of the deleted files. \cr
#'  \code{parent_folder} \tab The folder ID of parent folder.
#' }
#' 
#' @seealso 
#' \code{\link{exportFromFileRepository}}, \cr
#' \code{\link{importToFileRepository}}, \cr
#' \code{\link{deleteFromFileRepository}}, \cr
#' \code{\link{exportFileRepositoryListing}}, \cr
#' \code{\link{createFileRepositoryFolder}}
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
                                  refresh,
                                  confirm,
                                  ..., 
                                  error_handling, 
                                  config, 
                                  api_param){
  NULL
}
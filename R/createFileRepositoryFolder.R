#' @name createFileRepositoryFolder
#' @title Create a Folder in the File Repository
#'
#' @description This method enables the user to create a folder in the
#'   file repository. The folder created may also be a subfolder of an
#'   existing folder.
#'
#' @inheritParams common-rcon-arg
#' @inheritParams common-dot-args
#' @inheritParams common-api-args
#' @param name `character(1)` The name of the folder. If a folder by
#'   this name already exists, no action will be taken.
#' @param folder_id `integerish(0/1)`. The ID of the parent folder. When
#'   length is 0, the new folder is placed in the top-level.
#' @param dag_id `integerish(0/1)` The ID of a data access group. When
#'   provided, access to the folder will be restricted to the DAG.
#' @param role_id `integerish(0/1)` The ID of a role. When provided,
#'   access to the folder will be restricted to users with that role.
#'
#' @return
#' Returns a data frame with the columns
#'
#' |             |                                                            |
#' |-------------|------------------------------------------------------------|
#' | `folder_id` | The REDCap assigned ID value for the newly created folder. |
#' | `name`      | The name assigned to the folder by the user.               |
#'
#'
#' @seealso
#' [exportFromFileRepository()], \cr
#' [importToFileRepository()], \cr
#' [deleteFromFileRepository()], \cr
#' [exportFileRepository()], \cr
#' [importFileRepository()], \cr
#' [deleteFileRepository()], \cr
#' [exportFileRepositoryListing()]
#'
#' @examples
#' \dontrun{
#' unlockREDCap(connections = c(rcon = "project_alias"),
#'              url = "your_redcap_url",
#'              keyring = "API_KEYs",
#'              envir = globalenv())
#'
#' # Create a folder in the top-level directory
#' createFileRepositoryFolder(rcon,
#'                            name = "New Folder Name")
#'
#' # Create a folder in a subfolder
#' createFileRepositoryFolder(rcon,
#'                            name = "New Folder Name",
#'                            folder_id = 12345)
#'
#' # Create a folder assigned to a Data Access Group
#' createFileRepositoryFolder(rcon,
#'                            name = "New Folder Name",
#'                            dag_id = 678)
#' }
#'
#'
#' @export

createFileRepositoryFolder <- function(rcon,
                                       name,
                                       folder_id = numeric(0),
                                       dag_id = numeric(0),
                                       role_id = numeric(0),
                                       ...){
  UseMethod("createFileRepositoryFolder")
}

#' @rdname createFileRepositoryFolder
#' @export

createFileRepositoryFolder.redcapApiConnection <- function(rcon,
                                                           name,
                                                           folder_id = numeric(0),
                                                           dag_id = numeric(0),
                                                           role_id = numeric(0),
                                                           ...){
  # Argument Validation ---------------------------------------------

  coll <- checkmate::makeAssertCollection()

  checkmate::assert_class(x = rcon,
                          classes = "redcapApiConnection",
                          add = coll)

  checkmate::assert_character(x = name,
                              len = 1,
                              max.chars = 150,
                              add = coll)

  checkmate::assert_integerish(x = folder_id,
                               max.len = 1,
                               add = coll)

  checkmate::assert_integerish(x = dag_id,
                               max.len = 1,
                               add = coll)

  checkmate::assert_integerish(x = role_id,
                               max.len = 1,
                               add = coll)

  checkmate::reportAssertions(coll)

  FileRepo <- rcon$fileRepository()

  if (length(folder_id) > 0 && isTRUE(!folder_id %in% FileRepo$folder_id)){
    coll$push(sprintf("folder_id '%s' does not exist in the File Repository",
                      folder_id))
    checkmate::reportAssertions(coll)
  }

  # Determine if folder already exists ------------------------------

  ref_folder_id <-
    if (length(folder_id) == 0){
      0
    } else {
      folder_id
    }

  RepoRef <- FileRepo[FileRepo$parent_folder == ref_folder_id, ]

  if (nrow(RepoRef) > 0 &&
      name %in% RepoRef$name){

    logMessage(sprintf("A folder named '%s' already exists. No action taken.",
                    name))

    return(data.frame(folder_id = RepoRef$folder_id[RepoRef$name == name],
                      name = name))
  }

  # Build the API Body List -----------------------------------------

  body <- list(content = "fileRepository",
               action = "createFolder",
               format = "csv",
               returnFormat = "csv",
               name = name,
               folder_id = folder_id,
               dag_id = dag_id,
               role_id = role_id)

  # Make the API Call -----------------------------------------------

  response <- makeApiCall(rcon, body, ...)

  # Refresh the cached file repository ------------------------------
  rcon$flush_fileRepository()

  # Prepare Output --------------------------------------------------
  NewFolder <- as.data.frame(response)

  NewFolder$name <- rep(name, nrow(NewFolder))

  NewFolder
}

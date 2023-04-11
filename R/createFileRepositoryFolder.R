#' @name createFileRepositoryFolder
#' @title Create a Folder in the File Repository
#' 
#' @description Creates a folder in the file repository. This can be made
#'   a subfolder of an existing folder, given the folder ID of the parent
#'   folder.
#'   
#' @param rcon A \code{redcapConnection} object.
#' @param name \code{character(1)} The name of the folder. If a folder by 
#'   this name already exists, no action will be taken.
#' @param folder_id \code{integerish(0/1)}. The ID of the parent folder. If
#'   length is 0, the new folder is placed in the top-level.
#' @param dag_id \code{integerish(0/1)} The ID of a data access group. If 
#'   provided, access to the folder will be restricted to the DAG.
#' @param role_id \code{integerish(0/1)} The ID of a role. If provided, 
#'   access to the folder will be restricted to users with that role.
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
#' @details This method allows you to create a new folder in the File 
#'   Repository. You may optionally provide the folder_id of the parent 
#'   folder under which you wish this folder to be created. Providing a 
#'   dag_id and/or role_id will allow you to restrict access to only users 
#'   within a specific DAG (Data Access Group) or User Role, respectively.
#'   
#' @author Benjamin Nutter
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
  
  if (length(folder_id) > 0 && isTRUE(!folder_id %in% FileRepo$parent_folder)){
    coll$push("folder_id '%s' does not exist in the File Repository", 
              folder_id)
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
    
    message(sprintf("A folder named '%s' already exists. No action taken.", 
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
  
  body <- body[lengths(body) > 0]
  
  # Make the API Call -----------------------------------------------
  
  response <- makeApiCall(rcon, 
                          body = c(body, api_param), 
                          config)
  
  if (response$status_code != 200){
    redcap_error(response, 
                 error_handling = error_handling)
  } 
  
  # Refresh the cached file repository ------------------------------
  
  if (refresh && rcon$has_fileRepository()){
    rcon$refresh_fileRepository()
  }
  
  # Prepare Output --------------------------------------------------
  NewFolder <- read.csv(text = as.character(response), 
                        stringsAsFactors = FALSE, 
                        na.strings = "")
  NewFolder$name <- rep(name, nrow(NewFolder))
  
  NewFolder
}

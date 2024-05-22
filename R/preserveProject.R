#' @name preserveProject
#' @title Preserve Project Data Locally
#' 
#' @description The methods enable the user to export a project data and 
#'   meta data into local memory. For convenience, options are provided 
#'   to save the objects to files on the local machine. Files may be 
#'   saved as either .Rdata files or .csv files.
#'   
#' @inheritParams common-rcon-arg
#' @inheritParams redcapConnection
#' @inheritParams common-api-args
#' @param x `list` or `character`. If a `list`, the list returned (or saved)
#'   by `preserveProject`. If `character`, the directory to which the 
#'   CSV files are saved by `preserveProject`.
#' @inheritParams common-dot-args
#' @param save_as `character(1)` or `NULL`. When `"Rdata"`, the data objects
#'   will be saved to an .Rdata file. 
#'   When `"csv"`, the data objects will 
#'   be written to files at `dir`. Any other character value will prompt an 
#'   error.
#' @param dir `character(1)`. The path to a directory in which the data 
#'   objects (or files) will be saved. Must be provided if `save_as` is not
#'   `NULL`.
#' @param dir_create `logical(1)`. When `TRUE`, an attempt will be made to 
#'   create the directory at `dir` if it does not already exist. When `FALSE`, 
#'   and the directory does not exist, an error is returned.
#'   
#' @details The options to save files to local files provide the user a 
#'   convenient tool set for providing other users with the ability to work
#'   with data offline. See the examples for suggestions on how to read data
#'   into an `offlineConnection`.
#'   
#'   When saving to an .Rdata file, the data are saved in a list named
#'   `RedcapList`. The list has the same elements in the list returned when
#'   `save_as = NULL` and is suitable for creating an `offlineConnection`.
#'   The file name it is saved to follows the pattern 
#'   `"project-[project_id]-RedcapList.Rdata"`.
#'   
#'   When saving to a .csv file, each element of the data is saved to a 
#'   file with the pattern `"project-[project_id]-[data type].csv"`.
#'   
#'   `readPreservedProject` is a function of convenience for users who 
#'   need to work using offline connections. If given a `list`, it 
#'   must be in the format returned by `preserveProject`. If given a 
#'   `character`, it must be the directory in which the CSV files were 
#'   saved by `preserveProject`. If any of the file names have been changed, 
#'   `readPreservedProject` will fail to execute. Refer to 
#'   `vignette("redcapAPI-offline-connection", package = "redcapAPI")`
#'   for more details.
#'   
#' @return
#' ## `preserveProject
#' `
#' When `save_as = NULL`, returns a list is returned with the elements
#' 
#' * `project_information`
#' * `arms`
#' * `events`
#' * `meta_data`
#' * `mappings`
#' * `repeating_instruments`
#' * `users`
#' * `user_roles`
#' * `user_role_assignments`
#' * `dags`
#' * `dag_assignments`
#' * `records`
#' 
#' When `save_as` is not `NULL`, the logical `TRUE` is invisibly returned
#' to provide an indication that the save operation(s) are complete.
#' 
#' ## `readPreservedProject`
#' 
#' Returns a `redcapOfflineConnection` object.
#'   
#' @seealso
#' `vignette("redcapAPI-offline-connection", package = "redcapAPI")`,\cr
#' [offlineConnection()] \cr
#' \cr
#' [purgeProject()], \cr
#' [restoreProject()]
#' 
#' @examples
#' \dontrun{
#' unlockREDCap(connections = c(rcon = "project_alias"), 
#'              url = "your_redcap_url", 
#'              keyring = "API_KEYs", 
#'              envir = globalenv())
#'              
#' # Save a project to the current session
#' 
#' projectData <- preserveProject(rcon)
#' 
#' 
#' # Save a project to an Rdata file
#' 
#' save_to_dir <- tempdir()
#' preserveProject(rcon, 
#'                 save_as = "Rdata", 
#'                 dir = save_to_dir)
#'
#' # Create an offline connection from the Rdata file
#' load(file.path(save_to_dir, 
#'                "project-[project_id]-RedcapList.Rdata"))
#' 
#' off_conn <- readPreservedProject(RedcapList, 
#'                                  version = "[redcap_api_version]", 
#'                                  url = "[redcap_api_url]")
#'                   
#'                   
#' # Save a project to CSV files
#' 
#' save_to_dir <- tempdir()
#' preserveProject(rcon, 
#'                 save_as = "csv", 
#'                 dir = save_to_dir)
#'                 
#' # Create an offline connection from the CSV files
#' 
#' off_con <- 
#'  readPreservedProject(save_to_dir, 
#'                       version = "[redcap_api_version]", 
#'                       url = "[redcap_api_url]")
#' }
#' 
#'   
#' @export

preserveProject <- function(rcon, 
                            ..., 
                            save_as = NULL, 
                            dir, 
                            dir_create = TRUE){
  UseMethod("preserveProject")
}

#' @rdname preserveProject
#' @export

preserveProject.redcapApiConnection <- function(rcon,
                                                ..., 
                                                save_as = NULL,
                                                dir,
                                                dir_create = TRUE)
{
  ###################################################################
  # Argument Validation                                          ####
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::assert_choice(x = save_as, 
                           choices = c("Rdata", "csv"), 
                           null.ok = TRUE, 
                           add = coll)
  
  if (!is.null(save_as)){
    checkmate::assert_character(x = dir, 
                                len = 1,
                                add = coll)
  }
  
  checkmate::assert_logical(x = dir_create, 
                            len = 1, 
                            add = coll)
  
  checkmate::reportAssertions(coll)
  
  if (!is.null(save_as) && !dir_create){
    checkmate::assert_directory_exists(x = dir, 
                                       add = coll)
  }
  
  checkmate::reportAssertions(coll)
  
  ###################################################################
  # Make the List object                                         ####
  
  RedcapList <- 
    list(project_information   = exportProjectInformation(rcon, ...),
         arms                  = exportArms(rcon, ...), 
         events                = exportEvents(rcon, ...), 
         meta_data             = exportMetaData(rcon, ...),
         mappings              = exportMappings(rcon, ...),
         repeating_instruments = exportRepeatingInstrumentsEvents(rcon, ...),
         users                 = exportUsers(rcon, ...),
         user_roles            = exportUserRoles(rcon, ...),
         user_role_assignments = exportUserRoleAssignments(rcon, ...),
         dags                  = exportDags(rcon, ...),
         dag_assignments       = exportUserDagAssignments(rcon, ...),
         records               = exportRecordsTyped(rcon,
                                                    validation = skip_validation,
                                                    cast = raw_cast,
                                                    ...),
         external_coding        = exportExternalCoding(rcon, ...)
    )
  
  if (is.null(save_as)){
    return(RedcapList)
  } 
  
  if (dir_create & !dir.exists(dir)){
    dir.create(dir)
  }
  
  if (save_as == "Rdata"){
    .preserveProject_saveRdata(rcon, RedcapList, dir)
  }
  
  if (save_as == "csv"){
    .preserveProject_saveCsv(rcon, RedcapList, dir)
  }
  
  invisible(TRUE)
}

#####################################################################
# Unexported                                                     ####

.preserveProject_saveRdata <- function(rcon, RedcapList, dir){
  file_name <- sprintf("project-%s-RedcapList.Rdata", 
                       rcon$projectInformation()$project_id)
  save(RedcapList, 
       file = file.path(dir, file_name))
}

.preserveProject_saveCsv <- function(rcon, RedcapList, dir){
  for (i in seq_along(RedcapList)){
    file_name <- sprintf("project-%s-%s.csv", 
                         rcon$projectInformation()$project_id, 
                         names(RedcapList)[i])
    
    if (names(RedcapList)[i] == "external_coding"){
      dput(RedcapList[[i]], 
           file = file.path(dir, 
                            sub("\\.csv", ".txt", file_name)))
    } else {
      utils::write.csv(x = RedcapList[[i]], 
                       file = file.path(dir, file_name), 
                       row.names = FALSE)
    }
  }
}

#' @rdname preserveProject
#' @export

readPreservedProject <- function(x, ...){
  UseMethod("readPreservedProject")
}

#' @rdname preserveProject
#' @export

readPreservedProject.list <- function(x, ..., 
                                      version = NULL, 
                                      url = NULL){
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_list(x = x, 
                         names = "named", 
                         types = c("data.frame", "list"),
                         add = coll)
  
  checkmate::reportAssertions(coll)
  
  checkmate::assert_subset(x = names(x), 
                           choices = c("project_information", 
                                       "arms", 
                                       "events", 
                                       "meta_data", 
                                       "mappings", 
                                       "repeating_instruments", 
                                       "users", 
                                       "user_roles", 
                                       "user_role_assignments", 
                                       "dags", 
                                       "dag_assignments", 
                                       "records", 
                                       "external_coding"), 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  offlineConnection(meta_data = x$meta_data, 
                    arms = x$arms, 
                    events = x$events,  
                    mapping = x$mappings,
                    repeat_instrument = x$repeat_instruments,
                    users = x$users, 
                    user_roles = x$user_roles,
                    dags = x$dags, 
                    dag_assignment = x$dag_assignments,
                    records = x$records, 
                    project_info = x$project_information,
                    version = version, 
                    url = url, 
                    external_coding = x$external_coding)
}

#' @rdname preserveProject
#' @export

readPreservedProject.character <- function(x, 
                                           ..., 
                                           version = NULL, 
                                           url = NULL){
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_character(x = x, 
                              len = 1, 
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  checkmate::assert_directory_exists(x = x, 
                                     add = coll)
  
  checkmate::reportAssertions(coll)
  
  files <- list.files(x, 
                      pattern = "project-[0-9]+.+$", 
                      full.names = TRUE)
  
  external_coding <- .rPP_getFileName(files, "external_coding")
  if (file.exists(external_coding)){
    external_coding <- eval(parse(file = external_coding))
  } else {
    external_coding <- list()
  }
  
  
  offlineConnection(
    meta_data = .rPP_getFileName(files, "meta_data"), 
    arms = .rPP_getFileName(files, "arms"), 
    events = .rPP_getFileName(files, "events"),
    mapping = .rPP_getFileName(files, "mappings"), 
    repeat_instrument = .rPP_getFileName(files, "repeating_instruments"), 
    users = .rPP_getFileName(files, "users"), 
    user_roles = .rPP_getFileName(files, "user_roles"), 
    dags = .rPP_getFileName(files, "dags"),
    dag_assignment = .rPP_getFileName(files, "dag_assignments"), 
    records = .rPP_getFileName(files, "records"), 
    version = version, 
    url = url, 
    external_coding = external_coding
  )
}

.rPP_getFileName <- function(files, type){
  files[which(grepl(type, files))]
}

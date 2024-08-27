#' @name purgeRestoreProject
#' @title Purge and Restore Project Data
#' 
#' @description These functions are primarily intended to assist with testing
#'   features of `redcapAPI`. Purging and restoring project data permits 
#'   us to perform tests on different project structures without having to 
#'   manage multiple projects or API tokens. 
#'   
#'   When purging project data, many of these actions may 
#'   only be performed with a project in development status, as they are 
#'   potentially destructive and may result in data loss. It is a good 
#'   practice to back up your data and project structure before purging
#'   a project. 
#'
#' @inheritParams common-dot-args
#' @inheritParams common-api-args
#' @param object,rcon A `redcapConnection` object. Except in 
#'   `restoreProject.list`, where `object` is a list of data frames
#'   to use in restoring the project. 
#' @param project_information `data.frame` for restoring data. Provides the 
#'   project settings to load via `importProjectInformation`. 
#' @param arms Either `logical(1)` indicating if arms data should be
#'   purged from the project; or a `data.frame` for restoring arms 
#'   data via `importArms`. 
#' @param events Either `logical(1)` indicating if events data should be
#'   purged from the project; or a `data.frame` for restoring events 
#'   data via `importEvents`
#' @param meta_data A `data.frame` for restoring metadata
#'   data via `importMetaData`. The API does not support deleting 
#'   metadata, but an import replaces the existing metadata.
#' @param mappings A `data.frame` for restoring instrument-event mappings
#'   via `importMappings`. The API does not support deleting 
#'   mappings, but an import replaces the existing mappings.
#' @param repeating_instruments A `data.frame` for restoring repeating instruments
#'   configuration via [importRepeatingInstrumentsEvents()]. The API does not support deleting 
#'   repeating instruments, but an import replaces the existing instruments.
#'   NOT YET IMPLEMENTED
#' @param users Either `logical(1)` indicating if users data should be
#'   purged from the project; or a `data.frame` for restoring users 
#'   data via `importUsers`. NOT YET IMPLEMENTED
#' @param user_roles Either `logical(1)` indicating if user roles data should be
#'   purged from the project; or a `data.frame` for restoring user roles
#'   data via `importUserRoles`. NOT YET IMPLEMENTED
#' @param user_role_assignments A `data.frame` for restoring user-role 
#'   assignments via `importUserRoleAssignments`. The API does not support deleting 
#'   assignments, but an import replaces the existing assignments. NOT YET IMPLEMENTED.
#' @param dags Either `logical(1)` indicating if DAG data should be
#'   purged from the project; or a `data.frame` for restoring DAGs 
#'   data via `importDags`. NOT YET IMPLEMENTED
#' @param dag_assignments A `data.frame` for restoring DAG 
#'   assignments via `importDagAssignments`. The API does not support deleting 
#'   assignments, but an import replaces the existing assignments. NOT YET IMPLEMENTED.
#' @param records Either `logical(1)` indicating if records data should be
#'   purged from the project; or a `data.frame` for restoring records
#'   data via `importRecords`
#' @param purge_all `logical(1)`. A shortcut option to purge all 
#'   data elements from a project.
#' @param flush `logical(1)`. When `TRUE`, all caches in the connection
#'   object will be flushed after completing the operation. This is highly 
#'   recommended.
#'   
#' @details When restoring a project, all arguments are optional. Any argument 
#' that is `NULL` will result in no import being made. The order of 
#' reconstructing the project is (purging data occurs in the reverse order):
#' 
#'  1. Update project information
#'  2. Import Arms Data
#'  3. Import Events Data
#'  4. Import Meta Data
#'  5. Import Mappings
#'  6. Import Repeating Instruments
#'  7. Import Users
#'  8. Import User Roles
#'  9. Import User-Role Assignments
#'  10. Import Data Access Groups
#'  11. Import Data Access Group Assignments
#'  12. Import Records
#'  
#' @examples
#' \dontrun{
#' unlockREDCap(connections = c(rcon = "project_alias"), 
#'              url = "your_redcap_url", 
#'              keyring = "API_KEYs", 
#'              envir = globalenv())
#'              
#' # Preserve a project
#' preserveProject(rcon)
#' 
#' # Purge a project
#' purgeProject(rcon, 
#'              purge_all = TRUE)
#'                 
#' # Restore a project
#' restoreProject(rcon)
#' 
#' }
#' 


#####################################################################
# Preserve Project                                               ####


#####################################################################
# Purge Project                                                  ####

#' @rdname purgeRestoreProject
#' @export

purgeProject <- function(rcon, ...){
  UseMethod("purgeProject")
}


#' @rdname purgeRestoreProject
#' @export

purgeProject.redcapApiConnection <- function(rcon, 
                                             arms           = FALSE, 
                                             events         = FALSE, 
                                             users          = FALSE, 
                                             user_roles     = FALSE, 
                                             dags           = FALSE, 
                                             records        = FALSE, 
                                             purge_all      = FALSE,
                                             flush          = TRUE, 
                                             ...)
{
  ###################################################################
  # Argument Validation                                          ####
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::assert_logical(x = arms, 
                            len = 1, 
                            any.missing = FALSE, 
                            add = coll)
  
  checkmate::assert_logical(x = events, 
                            len = 1, 
                            any.missing = FALSE, 
                            add = coll)
  
  checkmate::assert_logical(x = users, 
                            len = 1, 
                            any.missing = FALSE, 
                            add = coll)
  
  checkmate::assert_logical(x = user_roles, 
                            len = 1, 
                            any.missing = FALSE, 
                            add = coll)
  
  checkmate::assert_logical(x = dags, 
                            len = 1, 
                            any.missing = FALSE, 
                            add = coll)
  
  checkmate::assert_logical(x = records, 
                            len = 1, 
                            any.missing = FALSE, 
                            add = coll)
  
  checkmate::assert_logical(x = purge_all, 
                            len = 1, 
                            any.missing = FALSE,
                            add = coll)
  
  checkmate::assert_logical(x = flush, 
                            len = 1, 
                            any.missing = FALSE,
                            add = coll)

  checkmate::reportAssertions(coll)
  
  ###################################################################
  # Functional Code                                              ####
  
  if (purge_all){
    arms <- events <- users <- user_roles <- dags <- records <- purge_all
  }
  
  if (records){
    RecordId <- exportRecordsTyped(rcon, 
                                   fields = rcon$metadata()$field_name[1],
                                   cast = list(system = castRaw), 
                                   ...)
    
    if (nrow(RecordId)>0){
      if ("redcap_event_name" %in% names(RecordId)){
        RecordId$arm_num <- sub("^(.+)(arm_)(\\d+)$", "\\3", RecordId$redcap_event_name)
        
        RecordId <- RecordId[c(names(RecordId)[1], "arm_num")]
        RecordId <- RecordId[!duplicated(RecordId), ]
        
        RecordId <- split(RecordId, 
                          RecordId$arm_num, 
                          drop = TRUE)
        
        # Delete records from each arm individually
        for (i in seq_along(RecordId)){
          deleteRecords(rcon, 
                        records = RecordId[[i]][[1]], 
                        arm = unique(RecordId[[i]]$arm_num),
                        ...)
        }
      } else {
        deleteRecords(rcon, 
                      records = RecordId[[1]],
                      ...)
      }
    }
  }
  
  if (dags && nrow(rcon$dags()) > 0){
    deleteDags(rcon,
               dags = rcon$dags()$unique_group_name,
               ...)
  }

  if (user_roles && nrow(rcon$user_roles()) > 0){
    deleteUserRoles(rcon,
                    rcon$user_roles()$unique_role_name,
                    ...)
  }
  
  # if (users){
  # deleteUsers(rcon,
  #             rcon$users()$username,
  #             config = config)
  # }
  
  if (events)
    deleteEvents(rcon, 
                 events = rcon$events()$unique_event_name, 
                 ...)
  
  if (arms && !is.null(rcon$arms()$arm_num))
    deleteArms(rcon, 
               arms = rcon$arms()$arm_num,
               ...)
}

#####################################################################
# restoreProject                                                 ####

#' @rdname purgeRestoreProject
#' @export

restoreProject <- function(object, ...){
  UseMethod("restoreProject")
}

#' @rdname purgeRestoreProject
#' @export

restoreProject.redcapApiConnection <- function(object, 
                                               project_information   = NULL,
                                               arms                  = NULL, 
                                               events                = NULL, 
                                               meta_data             = NULL,
                                               mappings              = NULL, 
                                               repeating_instruments = NULL,
                                               users                 = NULL, 
                                               user_roles            = NULL, 
                                               user_role_assignments = NULL,
                                               dags                  = NULL, 
                                               dag_assignments       = NULL,
                                               records               = NULL, 
                                               flush                 = TRUE, 
                                               ...)
{
  rcon <- object
  ###################################################################
  # Argument Validation                                          ####
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::assert_data_frame(x = project_information, 
                               null.ok = TRUE,
                               col.names = "named",
                               add = coll)
  
  checkmate::assert_data_frame(x = arms, 
                               null.ok = TRUE,
                               col.names = "named",
                               add = coll)
  
  checkmate::assert_data_frame(x = events, 
                               null.ok = TRUE,
                               col.names = "named",
                               add = coll)
  
  checkmate::assert_data_frame(x = meta_data, 
                               null.ok = TRUE,
                               col.names = "named",
                               add = coll)
  
  checkmate::assert_data_frame(x = mappings, 
                               null.ok = TRUE,
                               col.names = "named",
                               add = coll)
  
  checkmate::assert_data_frame(x = repeating_instruments, 
                               null.ok = TRUE,
                               col.names = "named",
                               add = coll)
  
  checkmate::assert_data_frame(x = users, 
                               null.ok = TRUE,
                               col.names = "named",
                               add = coll)
  
  checkmate::assert_data_frame(x = user_roles, 
                               null.ok = TRUE,
                               col.names = "named",
                               add = coll)
  
  checkmate::assert_data_frame(x = user_role_assignments, 
                               null.ok = TRUE,
                               col.names = "named",
                               add = coll)
  
  checkmate::assert_data_frame(x = dags, 
                               null.ok = TRUE,
                               col.names = "named",
                               add = coll)
  
  checkmate::assert_data_frame(x = dag_assignments, 
                               null.ok = TRUE,
                               col.names = "named",
                               add = coll)
  
  checkmate::assert_data_frame(x = records, 
                               null.ok = TRUE,
                               col.names = "named",
                               add = coll)

  checkmate::reportAssertions(coll)
  
  ###################################################################
  # Functional Code                                              ####
  
  if (!is.null(project_information)){
    importProjectInformation(rcon, 
                             data = project_information,
                             ...)
  }
  
  if (!is.null(arms) && nrow(arms) > 0){
    importArms(rcon, 
               arms_data = arms, 
               ...)
  }
  
  if (!is.null(events) && nrow(events) > 0){
    importEvents(rcon, 
                 event_data = events,
                 ...)
  }
  
  if (!is.null(meta_data)){
    importMetaData(rcon, 
                   data = meta_data,
                   ...)
  }
  
  if (!is.null(mappings) && nrow(mappings) > 0){
    importMappings(rcon, 
                   data = mappings,
                   ...)
  }
  
  if (!is.null(repeating_instruments) && nrow(repeating_instruments) > 0){
    importRepeatingInstrumentsEvents(rcon,
                                     data = repeating_instruments,
                                     ...)
  }
  
  if (!is.null(users)){
    importUsers(rcon,
                data = users,
                ...)
  }
  
  if (!is.null(user_roles)){
    importUserRoles(rcon,
                    data = user_roles,
                    ...)
  }
  
  if (!is.null(user_role_assignments)){
    importUserRoleAssignments(rcon,
                              data = user_role_assignments,
                              ...)
  }
  
  if (!is.null(dags)){
    importDags(rcon,
               data = dags,
               ...)
  }
  
  if (!is.null(dag_assignments)){
    importUserDagAssignments(rcon,
                             data = dag_assignments,
                             ...)
  }
  
  if (!is.null(records) && nrow(records) > 0){
    importRecords(rcon, 
                  data = records,
                  ...)
  }
  
  if (flush) rcon$flush_all()
}

#' @rdname purgeRestoreProject
#' @export

restoreProject.list <- function(object,
                                ...,
                                rcon)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_list(x = object, 
                         names = "named", 
                         add = coll)
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::reportAssertions(coll)
  
  checkmate::assert_subset(x = names(object), 
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
                                       "records"), 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  object$rcon <- rcon

  do.call(restoreProject.redcapApiConnection,
          utils::modifyList(list(...), object))
}

#' @name purgeRestoreProject
#' @title Purge and Restore Project Data
#' 
#' @description These functions are primarily intended to assist with testing
#'   features of \code{redcapAPI}. Purging and restoring project data permits 
#'   us to perform tests on different project structures without having to 
#'   manage multiple projects or API tokens. 
#'   
#'   When purging project data, many of these actions may 
#'   only be performed with a project in development status, as they are 
#'   potentially destructive and may result in data loss. It is a good 
#'   practice to back up your data and project structure before purging
#'   a project. 
#'   
#' @param object,rcon A \code{redcapConnection} object. Except in 
#'   \code{restoreProject.list}, where \code{object} is a list of data frames
#'   to use in restoring the project. 
#' @param project_information \code{data.frame} for restoring data. Provides the 
#'   project settings to load via \code{importProjectInformation}. 
#' @param arms Either \code{logical(1)} indicating if arms data should be
#'   purged from the project; or a \code{data.frame} for restoring arms 
#'   data via \code{importArms}. 
#' @param events Either \code{logical(1)} indicating if events data should be
#'   purged from the project; or a \code{data.frame} for restoring events 
#'   data via \code{importEvents}
#' @param meta_data A \code{data.frame} for restoring metadata
#'   data via \code{importMetaData}. The API doesn't support deleting 
#'   metadata, but an import replaces the existing metadata.
#' @param mappings A \code{data.frame} for restoring instrument-event mappings
#'   via \code{importMappings}. The API doesn't support deleting 
#'   mappings, but an import replaces the existing mappings.
#' @param repeating_instruments A \code{data.frame} for restoring repeating instruments
#'   configuration via \code{\link{importRepeatingInstrumentsEvents}}. The API doesn't support deleting 
#'   repeating instruments, but an import replaces the existing instruments.
#'   NOT YET IMPLEMENTED
#' @param users Either \code{logical(1)} indicating if users data should be
#'   purged from the project; or a \code{data.frame} for restoring users 
#'   data via \code{importUsers}. NOT YET IMPLEMENTED
#' @param user_roles Either \code{logical(1)} indicating if user roles data should be
#'   purged from the project; or a \code{data.frame} for restoring user roles
#'   data via \code{importUserRoles}. NOT YET IMPLEMENTED
#' @param user_role_assignments A \code{data.frame} for restoring user role 
#'   assignments via \code{importUserRoleAssignments}. The API doesn't support deleting 
#'   assignments, but an import replaces the existing assignments. NOT YET IMPLEMENTED.
#' @param dags Either \code{logical(1)} indicating if DAG data should be
#'   purged from the project; or a \code{data.frame} for restoring DAGs 
#'   data via \code{importDags}. NOT YET IMPLEMENTED
#' @param dag_assignments A \code{data.frame} for restoring DAG 
#'   assignments via \code{importDagAssignments}. The API doesn't support deleting 
#'   assignments, but an import replaces the existing assignments. NOT YET IMPLEMENTED.
#' @param records Either \code{logical(1)} indicating if user roles data should be
#'   purged from the project; or a \code{data.frame} for restoring user roles
#'   data via \code{importRecords}
#' @param purge_all \code{logical(1)}. A shortcut option to purge all 
#'   data elements from a project.
#' @param flush \code{logical(1)}. If \code{TRUE}, the all caches in the connection
#'   object will be flushed after completing the operation. This is highly 
#'   recommended.
#' @param ... Arguments to pass to other methods. 
#' @param error_handling An option for how to handle errors returned by the API.
#'   see \code{\link{redcap_error}}
#' @param config \code{list} Additional configuration parameters to pass to 
#'   \code{\link[httr]{POST}}. These are appended to any parameters in 
#'   \code{rcon$config}.
#'   
#' @details When restoring a project, all arguments are optional. Any argument 
#' that is \code{NULL} will result in no import being made. The order of 
#' reconstructing the project is (purging data occurs in the reverse order):
#' 
#' \itemize{
#'  \item{1. }{Update project information}
#'  \item{2. }{Import Arms Data}
#'  \item{3. }{Import Events Data}
#'  \item{4. }{Import Meta Data}
#'  \item{5. }{Import Mappings}
#'  \item{6. }{Import Repeating Instruments}
#'  \item{7. }{Import Users}
#'  \item{8. }{Import User Roles}
#'  \item{9. }{Import User Role Assignments}
#'  \item{10. }{Import Data Access Groups}
#'  \item{11. }{Import Data Access Group Assignments}
#'  \item{12. }{Import Records}
#' }


#####################################################################
# Preserve Project                                               ####

#' @rdname purgeRestoreProject
#' @export

preserveProject <- function(object, 
                            ...){
  UseMethod("preserveProject")
}

#' @rdname purgeRestoreProject
#' @export

preserveProject.redcapApiConnection <- function(object,
                                                ..., 
                                                error_handling = getOption("redcap_error_handling"), 
                                                config         = list()){
  ###################################################################
  # Argument Validation                                          ####
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = object, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  error_handling <- checkmate::matchArg(x = error_handling, 
                                        choices = c("null", "error"), 
                                        .var.name = "error_handling",
                                        add = coll)
  
  checkmate::assert_list(x = config, 
                         names = "named", 
                         add = coll)
  
  checkmate::reportAssertions(coll)
  
  ###################################################################
  # Make the List object                                         ####
  
  RedcapList <- 
    list(project_information   = exportProjectInformation(object, 
                                                          error_handling = error_handling, 
                                                          config = config),
         arms                  = exportArms(object, 
                                            error_handling = error_handling, 
                                            config = config), 
         events                = exportEvents(object, 
                                              error_handling = error_handling, 
                                              config = config), 
         meta_data             = exportMetaData(object, 
                                                error_handling = error_handling, 
                                                config = config),
         mappings              = exportMappings(object, 
                                                error_handling = error_handling, 
                                                config = config), 
         repeating_instruments = exportRepeatingInstrumentsEvents(object, 
                                                                  error_handling = error_handling, 
                                                                  config = config),
         users                 = exportUsers(object, 
                                             error_handling = error_handling, 
                                             config = config), 
         user_roles            = NULL, # FIXME: Add this when methods are available 
         user_role_assignments = NULL, # FIXME: Add this when methods are available
         dags                  = NULL, # FIXME: Add this when methods are available
         dag_assignments       = NULL, # FIXME: Add this when methods are available
         records               = exportRecordsTyped(object, 
                                                    cast = raw_cast,
                                                    error_handling = error_handling, 
                                                    config = config)
    )
  
  RedcapList
}

#####################################################################
# Purge Project                                                  ####

#' @rdname purgeRestoreProject
#' @export

purgeProject <- function(object, ...){
  UseMethod("purgeProject")
}


#' @rdname purgeRestoreProject
#' @export

purgeProject.redcapApiConnection <- function(object, 
                                             arms           = FALSE, 
                                             events         = FALSE, 
                                             users          = FALSE, 
                                             user_roles     = FALSE, 
                                             dags           = FALSE, 
                                             records        = FALSE, 
                                             purge_all      = FALSE,
                                             flush          = TRUE, 
                                             ..., 
                                             error_handling = getOption("redcap_error_handling"), 
                                             config         = list()){
  ###################################################################
  # Argument Validation                                          ####
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = object, 
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
  
  error_handling <- checkmate::matchArg(x = error_handling, 
                                        choices = c("null", "error"), 
                                        .var.name = "error_handling",
                                        add = coll)
  
  checkmate::assert_list(x = config, 
                         names = "named", 
                         add = coll)
  
  checkmate::reportAssertions(coll)
  
  ###################################################################
  # Functional Code                                              ####
  
  if (purge_all){
    arms <- events <- users <- user_roles <- dags <- records <- purge_all
  }
  
  if (records){
    RecordId <- exportRecordsTyped(object, 
                                   fields = object$metadata()$field_name[1],
                                   cast = list(system = castRaw),
                                   error_handling = error_handling, 
                                   config = config)
    
    if (nrow(RecordId)){
      if ("redcap_event_name" %in% names(RecordId)){
        RecordId$arm_num <- sub("^(.+)(arm_)(\\d+)$", "\\3", RecordId$redcap_event_name)
        
        RecordId <- RecordId[c(names(RecordId)[1], "arm_num")]
        RecordId <- RecordId[!duplicated(RecordId), ]
        
        RecordId <- split(RecordId, 
                          RecordId$arm_num, 
                          drop = TRUE)
        
        # Delete records from each arm individually
        for (i in seq_along(RecordId)){
          deleteRecords(object, 
                        records = RecordId[[i]][[1]], 
                        arm = unique(RecordId[[i]]$arm_num),
                        error_handling = error_handling, 
                        config = config)
        }
      } else {
        deleteRecords(object, 
                      records = RecordId[[1]], 
                      error_handling = error_handling, 
                      config = config)
      }
    }
  }
  
  if (dags){
    # FIXME: Uncomment after writing DAG methods
    # deleteDags(object, 
    #            dags = object$dags()$unique_group_name, 
    #            error_handling = error_handling, 
    #            config = config)
  }

  if (user_roles){
    # FIXME: Uncomment after writing User Role methods
    # deleteUserRoles(object, 
    #                 object$user_roles()$unique_role_name, 
    #                 error_handling = error_handling, 
    #                 config = config)
  }
  
  if (users){
    # FIXME: Uncomment after writing User methods
    # deleteUsers(object, 
    #             object$users()$username, 
    #             error_handling = error_handling, 
    #             config = config)
  }
  
  if (events){
    deleteEvents(object, 
                 events = object$events()$unique_event_name, 
                 error_handling = error_handling, 
                 config = config)
  }
  
  if (arms && !is.null(object$arms()$arm_num)){
    deleteArms(object, 
               arms = object$arms()$arm_num, 
               error_handling = error_handling, 
               config = config)
  }
  
  if (flush) object$flush_all()
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
                                               ..., 
                                               error_handling        = getOption("redcap_error_handling"), 
                                               config                = list()){
  ###################################################################
  # Argument Validation                                          ####
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = object, 
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
  
  error_handling <- checkmate::matchArg(x = error_handling, 
                                        choices = c("null", "error"), 
                                        .var.name = "error_handling",
                                        add = coll)
  
  checkmate::assert_list(x = config, 
                         names = "named", 
                         add = coll)
  
  checkmate::reportAssertions(coll)
  
  ###################################################################
  # Functional Code                                              ####
  
  if (!is.null(project_information)){
    importProjectInformation(object, 
                             data = project_information, 
                             error_handling = error_handling, 
                             config = config)
  }
  
  if (!is.null(arms) && nrow(arms) > 0){
    importArms(object, 
               arms_data = arms, 
               error_handling = error_handling, 
               config = config)
  }
  
  if (!is.null(events) && nrow(events) > 0){
    importEvents(object, 
                 event_data = events, 
                 error_handling = error_handling, 
                 config = config)
  }
  
  if (!is.null(meta_data)){
    importMetaData(object, 
                   data = meta_data, 
                   error_handling = error_handling, 
                   config = config)
  }
  
  if (!is.null(mappings) && nrow(mappings) > 0){
    importMappings(object, 
                   data = mappings, 
                   error_handling = error_handling, 
                   config = config)
  }
  
  if (!is.null(repeating_instruments) && nrow(repeating_instruments) > 0){
    importRepeatingInstrumentsEvents(object,
                                     data = repeating_instruments,
                                     error_handling = error_handling,
                                     config = config)
  }
  
  if (!is.null(users)){
    # FIXME: Uncomment after implementing User methods
    # importUsers(object,
    #             data = users, 
    #             error_handling = error_handling, 
    #             config = config)
  }
  
  if (!is.null(user_roles)){
    # FIXME: Uncomment after implementing User Role methods
    # importUserRoles(object,
    #                 data = user_roles, 
    #                 error_handling = error_handling, 
    #                 config = config)
  }
  
  if (!is.null(user_role_assignments)){
    # FIXME: Uncomment after implementing User Role methods
    # importUserRoleAssignments(object,
    #                           data = user_role_assignments, 
    #                           error_handling = error_handling, 
    #                           config = config)
  }
  
  if (!is.null(dags)){
    # FIXME: Uncomment after implementing DAG methods
    # importDags(object,
    #            data = dags, 
    #            error_handling = error_handling, 
    #            config = config)
  }
  
  if (!is.null(dag_assignments)){
    # FIXME: Uncomment after implementing DAG methods
    # importDagAssignments(object,
    #                      data = dag_assignments, 
    #                      error_handling = error_handling, 
    #                      config = config)
  }
  
  if (!is.null(records) && nrow(records) > 0){
    importRecords(object, 
                  data = records, 
                  error_handling = error_handling, 
                  config = config)
  }
  
  if (flush) object$flush_all()
}

#' @rdname purgeRestoreProject
#' @export

restoreProject.list <- function(object, 
                                rcon, 
                                ..., 
                                error_handling = getOption("redcap_error_handling"), 
                                config         = list()){
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

  do.call(restoreProject.redcapApiConnection, 
          c(list(object = rcon), object))
}

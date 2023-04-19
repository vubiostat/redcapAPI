#' @name purgeRestoreProject
#' @title Purge and Restore Project Data
#' 
#' @description These functions are primarily intended to assist with testing
#'   features of \code{redcapAPI}. Purging and restoring project data permits 
#'   us to perform tests on different project structures without havng to 
#'   manage multiple projects or API tokens. 
#'   
#'   When purging project data, many of these actions may 
#'   only be performed with a project in development status, as they are 
#'   potentially destructive and may result in data loss. It is a good 
#'   practice to back up your data and project structure before purging
#'   a project. 
#'   
#' @param rcon A \code{redcapConnection} object. 
#' @param project_info \code{data.frame} for restoring data. Provides the 
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
#' @param repeating_insruments A \code{data.frame} for restoring repeating instruments
#'   configuration via \code{importRepeatingInstrumentsEvents}. The API doesn't support deleting 
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
#'  \item{1}{Update project information}
#'  \item{2}{Import Arms Data}
#'  \item{3}{Import Events Data}
#'  \item{4}{Import Meta Data}
#'  \item{5}{Import Mappings}
#'  \item{6}{Import Repeating Instruments}
#'  \item{7}{Import Users}
#'  \item{8}{Import User Roles}
#'  \item{9}{Import User Role Assignments}
#'  \item{10}{Import Data Access Groups}
#'  \item{11}{Import Data Access Group Assignments}
#'  \item{12}{Import Records}
#' }

purgeProject <- function(rcon, 
                         arms           = FALSE, 
                         events         = FALSE, 
                         users          = FALSE, 
                         user_roles     = FALSE, 
                         dags           = FALSE, 
                         records        = FALSE, 
                         error_handling = getOption("redcap_error_handling"), 
                         config         = list()){
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
  
  if (records){
    RecordId <- exportRecordsTyped(rcon, 
                                   fields = rcon$metadata()$field_name[1], 
                                   error_handling = error_handling, 
                                   config = config)
    
    deleteRecords(rcon, 
                  records = RecordId[[1]], 
                  error_handling = error_handling, 
                  config = config)
  }
  
  if (dags){
    # FIXME: Uncomment after writing DAG methods
    # deleteDags(rcon, 
    #            dags = rcon$dags()$unique_group_name, 
    #            error_handling = error_handling, 
    #            config = config)
  }

  if (user_roles){
    # FIXME: Uncomment after writing User Role methods
    # deleteUserRoles(rcon, 
    #                 rcon$user_roles()$unique_role_name, 
    #                 error_handling = error_handling, 
    #                 config = config)
  }
  
  if (users){
    # FIXME: Uncomment after writing User methods
    # deleteUsers(rcon, 
    #             rcon$users()$username, 
    #             error_handling = error_handling, 
    #             config = config)
  }
  
  if (events){
    deleteEvents(rcon, 
                 events = rcon$events()$unique_event_name, 
                 error_handling = error_handling, 
                 config = config)
  }
  
  if (arms){
    deleteArms(rcon, 
               arms = rcon$arms()$arm_num, 
               error_handling = error_handling, 
               config = config)
  }
}

#' @rdname purgeRestoreProject

restoreProject <- function(rcon, 
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
                           error_handling        = getOption("redcap_error_handling"), 
                           config                = list()){
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
    importProjectInformation(rcon, 
                             data = project_information, 
                             error_handling = error_handling, 
                             config = config)
  }
  
  if (!is.null(arms)){
    importArms(rcon, 
               arms_data = arms, 
               error_handling = error_handling, 
               config = config)
  }
  
  if (!is.null(events)){
    importEvents(rcon, 
                 event_data = events, 
                 error_handling = error_handling, 
                 config = config)
  }
  
  if (!is.null(meta_data)){
    importMetaData(rcon, 
                   data = meta_data, 
                   error_handling = error_handling, 
                   config = config)
  }
  
  if (!is.null(mappings)){
    importMappings(rcon, 
                   data = mappings, 
                   error_handling = error_handling, 
                   config = config)
  }
  
  if (!is.null(repeating_instruments)){
   # FIXME: Uncomment after implementing Repeating Instrument methods
    # importRepeatingInstrumentsEvents(rcon,
    #                                  data = repeating_instruments,
    #                                  error_handling = error_handling, 
    #                                  config = config)
  }
  
  if (!is.null(users)){
    # FIXME: Uncomment after implementing User methods
    # importUsers(rcon,
    #             data = users, 
    #             error_handling = error_handling, 
    #             config = config)
  }
  
  if (!is.null(user_roles)){
    # FIXME: Uncomment after implementing User Role methods
    # importUserRoles(rcon,
    #                 data = user_roles, 
    #                 error_handling = error_handling, 
    #                 config = config)
  }
  
  if (!is.null(user_role_assignments)){
    # FIXME: Uncomment after implementing User Role methods
    # importUserRoleAssignments(rcon,
    #                           data = user_role_assignments, 
    #                           error_handling = error_handling, 
    #                           config = config)
  }
  
  if (!is.null(dags)){
    # FIXME: Uncomment after implementing DAG methods
    # importDags(rcon,
    #            data = dags, 
    #            error_handling = error_handling, 
    #            config = config)
  }
  
  if (!is.null(dag_assignments)){
    # FIXME: Uncomment after implementing DAG methods
    # importDagAssignments(rcon,
    #                      data = dag_assignments, 
    #                      error_handling = error_handling, 
    #                      config = config)
  }
  
  if (!is.null(records)){
    importRecords(rcon, 
                  data = records, 
                  error_handling = error_handling, 
                  config = config)
  }
}

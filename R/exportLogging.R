#' @name exportLogging
#' @title Export Logging Records
#' 
#' @description These methods enable to user to export the logging 
#'   (audit trail) of all changes made to a project, including data exports, 
#'   data changes, project metadata changes, modification of user rights, etc.
#'   
#' @inheritParams common-rcon-arg
#' @inheritParams common-dot-args
#' @inheritParams common-api-args
#' @param logtype `character(0/1)`. The log event types to export. 
#'   When the length is zero, all event types are exported. Otherwise, it
#'   must be one of
#'   `c("export", "manage", "user", "record", "record_add", "record_edit", "record_delete", "lock_record", "page_view")`
#' @param user `character(0/1)`. Users for whom to return logs. By default
#'   logs for all users are returned.
#' @param record `character(0/1)`. Record ID for which logs are to be returned.
#'   By default, logs are returned for all records.
#' @param dag `character(0/1)`. Data access group ID for which to return logs. 
#'   By default, logs are returned for all data access groups.
#' @param beginTime `POSIXct(0/1)`. When given, only 
#'   logs recorded after this time will be returned.
#' @param endTime `POSIXct(0/1)`. When given, only logs
#'   recorded before this time will be returned. 
#'   
#' @return 
#' Returns a data frame with columns
#' 
#' |            |                                      |
#' |------------|--------------------------------------|
#' | `timestamp` | The date/time of the logging record. |
#' | `username`  | The user name of the user that performed the action being logged. |
#' | `action`    | The classification of action being logged. |
#' | `details`   | Details of the action being logged. |
#' | `record`    | The record ID associated with the action being logged. When not related to a record, this will be `NA` |
#'
#' 
#' @examples
#' \dontrun{
#' unlockREDCap(connections = c(rcon = "project_alias"), 
#'              url = "your_redcap_url", 
#'              keyring = "API_KEYs", 
#'              envir = globalenv())
#'                     
#' # Export all of the logging events       
#' exportLogging(rcon)
#'   
#' # Export all of the events for record '2'
#' exportLogging(rcon, 
#'               record = "2")
#'                 
#' # Export all of the events where a record was deleted
#' exportLoging(rcon, 
#'              logtype = "record_delete")
#' }
#' @export

exportLogging <- function(rcon, 
                          logtype = character(0), 
                          user = character(0), 
                          record = character(0), 
                          dag = character(0), 
                          beginTime = .POSIXct(character(0)), 
                          endTime = .POSIXct(character(0)), 
                          ...){
  
  UseMethod("exportLogging")
  
}

#' @rdname exportLogging
#' @export

exportLogging.redcapApiConnection <- function(rcon, 
                                              logtype = character(0), 
                                              user = character(0), 
                                              record = character(0), 
                                              dag = character(0), 
                                              beginTime = as.POSIXct(character(0)), 
                                              endTime = as.POSIXct(character(0)), 
                                              ...,
                                              error_handling = getOption("redcap_error_handling"),
                                              config = list(), 
                                              api_param = list()){
  
  # Argument checks -------------------------------------------------
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon,
                          classes = "redcapApiConnection",
                          add = coll)
  
  if (length(logtype) == 1){
    logtype <- checkmate::matchArg(x = logtype, 
                                   choices = c("export", "manage", "user", "record", 
                                               "record_add", "record_edit", "record_delete", 
                                               "lock_record", "page_view"),
                                   add = coll, 
                                   .var.name = "logtype")
  }
  
  checkmate::assert_character(x = user,
                              max.len = 1,
                              add = coll)
  
  checkmate::assert_character(x = record, 
                              max.len = 1,
                              add = coll)
  
  checkmate::assert_character(x = dag, 
                              max.len = 1,
                              add = coll)
  
  checkmate::assert_posixct(x = beginTime, 
                            max.len = 1, 
                            add = coll)
  
  checkmate::assert_posixct(x = endTime, 
                            max.len = 1, 
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
  
  # Build the Body List ---------------------------------------------
  
  body <- list(content = 'log', 
               format = 'csv', 
               returnFormat = 'csv', 
               logtype = logtype, 
               user = user, 
               record = record, 
               dag = dag, 
               beginTime = format(beginTime, 
                                  format = "%Y-%m-%d %H:%M"), 
               endTime = format(endTime, 
                                format= "%Y-%m-%d %H:%M"))
  
  body <- body[lengths(body) > 0]
  
  # Call to the API -------------------------------------------------
  response <- makeApiCall(rcon, 
                          body = c(body, api_param), 
                          config = config)
  
  if (response$status_code != 200){
    redcapError(response, 
                 error_handling = error_handling)
  } 
  
  Log <- utils::read.csv(text = as.character(response),
                         stringsAsFactors = FALSE,
                         na.strings = "")
  
  # Format and return data ------------------------------------------
  Log$timestamp <- as.POSIXct(Log$timestamp, 
                              format = "%Y-%m-%d %H:%M")
  
  Log
}

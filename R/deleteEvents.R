#' @name deleteEvents
#' @title Delete Events from a Project
#' 
#' @description This method allows you to delete Events from a project.
#'   Notice: Because of this method's destructive nature, it is only 
#'   available for use for projects in Development status. Additionally, 
#'   please be aware that deleting an event will automatically delete 
#'   any records/data that have been collected under that event 
#'   (this is non-reversible data loss).
#'   
#' @param rcon A \code{redcapConnection} object.
#' @param events \code{character} vector giving the unique event names
#'   of the events to be deleted.
#' @param refresh \code{logical(1)} If \code{TRUE}, cached event data will be 
#'   refreshed after the deletion.
#' @param ... Additional arguments to pass to other methods.
#' @param error_handling An option for how to handle errors returned by the API.
#'   see \code{\link{redcap_error}}
#' @param config \code{list} Additional configuration parameters to pass to 
#'   \code{\link[httr]{POST}}. These are appended to any parameters in 
#'   \code{rcon$config}.
#' @param api_param \code{list} Additional API parameters to pass into the
#'   body of the API call. This provides users to execute calls with options
#'   that may not otherwise be supported by \code{redcapAPI}.
#'   
#' @export

deleteEvents <- function(rcon, 
                         events = NULL, 
                         ...){
  UseMethod("deleteEvents")
}

#' @rdname deleteEvents
#' @export

deleteEvents <- function(rcon, 
                         events         = NULL, 
                         refresh        = TRUE,
                         ..., 
                         error_handling = getOption("redcap_error_handling"), 
                         config         = list(), 
                         api_param      = list()){
  ###################################################################
  # Argument validation
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::assert_character(x = events, 
                              any.missing = FALSE, 
                              null.ok = TRUE, 
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
  
  ###################################################################
  # Make the API Body List
  
  body <- c(list(content = 'event', 
                 action = 'delete'), 
            vectorToApiBodyList(events, "events"))
  
  body <- body[lengths(body) > 0]
  
  ###################################################################
  # Call the API
  
  response <- makeApiCall(rcon, 
                          body = c(body, api_param), 
                          config = config)
  
  if (response$status_code != 200) return(redcap_error(response, error_handling))
  
  message(sprintf("Events deleted: %s", 
                  as.character(response)))
  
  if (refresh && rcon$has_events()){
    rcon$refresh_events()
  }
}

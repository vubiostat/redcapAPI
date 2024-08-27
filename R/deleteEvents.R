#' @describeIn eventsMethods Delete events from a project.
#' @order 3
#' @export

deleteEvents <- function(rcon, 
                         events = NULL, 
                         ...){
  UseMethod("deleteEvents")
}

#' @rdname eventsMethods
#' @order 6
#' @export

deleteEvents.redcapApiConnection <- function(rcon, 
                                             events         = NULL,
                                             ...)
{
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

  checkmate::reportAssertions(coll)
  
  ###################################################################
  # Make the API Body List
  
  body <- c(list(content = 'event', 
                 action = 'delete'), 
            vectorToApiBodyList(events, "events"))

  ###################################################################
  # Call the API
  rcon$flush_events()
  rcon$flush_arms()
  rcon$flush_projectInformation()
  invisible(as.character(makeApiCall(rcon, body, ...)))
}

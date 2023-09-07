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
  
  if (response$status_code != 200) return(redcapError(response, error_handling))
  
  message(sprintf("Events deleted: %s", 
                  as.character(response)))
  
  if (refresh && rcon$has_events()){
    rcon$refresh_events()
    rcon$refresh_projectInformation()
  }
}

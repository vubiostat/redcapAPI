#' @describeIn eventsMethods Add events to a project or modify existing events.
#' @order 2
#' @export

importEvents <- function(rcon, 
                         data, 
                         override = FALSE, 
                         ...){
  UseMethod("importEvents")
}

#' @rdname eventsMethods
#' @order 5
#' @export

importEvents.redcapApiConnection <- function(rcon, 
                                             data, 
                                             override       = FALSE, 
                                             refresh        = TRUE,
                                             ..., 
                                             error_handling = getOption("redcap_error_handling"), 
                                             config         = list(), 
                                             api_param      = list()){
  
  dots <- list(...)
  if ("event_data" %in% names(dots)) data <- dots$event_data
  
  ###################################################################
  # Argument Validation
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::assert_data_frame(x = data, 
                               add = coll)
  
  checkmate::assert_logical(x = override, 
                            len = 1, 
                            any.missing = FALSE, 
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
  
  checkmate::assert_subset(x = names(data), 
                           choices = names(REDCAP_EVENT_STRUCTURE), # Defined in redcapDataStructure.R 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  ###################################################################
  # Make API Body List
  
  body <- list(content = "event", 
               action = "import", 
               override = as.numeric(override), 
               format = "csv", 
               returnFormat = "csv", 
               data = writeDataForImport(data))
  
  body <- body[lengths(body) > 0]
  
  ###################################################################
  # Call the API
  
  response <- makeApiCall(rcon, 
                          body = c(body, api_param), 
                          config = config)
  
  if (response$status_code != 200) return(redcapError(response, error_handling))
  
  message(sprintf("Events imported: %s", 
                  as.character(response)))
  
  if (refresh && rcon$has_events()){
    rcon$refresh_events()
    rcon$refresh_projectInformation()
  }
}

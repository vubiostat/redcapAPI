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
                                             ...)
{
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

  ###################################################################
  # Call the API
  rcon$flush_events()
  rcon$flush_arms()
  rcon$flush_projectInformation()
  invisible(as.character(
    makeApiCall(rcon, body, ...)
  ))
}

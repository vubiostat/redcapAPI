#' @describeIn mappingMethods Import instrument-event mappings to the project.
#' @order 2
#' @export

importMappings <- function(rcon, 
                           data,  
                           ...){
  UseMethod("importMappings")
}

#' @rdname mappingMethods
#' @order 4
#' @export

importMappings.redcapApiConnection <- function(rcon, 
                                               data, 
                                               ...)
{
  ###################################################################
  # Argument Validation                                          ####
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::assert_data_frame(x = data, 
                               col.names = "named", 
                               add = coll)

  checkmate::reportAssertions(coll)
  
  checkmate::assert_subset(x = names(data), 
                           choices = names(rcon$mapping()), 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  checkmate::assert_subset(x = data$arm_num, 
                           choices = rcon$arms()$arm_num, 
                           add = coll)
  
  checkmate::assert_subset(x = data$unique_event_name, 
                           choices = rcon$events()$unique_event_name, 
                           add = coll)
  
  checkmate::assert_subset(x = data$form, 
                           choices = rcon$instruments()$instrument_name, 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  ###################################################################
  # Make the API Body List                                       ####
  
  body <- list(content = "formEventMapping", 
               format = "csv", 
               returnFormat = "csv", 
               data = writeDataForImport(data))

  ###################################################################
  # Call the API                                                 ####
  rcon$flush_mapping()  
  invisible(as.character(
    makeApiCall(rcon, body, ...)
  ))
}

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
                                               ..., 
                                               error_handling  = getOption("redcap_error_handling"),
                                               config          = list(), 
                                               api_param       = list()){
  ###################################################################
  # Argument Validation                                          ####
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::assert_data_frame(x = data, 
                               col.names = "named", 
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
  
  body <- body[lengths(body) > 0]
  
  ###################################################################
  # Call the API                                                 ####
  
  response <- makeApiCall(rcon, 
                          body = c(body, api_param), 
                          config = config)
  rcon$flush_mapping()
  
  if (response$status_code != "200")
    redcapError(response, error_handling)
  
  invisible(as.character(response))
}

#' @describeIn repeatingInstrumentMethods Import repeating instruments and events.
#' @order 2
#' @export

importRepeatingInstrumentsEvents <- function(rcon, 
                                             data, 
                                             ...){
  UseMethod("importRepeatingInstrumentsEvents")
}

#' @rdname repeatingInstrumentMethods
#' @order 4
#' @export

importRepeatingInstrumentsEvents.redcapApiConnection <- function(rcon, 
                                                                 data, 
                                                                 refresh = TRUE, 
                                                                 ..., 
                                                                 error_handling = getOption("redcap_error_handling"), 
                                                                 config         = list(), 
                                                                 api_param      = list()){
  ###################################################################
  # Argument Validation                                          ####
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::assert_data_frame(x = data, 
                               col.names = "named",
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
                           choices = names(REDCAP_REPEAT_INSTRUMENT_STRUCTURE), 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  ###################################################################
  # Make the Body List                                           ####
  
  body <- list(content = "repeatingFormsEvents", 
               format = "csv", 
               returnFormat = "csv", 
               data = writeDataForImport(data))
  
  body <- body[lengths(body) > 0]
  
  ###################################################################
  # Call the API                                                 ####
  
  response <- makeApiCall(rcon, 
                          body = c(body, api_param), 
                          config = config)
  
  if (response$status_code != 200) return(redcapError(response, error_handling))
  
  message(sprintf("Rows imported: %s", 
                  as.character(response)))
  
  if (refresh && rcon$has_repeatInstrumentEvent()){
    rcon$refresh_projectInformation()
    rcon$refresh_repeatInstrumentEvent()
  }
}

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
                           choices = names(REDCAP_REPEAT_INSTRUMENT_STRUCTURE), 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  ###################################################################
  # Make the Body List                                           ####
  
  body <- list(content = "repeatingFormsEvents", 
               format = "csv", 
               returnFormat = "csv", 
               data = writeDataForImport(data))

  ###################################################################
  # Call the API                                                 ####
  rcon$flush_projectInformation()
  rcon$flush_repeatInstrumentEvent()
  invisible(as.character(
    makeApiCall(rcon, body, ...)
  ))
}

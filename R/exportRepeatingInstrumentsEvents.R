#' @describeIn repeatingInstrumentMethods Export repeating instruments and events.
#' @order 1
#' @export

exportRepeatingInstrumentsEvents <- function(rcon, 
                                             ...){
  UseMethod("exportRepeatingInstrumentsEvents")
}

#' @rdname repeatingInstrumentMethods
#' @order 3
#' @export

exportRepeatingInstrumentsEvents.redcapApiConnection <- function(rcon, 
                                                                  ...)
{
  ###################################################################
  # Argument Validation                                          ####
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = c("redcapApiConnection"), 
                          add = coll)

  checkmate::reportAssertions(coll)
  
  checkmate::reportAssertions(coll)
  
  ###################################################################
  # Handle Project w/o repeating instruments                     ####
  
  if (rcon$projectInformation()$has_repeating_instruments_or_events == 0){
    return(REDCAP_REPEAT_INSTRUMENT_STRUCTURE)
  }
  
  ###################################################################
  # Make the Body List                                           ####
  
  body <- list(content = "repeatingFormsEvents", 
               format = "csv")

  ###################################################################
  # Call the API                                                 ####
  as.data.frame(makeApiCall(rcon, body, ...))
}

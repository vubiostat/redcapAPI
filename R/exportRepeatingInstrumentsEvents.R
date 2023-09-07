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
                                                                  ...,
                                                                  error_handling = getOption("redcap_error_handling"), 
                                                                  config = list(), 
                                                                  api_param = list()){
  ###################################################################
  # Argument Validation                                          ####
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = c("redcapApiConnection"), 
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
  
  body <- body[lengths(body) > 0]
  
  ###################################################################
  # Call the API                                                 ####
  
  response <- makeApiCall(rcon, 
                          body = c(body, api_param), 
                          config = config)
  
  if (response$status_code != 200){
    redcapError(response, 
                 error_handling = error_handling)
  }
  
  utils::read.csv(text = as.character(response), 
                  stringsAsFactors = FALSE,
                  na.strings = "")
}

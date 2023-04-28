#' @name exportRepeatingInstrumentsEvents
#' @title Export Repeating Instruments and Events Settings
#' 
#' @description This method allows you to export a list of the repeated 
#'   instruments and repeating events for a project. This includes 
#'   their unique instrument name as seen in the second column of the 
#'   Data Dictionary, as well as each repeating instrument's corresponding 
#'   custom repeating instrument label. For longitudinal projects, the 
#'   unique event name is also returned for each repeating instrument. 
#'   Additionally, repeating events are returned as separate items, in 
#'   which the instrument name will be blank/null to indicate that it is a 
#'   repeating event (rather than a repeating instrument).
#'   
#' @param rcon A \code{redcapConnection} object. 
#' @param ... Arguments to pass to other methodsArguments to be passed to other methods.
#' @param error_handling An option for how to handle errors returned by the API.
#'   see \code{\link{redcap_error}}
#' @param config \code{list} Additional configuration parameters to pass to 
#'   \code{\link[httr]{POST}}. These are appended to any parameters in 
#'   \code{rcon$config}.
#' @param api_param \code{list} Additional API parameters to pass into the
#'   body of the API call. This provides users to execute calls with options
#'   that may not otherwise be supported by \code{redcapAPI}.
#' 
#' @export

exportRepeatingInstrumentsEvents <- function(rcon, 
                                             ...){
  UseMethod("exportRepeatingInstrumentsEvents")
}

#' @rdname exportRepeatingInstrumentsEvents
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
    redcap_error(response, 
                 error_handling = error_handling)
  }
  
  utils::read.csv(text = as.character(response), 
                  stringsAsFactors = FALSE,
                  na.strings = "")
}
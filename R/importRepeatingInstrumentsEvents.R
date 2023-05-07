#' @name importRepeatingInstrumentsEvents
#' @title Import Repeating Instruments and Events Settings
#'
#' @description This method allows you to import a list of the repeated 
#'   instruments and repeating events for a project. This includes their 
#'   unique instrument name as seen in the second column of the Data 
#'   Dictionary, as well as each repeating instrument's corresponding custom 
#'   repeating instrument label. For longitudinal projects, the unique 
#'   event name is also needed for each repeating instrument. Additionally, 
#'   repeating events must be submitted as separate items, in which the 
#'   instrument name will be blank/null to indicate that it is a repeating 
#'   event (rather than a repeating instrument).
#'   
#' @param rcon A \code{redcapConnection} object. 
#' @param data \code{data.frame}. For classical projects, it must have the
#'   columns \code{form_name} and \code{custom_form_label}. Longitudinal
#'   projects also require a column for \code{event_name}.
#' @param refresh \code{logical(1)} If \code{TRUE}, the cached 
#'   value of repeating instruments and events on \code{rcon} will be
#'   refreshed.
#' @param ... additional arguments to pass to other methods.
#' @param error_handling An option for how to handle errors returned by the API.
#'   see \code{\link{redcap_error}}
#' @param config \code{list} Additional configuration parameters to pass to 
#'   \code{\link[httr]{POST}}. These are appended to any parameters in 
#'   \code{rcon$config}.
#' @param api_param \code{list} Additional API parameters to pass into the
#'   body of the API call. This provides users to execute calls with options
#'   that may not otherwise be supported by \code{redcapAPI}.
#'   
#' @details It should be noted that it is \emph{not} possible to update the
#' \code{has_repeating_instruments_or_events} property of the project through
#' \code{importProjectInformation}. Enabling of repeating instruments and 
#' events must be done through the GUI.
#' 
#' Although the API does not provide a delete method, it is possible to 
#' remove settings by doing an import that excludes the settings you wish
#' to remove. All settings can be cleared by executing
#' \code{importRepeatingInstrumentsEvents(rcon, REDCAP_REPEAT_INSTRUMENT_STRUCTURE)}.
#'   
#' @export

importRepeatingInstrumentsEvents <- function(rcon, 
                                             data, 
                                             ...){
  UseMethod("importRepeatingInstrumentsEvents")
}

#' @rdname importRepeatingInstrumentsEvents
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
  
  if (response$status_code != 200) return(redcap_error(response, error_handling))
  
  message(sprintf("Rows imported: %s", 
                  as.character(response)))
  
  if (refresh && rcon$has_repeatInstrumentEvent()){
    rcon$refresh_projectInformation()
    rcon$refresh_repeatInstrumentEvent()
  }
}

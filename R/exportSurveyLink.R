#' @name exportSurveyLink
#' @title Export Survey Link for a Participant
#'
#' @description This method returns a unique survey link (i.e., a URL) in 
#'   plain text format for a specified record and data collection 
#'   instrument (and event, if longitudinal) in a project. If the user 
#'   does not have 'Survey Distribution Tools' privileges, they will not be 
#'   able to use this method, and an error will be returned. If the 
#'   specified data collection instrument has not been enabled as a 
#'   survey in the project, an error will be returned.
#'   
#' @param rcon A \code{redcapConnection} object.
#' @param record \code{character(1)} giving the record ID. May also be 
#'   numeric (will be coerced to character).
#' @param instrument \code{character(1)} giving the survey instrument. This 
#'   must be one of the form names listed in the meta data.
#' @param event \code{character(1)} A unique event name. Only applies to 
#'   longitudinal projects. 
#' @param repeat_instance \code{integerish(1)}, the repeat instance if the
#'   instrument is designated as a repeating instrument. Default value is 1.
#' @param ... Additional arguments to pass to other methods.
#' @param error_handling An option for how to handle errors returned by the API.
#'   see \code{\link{redcapError}}
#' @param config \code{list} Additional configuration parameters to pass to 
#'   \code{\link[httr]{POST}}. These are appended to any parameters in 
#'   \code{rcon$config}.
#' @param api_param \code{list} Additional API parameters to pass into the
#'   body of the API call. This provides users to execute calls with options
#'   that may not otherwise be supported by \code{redcapAPI}.
#'   
#' @export

exportSurveyLink <- function(rcon, 
                             record, 
                             instrument, 
                             event, 
                             repeat_instance = 1, 
                             ...){
  UseMethod("exportSurveyLink")
}

#' @rdname exportSurveyLink
#' @export

exportSurveyLink.redcapApiConnection <- function(rcon, 
                                                 record, 
                                                 instrument, 
                                                 event           = NULL, 
                                                 repeat_instance = 1, 
                                                 ..., 
                                                 error_handling  = getOption("redcap_error_handling"), 
                                                 config          = list(), 
                                                 api_param       = list()){
  
  if (is.numeric(record)) record <- as.character(record)
  
  ###################################################################
  # Argument Validation                                          ####
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapConnection", 
                          add = coll)
  
  checkmate::assert_character(x = record, 
                              len = 1, 
                              any.missing = FALSE, 
                              add = coll)
  
  checkmate::assert_character(x = instrument, 
                              len = 1, 
                              any.missing = FALSE, 
                              add = coll)
  
  checkmate::assert_character(x = event, 
                              len = 1, 
                              any.missing = FALSE, 
                              null.ok = TRUE, 
                              add = coll)
  
  checkmate::assert_integerish(x = repeat_instance, 
                               len = 1, 
                               any.missing = FALSE, 
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
  
  checkmate::assert_subset(x = instrument, 
                           choices = rcon$instruments()$instrument_name, 
                           add = coll)
  
  if (!is.null(event)){
    checkmate::assert_subset(x = event, 
                             choices = rcon$events()$unique_event_name, 
                             add = coll)
  }
  
  checkmate::reportAssertions(coll)
  
  ###################################################################
  # API Body List                                                ####
  
  body <- list(content = "surveyLink", 
               record = record, 
               instrument = instrument, 
               event = event, 
               repeat_instance = repeat_instance, 
               returnFormat = "csv")
  
  body <- body[lengths(body) > 0]
  
  ###################################################################
  # Call the API                                                 ####
  
  response <- makeApiCall(rcon, 
                          body = c(body, api_param), 
                          config = config)
  
  if (response$status_code != 200) return(redcapError(response, error_handling))
  
  as.character(response)
}

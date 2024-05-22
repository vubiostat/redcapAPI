#' @describeIn surveyMethods Export a survey participant's survey instrument link.
#' @order 2
#' @export

exportSurveyLink <- function(rcon, 
                             record, 
                             instrument, 
                             event, 
                             repeat_instance = 1, 
                             ...){
  UseMethod("exportSurveyLink")
}

#' @rdname surveyMethods
#' @order 6
#' @export

exportSurveyLink.redcapApiConnection <- function(rcon, 
                                                 record, 
                                                 instrument, 
                                                 event           = NULL, 
                                                 repeat_instance = 1, 
                                                 ...)
{
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

  ###################################################################
  # Call the API                                                 ####
  as.character(makeApiCall(rcon, body, ...))
}

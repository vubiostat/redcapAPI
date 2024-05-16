#' @describeIn surveyMethods Export survey participants for a survey instrument.
#' @order 1
#' @export

exportSurveyParticipants <- function(rcon, 
                                     instrument, 
                                     event, ...){
  UseMethod("exportSurveyParticipants")
}

#' @rdname surveyMethods
#' @order 5
#' @export

exportSurveyParticipants.redcapApiConnection <- function(rcon, 
                                                         instrument     = NULL, 
                                                         event          = NULL,
                                                         ...)
{
   ##################################################################
  # Argument Validation
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::assert_character(x = instrument, 
                              len = 1, 
                              any.miss = FALSE, 
                              null.ok = TRUE, 
                              add = coll)
  
  checkmate::assert_character(x = event, 
                              len = 1, 
                              any.miss = FALSE, 
                              null.ok = TRUE, 
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
  
   ##################################################################
  # Make API Body List
  
  body <- list(instrument = instrument,
               event = event,
               content = 'participantList',
               format = 'csv', 
               returnFormat = 'csv')

   ##################################################################
  # Call the API
  as.data.frame(makeApiCall(rcon, body, ...))
}

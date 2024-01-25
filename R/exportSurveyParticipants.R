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
                                                         ...,
                                                         error_handling = getOption("redcap_error_handling"),
                                                         config         = list(), 
                                                         api_param      = list()){
  
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
  
  error_handling <- checkmate::matchArg(x = error_handling, 
                                        choices = c("null", "error"), 
                                        add = coll, 
                                        .var.name = "error_handling")
  
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
  
   ##################################################################
  # Make API Body List
  
  body <- list(token=rcon$token, 
               instrument = instrument,
               event = event,
               content = 'participantList',
               format = 'csv', 
               returnFormat = 'csv')
  
  body <- body[lengths(body) > 0]
  
   ##################################################################
  # Call the API
  
  response <- makeApiCall(rcon, 
                          body = c(body, api_param), 
                          config = config)
  
  if (response$status_code != 200) return(redcapError(response, error_handling))
  
  as.data.frame(response)
}

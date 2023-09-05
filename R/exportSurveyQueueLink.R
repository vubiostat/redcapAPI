#' @describeIn surveyMethods Export a survey participant's survey queue link.
#' @order 3
#' @export

exportSurveyQueueLink <- function(rcon, 
                                  record, 
                                  ...){
  UseMethod("exportSurveyQueueLink")
}

#' @rdname surveyMethods
#' @order 7
#' @export 

exportSurveyQueueLink.redcapApiConnection <- function(rcon, 
                                                      record, 
                                                      ..., 
                                                      error_handling = getOption("redcap_error_handling"), 
                                                      config         = list(), 
                                                      api_param      = list()){
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
  
  ###################################################################
  # API Body List                                                ####
  
  body <- list(content = "surveyQueueLink", 
               record = record, 
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

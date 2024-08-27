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

  checkmate::reportAssertions(coll)
  
  ###################################################################
  # API Body List                                                ####
  
  body <- list(content = "surveyQueueLink", 
               record = record, 
               returnFormat = "csv")

  ###################################################################
  # Call the API                                                 ####
  as.character(makeApiCall(rcon, body, ...))
}

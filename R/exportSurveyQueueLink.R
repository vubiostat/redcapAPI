#' @name exportSurveyQueueLink
#' @title Export Survey Queue Link
#' 
#' @description This method returns a unique Survey Queue link (i.e., a URL) 
#'   in plain text format for the specified record in a project that is 
#'   utilizing the Survey Queue feature. If the user does not have 
#'   'Survey Distribution Tools' privileges, they will not be able to use 
#'   this method, and an error will be returned. If the Survey Queue 
#'   feature has not been enabled in the project, an error will be returned.
#'   
#' @param rcon A \code{redcapConnection} object.
#' @param record \code{character(1)} giving the record ID. May also be 
#'   numeric (will be coerced to character).
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

exportSurveyQueueLink <- function(rcon, 
                                  record, 
                                  ...){
  UseMethod("exportSurveyQueueLink")
}

#' @rdname exportSurveyQueueLink
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

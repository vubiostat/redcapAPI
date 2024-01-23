#' @describeIn projectInformationMethods Export project settings.
#' @order 1
#' @export

exportProjectInformation <- function(rcon, 
                                     ...){
  UseMethod("exportProjectInformation")
}

#' @rdname projectInformationMethods
#' @order 3
#' @export

exportProjectInformation.redcapApiConnection <- function(rcon, 
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
  
   ##################################################################
  # Make the Body List
  
  body <- list(token = rcon$token, 
               content = 'project',
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

#' @describeIn recordsManagementMethods Get the ID number for the next record to be created.
#' @order 1
#' @export

exportNextRecordName <- function(rcon, 
                                 ...){
  UseMethod("exportNextRecordName")
}

#' @rdname recordsManagementMethods
#' @order 3
#' @export


exportNextRecordName.redcapApiConnection <- function(rcon, 
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
               content = 'generateNextRecordName')
  
  body <- body[length(body) > 0]
  
   ##################################################################
  # Call the API
  
  response <- makeApiCall(rcon, 
                          body = c(body, api_param), 
                          config = config)
  
  if (response$status_code != 200) redcapError(response, error_handling)
  
  as.numeric(rawToChar(response$content))
}

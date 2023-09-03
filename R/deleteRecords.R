#' @describeIn recordsMethods Delete records from a project.
#' @order 5
#' @export

deleteRecords <- function(rcon, 
                          records, 
                          arm      = NULL,
                          ...){
  UseMethod("deleteRecords")
}

#' @rdname recordsMethods
#' @order 9
#' @export

deleteRecords.redcapApiConnection <- function(rcon, 
                                              records, 
                                              arm            = NULL, 
                                              ...,
                                              error_handling = getOption("redcap_error_handling"), 
                                              config         = list(), 
                                              api_param      = list()){
  
  if (is.numeric(records)) records <- as.character(records)
  if (is.character(arm)) arm <- as.numeric(arm)
  
   ##################################################################
  # Argument Validation
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_character(x = records,
                              any.missing = FALSE,
                              min.len = 1,
                              add = coll)
  
  checkmate::assert_integerish(arm,
                               len = 1, 
                               any.missing = FALSE,
                               null.ok = TRUE,
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
  
  Arms <- rcon$arms()
  
  checkmate::assert_subset(x = arm,
                           choices = Arms$arm_num, 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
   ##################################################################
  # Build the Body List
  
  body <- list(token = rcon$token,
               content = "record",
               action = "delete", 
               arm = arm)
  
  body <- c(body,
            vectorToApiBodyList(vector = records,
                                parameter_name = "records"))

  body <- body[lengths(body) > 0]
  
   ##################################################################
  # Call the API
  
  response <- makeApiCall(rcon, 
                          body = c(body, api_param), 
                          config = config)
  
  if (response$status_code != 200){
    return(redcapError(response, error_handling))
  } 
  
  as.character(response)
}

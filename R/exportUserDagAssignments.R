# Complete documentation in documentation.R
#' @describeIn dagAssignmentMethods Export current User-DAG Assignments
#' @order 1
#' @export

exportUserDagAssignments <- function(rcon, 
                                     ...){
  UseMethod("exportUserDagAssignments")
}

#' @rdname dagAssignmentMethods
#' @export

exportUserDagAssignments.redcapApiConnection <- function(rcon, 
                                                         ..., 
                                                         error_handling = getOption("redcap_error_handling"), 
                                                         config = list(), 
                                                         api_param = list()){
  ###################################################################
  # Argument Validation                                          ####
  
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
  
  ###################################################################
  # Build the body list                                          ####
  
  body <- list(content = "userDagMapping", 
               format = "csv", 
               returnFormat = "csv")
  
  body <- body[lengths(body) > 0]
  
  ###################################################################
  # Make the API Call                                            ####
  
  response <- makeApiCall(rcon, 
                          body = c(body, api_param), 
                          config = config)
  
  if (response$status_code != 200){
    redcapError(response, 
                 error_handling = error_handling)
  }
  
  response <- as.data.frame(response)
  
  if(nrow(response) == 0) REDCAP_DAG_ASSIGNMENT_STRUCTURE else response

}

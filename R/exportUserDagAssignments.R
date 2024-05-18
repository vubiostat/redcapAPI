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
                                                         ...)
{
  ###################################################################
  # Argument Validation                                          ####
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)

  checkmate::reportAssertions(coll)
  
  ###################################################################
  # Build the body list                                          ####
  
  body <- list(content = "userDagMapping", 
               format = "csv", 
               returnFormat = "csv")

  ###################################################################
  # Make the API Call                                            ####
  
  response <- as.data.frame(makeApiCall(rcon, body, ...))

  if(nrow(response) == 0) REDCAP_DAG_ASSIGNMENT_STRUCTURE else response
}

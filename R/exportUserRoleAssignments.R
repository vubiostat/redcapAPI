#' @describeIn userRoleAssignmentMethods Export user-role assignments from a project.
#' @order 1
#' @export

exportUserRoleAssignments <- function(rcon, ...){
  UseMethod("exportUserRoleAssignments")
}

#' @rdname userRoleAssignmentMethods
#' @order 3
#' @export

exportUserRoleAssignments.redcapApiConnection <- function(rcon, 
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
  # API Body List                                                ####
  
  body <- list(content = "userRoleMapping", 
               format = "csv", 
               returnFormat = "csv")

  ###################################################################
  # Call the API                                                 ####
  
  response <- as.data.frame(makeApiCall(rcon, body, ...))

  if(nrow(response) == 0) REDCAP_USER_ROLE_ASSIGNMENT_STRUCTURE else response
}

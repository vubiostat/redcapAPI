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
  # API Body List                                                ####
  
  body <- list(content = "userRoleMapping", 
               format = "csv", 
               returnFormat = "csv")
  
  body <- body[lengths(body) > 0]
  
  ###################################################################
  # Call the API                                                 ####
  
  response <- makeApiCall(rcon, 
                          body = body, 
                          config = config)
  
  if (response$status_code != 200){
    redcapError(response, 
                error_handling = error_handling)
  }
  
  if (as.character(response) == ""){
    return(REDCAP_USER_ROLE_ASSIGNMENT_STRUCTURE)
  }
  
  utils::read.csv(text = as.character(response), 
                  na.strings = "", 
                  stringsAsFactors = FALSE)
}

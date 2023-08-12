#' @name exportUserRoleAssignments
#' @title Export User-Role Assignments
#' 
#' @description This method allows you to export existing User-Role assignments for a project
#' 
#' @param rcon A \code{redcapConnection} object.
#' @param ... Arguments to be passed to other methods.
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

exportUserRoleAssignments <- function(rcon, ...){
  UseMethod("exportUserRoleAssignments")
}

#' @rdname exportUserRoleAssignments
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
  
  read.csv(text = as.character(response), 
           na.strings = "", 
           stringsAsFactors = FALSE)
}

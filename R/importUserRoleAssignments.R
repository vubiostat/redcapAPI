#' @describeIn userRoleAssignmentMethods Import user-role assignments to a project.
#' @order 2   
#' @export

importUserRoleAssignments <- function(rcon, 
                                      data, 
                                      ...){
  UseMethod("importUserRoleAssignments")
}

#' @rdname userRoleAssignmentMethods
#' @order 4
#' @export

importUserRoleAssignments.redcapApiConnection <- function(rcon, 
                                                          data, 
                                                          ..., 
                                                          error_handling = getOption("redcap_error_handling"), 
                                                          config         = list(), 
                                                          api_param      = list()){
  ###################################################################
  # Argument Validation                                          ####
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::assert_data_frame(x = data, 
                               col.names = "named", 
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
  
  
  
  checkmate::assert_subset(x = names(data), 
                           choices = c("username", "unique_role_name"), 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  
  
  checkmate::assert_subset(x = data$username, 
                           choices = rcon$users()$username, 
                           add = coll)
  
  checkmate::assert_subset(x = data$unique_role_name, 
                           choices = c(NA_character_, rcon$user_roles()$unique_role_name),
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  
  n_username <- table(data$username)
  repeated_username <- n_username[which(n_username > 1)]
  
  if (length(repeated_username) > 0){
    coll$push(sprintf("Each username may only be listed once. Check %s", 
                      paste0(names(repeated_username), collapse = ", ")))
  }
  
  checkmate::reportAssertions(coll)
  
  ###################################################################
  # API Body List                                                ####
  
  body <- list(content = "userRoleMapping", 
               action = "import", 
               format = "csv", 
               returnFormat = "csv", 
               data = writeDataForImport(data))
  
  body <- body[lengths(body) > 0]
  
  ###################################################################
  # Make the API Call                                            ####
  
  response <- makeApiCall(rcon, 
                          body = body, 
                          config = config)
  
  rcon$flush_users()
  rcon$flush_user_role_assignment()
  
  if (response$status_code != 200){
    redcapError(response, 
                error_handling = error_handling)
  }
  
  invisible(as.character(response))
}

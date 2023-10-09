#' @describeIn userRoleMethods Delete user roles from a project.
#' @order 3
#' @export

deleteUserRoles <- function(rcon, 
                            user_roles, 
                            ...){
  UseMethod("deleteUserRoles")
}

#' @rdname userRoleMethods
#' @order 6
#' @export

deleteUserRoles.redcapApiConnection <- function(rcon, 
                                                user_roles,
                                                refresh = TRUE,
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
  
  checkmate::assert_character(x = user_roles, 
                              null.ok = FALSE, 
                              any.missing = FALSE, 
                              add = coll)
  
  checkmate::assert_logical(x = refresh, 
                            len = 1, 
                            null.ok = FALSE, 
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
  
  
  
  checkmate::assert_subset(x = user_roles, 
                           choices = rcon$user_roles()$unique_role_name, 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  ###################################################################
  # Build the body list                                          ####
  
  body <- c(list(content = "userRole", 
               action = "delete"), 
            vectorToApiBodyList(user_roles, "roles"))
  
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
  
  if (refresh){
    rcon$refresh_user_roles()
  }
  
  invisible(as.character(response))
}

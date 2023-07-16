#' @name deleteUserRoles
#' @title Delete User Roles from a Project
#' 
#' @description This method allows you to delete User Roles from a project.
#' 
#' @param rcon A \code{redcapConnection} object.
#' @param user_roles \code{character} unique role names to be deleted from
#'   the project. 
#' @param refresh \code{logical(1)}. If \code{TRUE}, the cached data will
#'   be refreshed after the import.
#' @param ... Arguments to be passed to other methods.
#' @param error_handling An option for how to handle errors returned by the API.
#'   see \code{\link{redcap_error}}
#' @param config \code{list} Additional configuration parameters to pass to 
#'   \code{\link[httr]{POST}}. These are appended to any parameters in 
#'   \code{rcon$config}.
#' @param api_param \code{list} Additional API parameters to pass into the
#'   body of the API call. This provides users to execute calls with options
#'   that may not otherwise be supported by \code{redcapAPI}.
#'   
#' @export

deleteUserRoles <- function(rcon, 
                            user_roles, 
                            ...){
  UseMethod("deleteUserRoles")
}

#' @rdname deleteUserRoles
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
    # FIXME: set up refresh
  }
  
  message(sprintf("User Roles Deleted: %s", as.character(response)))
}

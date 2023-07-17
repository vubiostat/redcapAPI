#' @name importUserRoleAssignments
#' @title Import User-Role Assignments
#' 
#' @description This method allows you to assign users to any user role.
#' 
#' NOTE: If you wish to modify an existing mapping, you must provide its 
#' unique username and role name. If the 'unique_role_name' column is not 
#' provided, user will not assigned to any user role. 
#' There should be only one record per username.
#' 
#' @param rcon A \code{redcapConnection} object. 
#' @param data A \code{data.frame} with columns \code{username} and 
#'   \code{unique_role_name}. Each \code{username} must be unique. 
#'   Users without a \code{unique_role_name} will not be assigned to 
#'   a user role.
#' @param refresh \code{logical(1)}. When \code{TRUE}, the cached value
#'   in \code{rcon} will be refreshed after the import.
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

importUserRoleAssignments <- function(rcon, 
                                      data, 
                                      ...){
  UseMethod("importUserRoleAssignments")
}

#' @rdname importUserRoleAssignments
#' @export

importUserRoleAssignments.redcapApiConnection <- function(rcon, 
                                                          data, 
                                                          refresh        = TRUE, 
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
  
  checkmate::assert_logical(x = refresh, 
                            len = 1, 
                            null.ok = FALSE, 
                            any.missing = FALSE, 
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
  
  if (response$status_code != 200){
    redcapError(response, 
                error_handling = error_handling)
  }
  
  message(sprintf("User-Role Assignments Added/Updated: %s", 
                  as.character(response)))
}

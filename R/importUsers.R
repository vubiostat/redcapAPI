#' @describeIn userMethods Add users or modify user permissions in a project.
#' @order 2
#' @export

importUsers <- function(rcon, data, ...){
  UseMethod("importUsers")
}

#' @rdname userMethods
#' @order 5
#' @export

importUsers.redcapApiConnection <- function(rcon, 
                                            data,
                                            consolidate = TRUE, 
                                            ...)
{
  ###################################################################
  # Argument Validation                                          ####
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::assert_data_frame(x = data, 
                               col.names = "named", 
                               add = coll)
  
  checkmate::assert_logical(x = consolidate, 
                            len = 1, 
                            null.ok = FALSE, 
                            add = coll)

  checkmate::reportAssertions(coll)
  
  form_names <- rcon$instruments()$instrument_name
  form_access_names <- sprintf("%s_form_access", form_names)
  form_export_names <- sprintf("%s_export_access", form_names)
  
  checkmate::assert_subset(x = names(data), 
                           choices = c(names(redcapUserStructure(rcon$version())), 
                                       form_access_names, 
                                       form_export_names, 
                                       "data_export"), 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  data <- prepUserImportData(data,
                             rcon = rcon,
                             consolidate = consolidate)
  
  
  ###################################################################
  # Check for Users Assigned to User Role                        ####
  
  OrigUserRoleAssign <- rcon$user_role_assignment()

  user_conflict_exists <- .importUsers_detectUserRoleConflict(rcon, data)
  
  ###################################################################
  # Build the body list                                          ####
  
  body <- list(content = "user", 
               format = "csv", 
               returnFormat = "csv", 
               data = writeDataForImport(data))

  ###################################################################
  # Make the API Call                                            ####
  rcon$flush_users()
  response <- makeApiCall(rcon, body, ...)
  
  ###################################################################
  # Restore and refresh                                          ####
  if (user_conflict_exists){
    importUserRoleAssignments(rcon, 
                              data = OrigUserRoleAssign[1:2])
  }
  
  invisible(as.character(response))
}


#####################################################################
# Unexported                                                     ####

.importUsers_detectUserRoleConflict <- function(rcon, data){
  UsersAssignedRoles <- rcon$user_role_assignment()
  UsersAssignedRoles <- 
    UsersAssignedRoles[!is.na(UsersAssignedRoles$unique_role_name), ]
  UsersWithConflict <- 
    UsersAssignedRoles[UsersAssignedRoles$username %in% data$username, ]
  
  user_conflict_exists <- nrow(UsersWithConflict) > 0
  
  if (user_conflict_exists){
    UsersWithConflict$unique_role_name <- rep(NA_character_, 
                                              nrow(UsersWithConflict))
    
    importUserRoleAssignments(rcon, 
                              data = UsersWithConflict[1:2])
  }
  
  user_conflict_exists
}

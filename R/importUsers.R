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
  if(!consolidate) {
    extra_access <- intersect(names(data), form_access_names)
    extra_export <- intersect(names(data), form_export_names)
    if(length(extra_access) > 0L) {
      m <- sprintf('Form Access variables [%s] should generally not be set when consolidate = FALSE',
                   paste(extra_access, collapse = ','))
      warning(m)
    }
    if(length(extra_export) > 0L) {
      m <- sprintf('Form Export variables [%s] should generally not be set when consolidate = FALSE',
                   paste(extra_export, collapse = ','))
      warning(m)
    }
  }

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
  
  UsersWithRoles <- rcon$user_role_assignment()[,c('username','unique_role_name')]
  UsersWithRoles <- UsersWithRoles[!is.na(UsersWithRoles$unique_role_name), ]
  UsersWithConflict <- 
    UsersWithRoles[UsersWithRoles$username %in% data[,'username'], ]

  ###################################################################
  # Restore and refresh                                          ####
  if (nrow(UsersWithConflict) > 0){
    # Why is role set to missing before the "user" API call, then reset to original?
    # GH issue 206
    # "Users in roles cannot have their privileges modified via the 'Import User' API method."
    EmptyRoles <- UsersWithConflict
    EmptyRoles$unique_role_name <- NA_character_
    importUserRoleAssignments(rcon, EmptyRoles)

    on.exit(importUserRoleAssignments(rcon, UsersWithConflict),
            add = TRUE)
  }
  
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
  
  invisible(as.character(response))
}

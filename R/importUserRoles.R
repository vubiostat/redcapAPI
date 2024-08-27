#' @describeIn userRoleMethods Import user roles to a project.
#' @order 2
#' @export

importUserRoles <- function(rcon, 
                            data, 
                            ...){
  UseMethod("importUserRoles")
}

#' @rdname userRoleMethods
#' @order 5
#' @export

importUserRoles.redcapApiConnection <- function(rcon, 
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
  
  
  
  checkmate::assert_subset(x = names(data), 
                           choices = names(redcapUserRoleStructure(rcon$version())), 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  data <- prepUserImportData(data,
                             rcon = rcon,
                             consolidate = consolidate, 
                             user_role = TRUE)
  
  ###################################################################
  # Build the body list                                          ####
  
  body <- list(content = "userRole", 
               format = "csv", 
               returnFormat = "csv", 
               data = writeDataForImport(data))

  ###################################################################
  # Make the API Call                                            ####
  
  response <- makeApiCall(rcon, body, ...)

  # From REDCap 14.0.2 forward, caching can sometimes catch an NA role pre-definition
  roles <- c(NA)
  while(length(roles > 0) && any(is.na(roles)))
  {
    rcon$refresh_user_roles()
    roles <- rcon$user_roles()$unique_role_name
  }
  
  invisible(as.character(response))
}

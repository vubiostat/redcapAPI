#' @describeIn userRoleMethods Export user roles from a project. 
#' @order 1
#' @export

exportUserRoles <- function(rcon, ...){
  UseMethod("exportUserRoles")
}

#' @rdname userRoleMethods
#' @order 4
#' @export

exportUserRoles.redcapApiConnection <- function(rcon, 
                                                labels         = TRUE, 
                                                form_rights    = TRUE, 
                                                ...)
{
  ###################################################################
  # Argument Validation                                          ####
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::assert_logical(x = labels, 
                            len = 1, 
                            null.ok = FALSE,
                            add = coll)
  
  checkmate::assert_logical(x = form_rights, 
                            len = 1, 
                            null.ok = FALSE, 
                            add = coll)

  checkmate::reportAssertions(coll)
  
  ###################################################################
  # API body list                                                ####
  
  body <- list(content = "userRole", 
               format = "csv", 
               returnFormat = "csv")
  
  ###################################################################
  # Make API Call                                                ####
  UserRole <- as.data.frame(makeApiCall(rcon, body, ...))

  if (nrow(UserRole) == 0) return(redcapUserRoleStructure(rcon$version()))
 
  # The API returns the forms_export string twice.  We reduce it to once here
  temp <- UserRole$forms_export
  temp <- strsplit(temp, ",")
  temp <- unlist(temp)
  temp <- temp[!duplicated(temp)]
  temp <- paste0(temp, collapse = ",")
  UserRole$forms_export <- temp
  
  ###################################################################
  # Format UserRole properties                                   ####
  
  if (labels){
    UserRole[REDCAP_USER_ROLE_TABLE_ACCESS_VARIABLES] <- 
      lapply(UserRole[REDCAP_USER_ROLE_TABLE_ACCESS_VARIABLES], 
             factor, 
             levels = 0:1, 
             labels = c("No Access", "Access"))
  }
  
  if (form_rights){
    FormAccess <- .exportUsers_separateFormAccess(rcon = rcon, 
                                                  UserRole$forms, 
                                                  nrow = nrow(UserRole),
                                                  export = FALSE)
    ExportAccess <- .exportUsers_separateFormAccess(rcon = rcon, 
                                                    form_access = UserRole$forms_export, 
                                                    nrow = nrow(UserRole), 
                                                    export = TRUE)
    UserRole <- 
      cbind(UserRole, 
            FormAccess, 
            ExportAccess)
    
    if (labels){
      UserRole[names(FormAccess)] <- 
        lapply(UserRole[names(FormAccess)], 
               .exportUsers_labels, 
               type = "form")
      
      UserRole[names(ExportAccess)] <- 
        lapply(UserRole[names(ExportAccess)], 
               .exportUsers_labels, 
               type = "form_export")
    }
    
  }
  
  UserRole
}

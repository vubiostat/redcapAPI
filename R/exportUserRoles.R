#' @name exportUserRoles 
#' @title Export User Roles for a Project
#'
#' @description This method allows you to export the list of user roles for a 
#'   project, including their user privileges.
#'   
#' @param rcon A \code{redcapConnection} object.
#' @param labels \code{logical(1)} Indicates if the data export and form access rights are
#'   converted to factor objects.
#' @param form_rights \code{logical(1)} Indicates if the form rights should be 
#'   transformed to one column per form. The API-provided character string
#'   is always returned with the format [form_name]:[access_code] and a comma separating
#'   each form.
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

exportUserRoles <- function(rcon, ...){
  UseMethod("exportUserRoles")
}

#' @rdname exportUserRoles
#' @export

exportUserRoles <- function(rcon, 
                            labels = TRUE, 
                            form_rights = TRUE, 
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
  
  checkmate::assert_logical(x = labels, 
                            len = 1, 
                            null.ok = FALSE,
                            add = coll)
  
  checkmate::assert_logical(x = form_rights, 
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
  
  ###################################################################
  # API body list                                                ####
  
  body <- list(content = "userRole", 
               format = "csv", 
               returnFormat = "csv")
  
  body <- body[lengths(body) > 0]
  
  ###################################################################
  # Make API Call                                                ####
  
  response <- makeApiCall(rcon, 
                          body = c(body, api_param), 
                          config = config)
  
  if (response$status_code != 200){
    redcapError(response, 
                 error_handling = error_handling)
  }
  
  if (as.character(response) == ""){
    return(REDCAP_USER_ROLE_STRUCTURE)
  }
  
  UserRole <- read.csv(text = as.character(response), 
                       na.strings = "", 
                       stringsAsFactors = FALSE)
  
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

#' @describeIn userMethods Export users affiliated with a project.
#' @order 1
#' @export

exportUsers <- function(rcon, 
                        ...){
  UseMethod("exportUsers")
}

#' @rdname userMethods
#' @order 4
#' @export

exportUsers.redcapApiConnection <- function(rcon, 
                                            dates = TRUE, 
                                            labels = TRUE, 
                                            form_rights = TRUE, 
                                            ...)
{
   ##################################################################
  # Argument Validation
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::assert_logical(x = dates, 
                            len = 1, 
                            add = coll)
  
  checkmate::assert_logical(x = labels, 
                            len = 1, 
                            add = coll)
  
  checkmate::assert_logical(x = form_rights, 
                            len = 1, 
                            add = coll)

  checkmate::reportAssertions(coll)
  
   ##################################################################
  # Build the Body List 
  
  body <- list(content = 'user', 
               format = 'csv', 
               returnFormat = 'csv')

   ##################################################################
  # API Call 
  Users <- as.data.frame(makeApiCall(rcon, body, ...))

  Users$forms_export <- 
    sub(",registration[:]\\d{1}.+$", "", Users$forms_export)
  
   ##################################################################
  # convert expiration date to POSIXct class 
  if (dates){
    Users$expiration <- as.POSIXct(Users$expiration, format="%Y-%m-%d")
  } 
  
   ##################################################################
  # Convert user privileges to labels 
  
  if (labels){
    access_var <- REDCAP_USER_TABLE_ACCESS_VARIABLES # defined in redcapDataStructures.R
    # Just in case the variable names ever change
    access_var <- access_var[access_var %in% names(Users)]
    
    Users[access_var] <- 
      lapply(Users[access_var], 
             .exportUsers_labels, 
             type = "project")
  }
  
   ##################################################################
  # Establish columns for the form rights 
  if (form_rights){
    FormAccess <- .exportUsers_separateFormAccess(rcon = rcon, 
                                                  Users$forms, 
                                                  nrow = nrow(Users),
                                                  export = FALSE)
    ExportAccess <- .exportUsers_separateFormAccess(rcon = rcon, 
                                                    form_access = Users$forms_export, 
                                                    nrow = nrow(Users), 
                                                    export = TRUE)
    Users <- 
      cbind(Users, 
            FormAccess, 
            ExportAccess)
    
    if (labels){
      Users[names(FormAccess)] <- 
        lapply(Users[names(FormAccess)], 
               .exportUsers_labels, 
               type = "form")
      
      Users[names(ExportAccess)] <- 
        lapply(Users[names(ExportAccess)], 
               .exportUsers_labels, 
               type = "form_export")
    }
    
  }
  
  Users
}


#####################################################################
# Unexported 

.exportUsers_separateFormAccess <- function(rcon, form_access, nrow, export = FALSE){
  forms <- unique(rcon$metadata()$form_name)
  
  FormAccess <- replicate(rep(NA_character_, nrow), 
                           n = length(forms), 
                           simplify = FALSE)
  FormAccess <- as.data.frame(FormAccess)
  names(FormAccess) <- sprintf("%s_%s_access", 
                               forms,
                               if (export) "export" else "form")
  
  for (i in seq_along(forms)){
    this_form <- forms[i]
    regex <- sprintf("^(|.+)(%s[:]\\d{1})(|.+)$", 
                     this_form)
    this_access <- sub(regex, "\\2", form_access)
    this_access[!grepl(this_form, this_access)] <- NA_character_
    this_access <- sub(this_form, "", this_access)
    this_access <- sub("[:]", "", this_access)
    this_access <- trimws(this_access)
    FormAccess[[i]] <- as.numeric(this_access)
  }
  
  FormAccess  
}

.exportUsers_labels <- function(x, type = c("project", "form", "form_export")){
  switch(type, 
         "project" = factor(x, 
                            levels = 0:1, 
                            labels = c("No Access", 
                                       "Access")), 
         "form" = factor(x, 
                         levels = c(0, 2, 1, 3), 
                         labels = c("No Access", 
                                    "Read Only", 
                                    "View records/responses and edit records (survey responses are read-only)", 
                                    "Edit survey responses")), 
         "form_export" = factor(x, 
                                levels = c(0, 2, 3, 1), 
                                labels = c("No Access", 
                                           "De-Identified", 
                                           "Remove Identifier Fields", 
                                           "Full Data Set")),
         identity())
}

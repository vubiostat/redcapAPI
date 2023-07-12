#' @name prepUserImportData
#' @title Prepare User Data for Import
#' 
#' @description Prepares a data frame for import via the API. Allows for 
#'   data to be passed in either the raw format or the labelled data 
#'   received from \code{exportUsers}.
#'   
#' @param data \code{data.frame} that has the structure of 
#'   \code{redcapAPI:::REDCAP_USER_STRUCTURE}. It may also have additional 
#'   columns for the form and export access of each of the instruments.
#' @param rcon \code{redcapConnection}. Used to determine the instruments
#'   in the project.
#' @param consolidate \code{logical(1)} If \code{TRUE}, the form and data 
#'   export access values will be read from the expanded columns. Otherwise, 
#'   the consolidated values (as provided by the API export) are utilized.
#'   
#' @export

prepUserImportData <- function(data, 
                               rcon, 
                               consolidate = TRUE){
  
  ###################################################################
  # Argument Validation                                          ####
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_data_frame(x = data, 
                               col.names = "named", 
                               add = coll)
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapConnection", 
                          add = coll)
  
  checkmate::assert_logical(x = consolidate, 
                            len = 1,
                            add = coll)
  
  checkmate::reportAssertions(coll)
  
  primary_fields <- names(data)
  primary_fields <- primary_fields[!grepl("(_export_access|_form_access)$", 
                                          primary_fields)]
  checkmate::assert_subset(x = primary_fields, 
                           choices = names(REDCAP_USER_STRUCTURE), 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  form_access_field <- names(data)[grepl("_form_access$", names(data))]
  export_access_field <- names(data)[grepl("_export_access$", names(data))]
  instrument <- rcon$instruments()$instrument_name
  
  prepUserImportData_validateAllFormsPresent(data = data, 
                                             form_access_field = form_access_field, 
                                             export_access_field = export_access_field, 
                                             instrument = instrument,
                                             consolidate = consolidate, 
                                             coll = coll)
  
  checkmate::reportAssertions(coll)

  ###################################################################
  # Functional Code                                              ####
  
  # Convert Expiration to text string, if necessary
  
  if (inherits(data$expiration, "Date") || inherits(data$expiration, "POSIXct")){
    data$expiration <- format(data$expiration, 
                              format = "%Y-%m-%d")
  }
  
  # Remove fields that can't be updated
  
  fields_to_remove <- c("email", "lastname", "firstname", 
                        "data_access_group_id")
  data <- data[!names(data) %in% fields_to_remove]
  
  # Convert values to numeric
  
  for (nm in names(data)){
    data[[nm]] <- 
      if (nm %in% REDCAP_USER_TABLE_ACCESS_VARIABLES){
        prepUserImportData_castAccessVariable(data[[nm]])
      } else if (nm %in% form_access_field){
        prepUserImportData_castFormAccess(data[[nm]])
      } else if (nm %in% export_access_field){
        prepUserImportData_castExportAccess(data[[nm]])
      } else {
        data[[nm]]
      }
  }
  
  # Make Form Access Field
  
  if (consolidate){
    data$forms <- 
      prepUserImportData_consolidateAccess(data[form_access_field], 
                                           "_form_access$")
    
    data$forms_export <- 
      prepUserImportData_consolidateAccess(data[export_access_field], 
                                           "_export_access$")
  }
  
  data <- data[!names(data) %in% c(form_access_field, 
                                   export_access_field)]
  
  data
}

#####################################################################
# Unexported                                                     ####

prepUserImportData_castAccessVariable <- function(x){
  as.numeric(x %in% c(1, "Access"))
}

prepUserImportData_castExportAccess <- function(x){
  x <- as.character(x)
  
  to_zero  <- which(x %in% c(0, "No Access"))
  to_one   <- which(x %in% c(1, "Full Data Set"))
  to_two   <- which(x %in% c(2, "De-Identified"))
  to_three <- which(x %in% c(3, "Remove Identifier Fields"))
  
  x[to_zero]  <- rep(0, length(to_zero))
  x[to_one]   <- rep(1, length(to_one))
  x[to_two]   <- rep(2, length(to_two))
  x[to_three] <- rep(3, length(to_three))

  as.numeric(x)
}

prepUserImportData_castFormAccess <- function(x){
  x <- as.character(x) 
  
  to_zero  <- which(x %in% c(0, "No Access"))
  to_one   <- which(x %in% c(1, "View records/responses and edit records (survey responses are read-only)"))
  to_two   <- which(x %in% c(2, "Read Only"))
  to_three <- which(x %in% c(3, "Edit survey responses"))
  
  x[to_zero]  <- rep(0, length(to_zero))
  x[to_one]   <- rep(1, length(to_one))
  x[to_two]   <- rep(2, length(to_two))
  x[to_three] <- rep(3, length(to_three))
  
  as.numeric(x)
}

prepUserImportData_consolidateAccess <- function(d, suffix){
  for (i in seq_along(d)){
    this_name <- sub(suffix, "", names(d)[i])
    d[[i]] <- sprintf("%s:%s", this_name, d[[i]])
  }
  
  apply(d, MARGIN = 1, FUN = paste0, collapse = ",")
}

prepUserImportData_extractFormName <- function(x, instrument){
  forms <- strsplit(x, ",")
  forms <- lapply(forms, 
                  function(f) sub("[:].+$", "", f))
  
  instrument_present <- logical(length(instrument))
  
  for (i in seq_along(instrument_present)){
    instrument_present[i] <- 
      all(vapply(forms, function(f) instrument[i] %in% forms[[i]], logical(1)))
  }
  
  instrument[instrument_present]
}

prepUserImportData_validateAllFormsPresent <- function(data, 
                                                       form_access_field, 
                                                       export_access_field, 
                                                       instrument,
                                                       consolidate, 
                                                       coll){
  # If consolidating, we need to make sure that all of the forms are present
  # in both form access and data export access.
  if (consolidate){
    form_access_forms <- sub("_form_access$", "", form_access_field)
    export_access_forms <- sub("_export_access$", "", export_access_field)
  }
  
  # if not consolidating forms, we need to make sure all of the forms are 
  # represented in the standard format
  
  if (!consolidate){
    form_access_forms <- prepUserImportData_extractFormName(data$forms, instrument)
    export_access_forms <- prepUserImportData_extractFormName(data$forms_export, instrument)
  }
  
  all_form_access <- all(form_access_forms %in% instrument)
  all_export_access <- all(export_access_forms %in% instrument)
  
  if (!all_form_access){
    msg <- sprintf("At least one user is missing an entry for the form(s): %s", 
                   paste0(setdiff(instrument, all_form_access), collapse = ", "))
    coll$push(msg)
  }
  
  if (!all_export_access){
    msg <- sprintf("At least one user is missing an export entry for the form(s): %s", 
                   paste0(setdiff(instrument, all_export_access), collapse = ", "))
    coll$push(msg)
  }
}
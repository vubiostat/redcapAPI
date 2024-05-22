#' @describeIn projectInformationMethods Import project settings.
#' @order 2
#' @export

importProjectInformation <- function(rcon, 
                                     data, 
                                     ...){
  UseMethod("importProjectInformation")
}

#' @rdname projectInformationMethods
#' @order 4
#' @export

importProjectInformation.redcapApiConnection <- function(rcon, 
                                                         data,
                                                         ...)
{
  ###################################################################
  # Argument Validation
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::assert_data_frame(x = data,
                               nrows = 1,
                               add = coll)

  checkmate::reportAssertions(coll)
  
  # Remove uneditable fields.
  data <- data[!names(data) %in% PROJECT_INFO_FIELDS_FIXED] # defined in redcapDataStructures.R
  
  checkmate::assert_subset(x = names(data), 
                           choices = PROJECT_INFO_FIELDS_EDITABLE, # defined in redcapDataStructures.R
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  .importProjectInformation_validateDataFields(data, coll)
  
  ###################################################################
  # Make API Body List
  
  body <- list(content = "project_settings", 
               format = "csv", 
               data = writeDataForImport(data))

  ###################################################################
  # Call the API
  rcon$flush_arms()
  rcon$flush_events()
  rcon$flush_projectInformation()
  invisible(as.character(
    makeApiCall(rcon, body, ...)
  ))
}

#####################################################################
# Unexported

.importProjectInformation_validateDataFields <- function(data, 
                                                         coll){
  if ("project_title" %in% names(data)){
    checkmate::assert_character(data$project_title, 
                                add = coll, 
                                .var.name = "project_title")
  }
  
  if ("project_language" %in% names(data)){
    checkmate::assert_character(data$project_language, 
                                add = coll, 
                                .var.name = "project_language")
  }
  
  if ("purpose" %in% names(data)){
    checkmate::assert_integerish(x = data$purpose, 
                                 add = coll, 
                                 .var.name = "purpose")
  }
  
  if ("purpose_other" %in% names(data)){
    checkmate::assert_character(x = data$purpose_other, 
                                add = coll, 
                                .var.name = "purpose_other")
  }
  
  if ("project_notes" %in% names(data)){
    checkmate::assert_character(x = data$project_notes, 
                                add = coll, 
                                .var.name = "project_notes")
  }
  
  if ("custom_record_label" %in% names(data)){
    checkmate::assert_character(x = data$custom_record_label, 
                                add = coll, 
                                .var.name = "custom_record_label")
  }
  
  if ("secondary_unique_field" %in% names(data)){
    checkmate::assert_character(x = data$secondary_unique_field, 
                                add = coll, 
                                .var.name = "secondary_unique_field")
  }
  
  if ("is_longitudinal" %in% names(data)){
    if (!all(data$is_longitudinal %in% c(0, 1))){
      coll$push("'is_longitudinal' must be 0/1")
    }
  }
  
  if ("surveys_enabled" %in% names(data)){
    if (!all(data$surveys_enabled %in% c(0, 1))){
      coll$push("'surveys_enabled' must be 0/1")
    }
  }
  
  if ("scheduling_enabled" %in% names(data)){
    if (!all(data$scheduling_enabled %in% c(0, 1))){
      coll$push("'scheduling_enabled' must be 0/1")
    }
  }
  
  if ("record_autonumbering_enabled" %in% names(data)){
    if (!all(data$record_autonumbering_enabled %in% c(0, 1))){
      coll$push("'record_autonumbering_enabled' must be 0/1")
    }
  }
  
  if ("randomization_enabled" %in% names(data)){
    if (!all(data$randomization_enabled %in% c(0, 1))){
      coll$push("'randomization_enabled' must be 0/1")
    }
  }
  
  if ("project_irb_number" %in% names(data)){
    checkmate::assert_character(x = data$project_irb_number, 
                                add = coll, 
                                .var.name = "project_irb_number")
  }
  
  if ("project_grant_number" %in% names(data)){
    checkmate::assert_character(x = data$project_grant_number, 
                                add = coll, 
                                .var.name = "project_grant_number")
  }
  
  if ("project_pi_firstname" %in% names(data)){
    checkmate::assert_character(x = data$project_pi_firstname, 
                                add = coll, 
                                .var.name = "project_pi_firstname")
  }
  
  if ("project_pi_lastname" %in% names(data)){
    checkmate::assert_character(x = data$project_pi_lastname, 
                                add = coll, 
                                .var.name = "project_pi_lastname")
  }
  
  if ("display_today_now_button" %in% names(data)){
    if (!all(data$display_today_now_button %in% c(0, 1))){
      coll$push("'display_today_now_button' must be 0/1")
    }
  }
  
  if ("bypass_branching_erase_field_prompt" %in% names(data)){
    if (!all(data$bypass_branching_erase_field_prompt %in% c(0, 1))){
      coll$push("'bypass_branching_erase_field_prompt' must be 0/1")
    }
  }
  
  checkmate::reportAssertions(coll)
}

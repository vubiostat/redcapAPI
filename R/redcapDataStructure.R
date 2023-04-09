#' @name redcapDataStructures
#' @title REDCap Data Structures
#'
#' @description Utilities for recognizing and validating data structures 
#'   for use with the REDCap API
#'   

validateRedcapData <- function(data, redcap_data){
  if (is.null(data)) return(NULL)
  
  checkmate::assert_data_frame(x = data, 
                               col.names = "named")
  
  in_data_and_not_structure <- setdiff(names(data), 
                                       names(redcap_data))
  
  # names in data not in the redcap structure
  if (length(in_data_and_not_structure) > 0){
    warning("The following names in 'data' are not recognized in the REDCap data structure.\n", 
            "Downstream functions may not operate as expected. {", 
            paste0(in_data_and_not_structure, collapse = ", "), "}\n")
  }
  
  # names in the redcap structure not in the data
  
  in_structure_and_not_data <- setdiff(names(redcap_data), 
                                       names(data))
  
  if (length(in_structure_and_not_data) > 0){
    warning("The following names in the REDCap data structure are not in 'data'. \n", 
            "Downstream functions may not operate as expected. {", 
            paste0(in_structure_and_not_data, collapse = ", "), "}\n")
  }
  return(data)
}

# Arms --------------------------------------------------------------
# Arms - Data Frame Structure

REDCAP_ARMS_STRUCTURE <- 
  data.frame(arm_num = numeric(0), 
             name = character(0), 
             stringsAsFactors = FALSE)

# Data Access Groups ------------------------------------------------
# DAG Structure

REDCAP_DAG_STRUCTURE <- 
  data.frame(data_access_group_name = character(0), 
             unique_group_name = character(0), 
             data_access_group_id = numeric(0), 
             stringsAsFactors = FALSE)

# DAG Assignment Structure

REDCAP_DAG_ASSIGNMENT_STRUCTURE <- 
  data.frame(username = character(0), 
             redcap_data_access_group = character(0), 
             stringsAsFactors = FALSE)

# Events ------------------------------------------------------------
# Event Structure

REDCAP_EVENT_STRUCTURE <- 
  data.frame(event_name = character(0), 
             arm_num = numeric(0), 
             unique_event_name = character(0), 
             custom_event_label = character(0), 
             stringsAsFactors = FALSE)

# Field Names -------------------------------------------------------
# Field Name Structure 

REDCAP_FIELDNAME_STRUCTURE <- 
  data.frame(original_field_name = character(0), 
             choice_value = character(0), 
             export_field_name = character(0), 
             stringsAsFactors = FALSE)

# Instruments -------------------------------------------------------
# Instrument Structure

REDCAP_INSTRUMENT_STRUCTURE <- 
  data.frame(instrument_name = character(0), 
             instrument_label = character(0), 
             stringsAsFactors = FALSE)

# Instrument Mapping Structure 

REDCAP_INSTRUMENT_MAPPING_STRUCTURE <- 
  data.frame(arm_num = numeric(0), 
             unique_event_name = character(0), 
             form = character(0), 
             stringsAsFactors = FALSE)

# Logging -----------------------------------------------------------
# Logging Structure

REDCAP_LOGGING_STRUCTURE <- 
  data.frame(timestamp = as.POSIXct(character(0)), 
             username = character(0), 
             action = character(0), 
             details = character(0), 
             record = character(0), 
             stringsAsFactors = FALSE)

# Meta Data ---------------------------------------------------------
# Meta Data - Data Frame Structure 

REDCAP_METADATA_STRUCTURE <- 
  data.frame(field_name = character(0),
             form_name = character(0),
             section_header = character(0),
             field_type = character(0),
             field_label = character(0),
             select_choices_or_calculations = character(0),
             field_note = character(0),
             text_validation_type_or_show_slider_number = character(0),
             text_validation_min = character(0),
             text_validation_max = character(0),
             identifier = character(0),
             branching_logic = character(0),
             required_field = character(0),
             custom_alignment = character(0),
             question_number = character(0),
             matrix_group_name = character(0),
             matrix_ranking = character(0),
             field_annotation = character(0), 
             stringsAsFactors = FALSE)

# Meta Data - Known Field Types 

REDCAP_METADATA_FIELDTYPE <- c("calc", 
                               "checkbox", 
                               "descriptive", 
                               "dropdown", 
                               "file", 
                               "notes", 
                               "radio", 
                               "slider", 
                               "text", 
                               "truefalse", 
                               "yesno")

# Meta Data - Known Validation Types 

REDCAP_METADATA_VALIDATION_TYPE <- c("date_dmy", 
                                     "date_mdy", 
                                     "date_ymd", 
                                     "datetime_dmy", 
                                     "datetime_mdy", 
                                     "datetime_seconds_dmy", 
                                     "datetime_seconds_mdy", 
                                     "datetime_seconds_ymd", 
                                     "datetime_ymd", 
                                     "email", 
                                     "integer", 
                                     "number", 
                                     "phone", 
                                     "signature", 
                                     "time", 
                                     "zipcode")

# Project Information -----------------------------------------------
# Project Information Structure 

REDCAP_PROJECT_INFORMATION_STRUCTURE <- 
  data.frame(project_id = character(0),
             project_title = character(0),
             creation_time = character(0),
             production_time = character(0),
             in_production = character(0),
             project_language = character(0),
             purpose = character(0),
             purpose_other = character(0),
             project_notes = character(0),
             custom_record_label = character(0),
             secondary_unique_field = character(0),
             is_longitudinal = character(0),
             has_repeating_instruments_or_events = character(0),
             surveys_enabled = character(0),
             scheduling_enabled = character(0),
             record_autonumbering_enabled = character(0),
             randomization_enabled = character(0),
             ddp_enabled = character(0),
             project_irb_number = character(0),
             project_grant_number = character(0),
             project_pi_firstname = character(0),
             project_pi_lastname = character(0),
             display_today_now_button = character(0),
             missing_data_codes = character(0),
             external_modules = character(0),
             bypass_branching_erase_field_prompt = character(0), 
             stringsAsFactors = FALSE)

# Users -------------------------------------------------------------
# Users Structure

REDCAP_USER_STRUCTURE <- 
  data.frame(username = character(0),
             email = character(0),
             firstname = character(0),
             lastname = character(0),
             expiration = as.POSIXct(character(0)),
             data_access_group = character(0),
             data_access_group_id = character(0),
             design = character(0),
             user_rights = character(0),
             data_access_groups = character(0),
             reports = character(0),
             stats_and_charts = character(0),
             manage_survey_participants = character(0),
             calendar = character(0),
             data_import_tool = character(0),
             data_comparison_tool = character(0),
             logging = character(0),
             file_repository = character(0),
             data_quality_create = character(0),
             data_quality_execute = character(0),
             api_export = character(0),
             api_import = character(0),
             mobile_app = character(0),
             mobile_app_download_data = character(0),
             record_create = character(0),
             record_rename = character(0),
             record_delete = character(0),
             lock_records_all_forms = character(0),
             lock_records = character(0),
             lock_records_customization = character(0),
             mycap_participants = character(0),
             random_setup = character(0),
             random_dashboard = character(0),
             random_perform = character(0),
             forms = character(0),
             forms_export = character(0), 
             stringsAsFactors = FALSE)

# User Roles --------------------------------------------------------
# User Role Structure

REDCAP_USER_ROLE_STRUCTURE <- 
  data.frame(unique_role_name = character(0),	
             role_label = character(0), 
             design = character(0),	
             user_rights = character(0),	
             data_access_groups = character(0),
             reports = character(0),
             stats_and_charts = character(0),
             manage_survey_participants = character(0),
             calendar = character(0),
             data_import_tool = character(0),
             data_comparison_tool = character(0),
             logging = character(0),
             file_repository = character(0),
             data_quality_create = character(0),
             data_quality_execute = character(0),
             api_export = character(0),
             api_import = character(0),	
             mobile_app = character(0),
             mobile_app_download_data = character(0),
             record_create = character(0),
             record_rename = character(0),
             record_delete = character(0),
             lock_records_customization = character(0),
             lock_records = character(0),
             lock_records_all_forms = character(0),
             mycap_participants = character(0),
             forms = character(0),
             forms_export = character(0),
             random_setup = character(0),
             random_dashboard = character(0),
             random_perform = character(0),
             stringsAsFactors = FALSE)

# User Role Assignment Structure 

REDCAP_USER_ROLE_ASSIGNMENT_STRUCTURE <- 
  data.frame(username = character(0), 
             unique_role_name = character(0), 
             stringsAsFactors = FALSE)

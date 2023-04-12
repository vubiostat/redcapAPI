#' @name importMetaData
#' @title Import Meta Data (Data Dictionary) to a Project
#' 
#' @description This method allows you to import metadata (i.e., Data Dictionary) 
#'   into a project. Notice: Because of this method's destructive nature, 
#'   it is only available for use for projects in Development status.
#'   
#' @param rcon a \code{redcapConnection} object. 
#' @param data \code{data.frame} with the Meta Data to import. 
#' @param ... Additional arguments to pass to other methods.
#' @param field_types \code{character} giving the acceptable field types
#'   when validating the \code{field_type} column. 
#' @param validation_types \code{character} giving the acceptable values 
#'   for the \code{text_validation_or_show_slider_number} column. 
#' @param config named \code{list}. Additional configuration parameters to pass to \code{httr::POST},
#'   These are appended to any parameters in \code{rcon$config}
#' @param api_param named \code{list}. Additional API parameters to pass into the body of the
#'   API call. This provides users to execute calls with options that may not
#'   otherwise be supported by redcapAPI.
#'   
#' @details Field names may start with a letter, have any number of letters, 
#'   numbers, or underscores, and end in either a letter or a number. All 
#'   letters must be lowercase (the function will coerce them to lower before
#'   checking for duplicate field names). 
#'   
#' Form names may start with a letter, have any number of letters, 
#'   numbers, or underscores, and end in either a letter or a number. All 
#'   letters must be lowercase (the function will coerce them to lower before
#'   checking for duplicate field names).
#'   
#' Field types may be one of \code{REDCAP_FIELD_TYPES}. In the event that a 
#'   new field type is added to REDCap and \code{redcapAPI} isn't yet updated, 
#'   you may add additional values via \code{c(REDCAP_FIELD_TYPES, "new_type")}.
#'   
#' Validation types may be one of \code{REDCAP_METADATA_VALIDATION_TYPE} or 
#'  \code{NA}. AS with field types, additional values can be appended if
#'  necessary. Only fields that have a field type of "text" or "slider" 
#'  should have a validation type. "slider" fields should be either \code{NA}
#'  (do not display the selected number) or \code{"number"}.
#'  
#' For multiple choice fields, the selection choices take the format of 
#' \code{"code1, label1 | ... | coden, labeln"}. For slider fields, the 
#' format is \code{"left_value | mid_value | right_value"}. Any of those 
#' values may be an empty character, but the two pipes are required, nonetheless.
#' 
#' For calculated fields, the values in \code{"select_choices_or_calculations"}
#'   are currently unvalidated.  
#' 
#' All of the values between brackets in the branching logic must be either a
#' field name or an existing unique event name (such as \code{"event_1_arm_1"})
#' 
#' @export

importMetaData <- function(rcon, 
                           data, 
                           ...){
  UseMethod("importMetaData")
}

#' @rdname importMetaData
#' @export

importMetaData.redcapApiConnection <- function(rcon, 
                                               data, 
                                               ..., 
                                               field_types = REDCAP_FIELD_TYPES, # exported constant - see redcapDataStructure
                                               validation_types = REDCAP_METADATA_VALIDATION_TYPE, # exported constant - see redcapDataStructure
                                               config = list(), 
                                               api_param = list()){
  ###################################################################
  # Argument Validation 
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::assert_data_frame(x = data, 
                               add = coll)
  
  checkmate::assert_character(x = field_types, 
                              add = coll)
  
  checkmate::assert_character(x = validation_types, 
                              add = coll)
  
  checkmate::assert_list(x = config, 
                         names = "named", 
                         add = coll)
  
  checkmate::assert_list(x = api_param, 
                         names = "named", 
                         add = coll)
  
  checkmate::reportAssertions(coll)
  
  ###################################################################
  # Data Validations
  
  # Data has correct column names
  
  checkmate::assert_subset(x = names(data), 
                           choices = names(REDCAP_METADATA_STRUCTURE), 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Check for duplicate field names
  data$field_name <- tolower(data$field_name)
  data$form_name <- tolower(data$form_name)
  
  duplicate_field_name <- data$field_name[duplicated(data$field_name)]
  
  if (any(duplicate_field_name)){
    msg <- sprintf("The following have duplicate field names: {%s}", 
                   duplicate_field_name)
    coll$push(msg)
  }
  
  
  
  # field_names are valid
  is_valid_field_name <- 
    grepl("^[a-z][a-z,0-9,_]+[a-z,0-9]$", data$field_name)
  
  if (any(!is_valid_field_name)){
    msg <- sprintf("The following field names do not conform to REDCap field name standards: {%s}", 
                   paste0(data$field_name[!is_valid_field_name], 
                          collapse = ", "))
    coll$push(msg)
  }
  
  
  
  # form_names are valid 
  is_valid_form_name <- 
    grepl("^[a-z][a-z,0-9,_]+[a-z,0-9]$", data$form_name)
  
  if (any(!is_valid_form_name)){
    msg <- sprintf("The following form names do not conform to REDCap form name standards: {%s}", 
                   paste0(data$form_name[!is_valid_form_name], 
                          collapse = ", "))
    coll$push(msg)
  }
  
  
  
  # field_types are valid
  is_valid_field_type <- data$field_type %in% field_types
  
  if (any(!is_valid_field_type)){
    msg <- sprintf("The following field types are not valid field types: {%s}", 
                   paste0(data$field_type[!is_valid_field_type], 
                          collapse = ", "))
    coll$push(msg)
  }
  
  
  
  # field validation types are valid
  is_valid_field_validation <- 
    data$text_validation_type_or_show_slider_number %in% 
    c(validation_types, NA_character_)
  
  if (any(!is_valid_field_validation)){
    msg <- sprintf("The following 'text_validation_type_or_show_slider_number' values are not valid {%s}", 
                   paste0(data$text_validation_type_or_show_slider_number[!is_valid_field_validation], 
                          collapse = ", "))
    coll$push(msg)
  }
  
  
  
  # field validation type only applied to text or slider
  w <- which(!data$field_type %in% c("text", "slider") & 
               !is.na(data$text_validation_type_or_show_slider_number))
  field_shouldnt_have_validation <- data$field_name[w]
  if (length(field_shouldnt_have_validation) > 0){
    msg <- sprintf("The following fields should not have a value in 'text_validation_type_or_show_slider_number: {%s}", 
                   field_shoudnt_have_validation)
    coll$push(msg)
  }
  
  
  
  # field choices only applied to calc, radio, dropdown, checkbox, and slider
  w <- which(!data$field_type %in% c("calc", "checkbox", "dropdown", "radio", "slider") & 
               !is.na(data$select_choices_or_calculations))
  field_shouldnt_have_choice <- data$field_name[w]
  if (length(field_shouldnt_have_choice) > 0){
    msg <- sprintf("The following fields should not have a value in 'select_choices_or_calculations: {%s}", 
                   field_shouldnt_have_choice)
    coll$push(msg)
  }
  
  
  
  # validate choices for multiple choice and slider 
  # make a logical. Assume all are valid
  is_valid_select <- rep(TRUE, nrow(data))
  
  # for multiple choice fields, update is_valid_select for invalid entries
  w_mult <- which(data$field_type %in% c("checkbox", "dropdown", "radio"))
  is_valid_mult <- grepl(REGEX_MULT_CHOICE, # defined in constants.R 
                         data$select_choices_or_calculations[w_mult])
  is_valid_select[w_mult] <- is_valid_mult
  
  # for slider fields, update is_valid_select for invalid entries
  w_slide <- which(data$field_type %in% "slider")
  is_valid_slide <- grepl(REGEX_SLIDE, # defined in constants.R
                          data$select_choices_or_calculations[w_slide])
  is_valid_select[w_slid] <- is_valid_slide
      
  # report the results
  if (any(!is_valid_slide)){
    msg <- sprintf("The following fields have invalid 'select_choices_or_calculations': {%s}", 
                   data$field_name[!is_valid_slide])
    coll$push(msg)
  }
  
  checkmate::reportAssertions(coll)
  
  ###################################################################
  # Build the body list
  
  body <- list(content = 'metadata', 
               format = 'csv', 
               returnFormat = 'csv', 
               data = writeDataForImport(data))
  
  body <- body[lengths(body) > 0]
  
  ###################################################################
  # Call the API
  
  response <- makeApiCall(rcon, 
                          body = c(body, api_param), 
                          config = config)
  
  response <- as.character(response)
}
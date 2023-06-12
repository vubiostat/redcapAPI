#' @name importMetaData
#' @title Import Meta Data (Data Dictionary) to a Project
#' 
#' @description This method allows you to import metadata (i.e., Data Dictionary) 
#'   into a project. Notice: Because of this method's destructive nature, 
#'   it is only available for use for projects in Development status.
#'   
#' @param rcon a \code{redcapConnection} object. 
#' @param data \code{data.frame} with the Meta Data to import. 
#' @param refresh \code{logical(1)}. When \code{TRUE}, the cached metadata
#'   and instruments will be refreshed after the import.
#' @param ... Additional arguments to pass to other methods.
#' @param field_types \code{character} giving the acceptable field types
#'   when validating the \code{field_type} column. 
#' @param validation_types \code{character} giving the acceptable values 
#'   for the \code{text_validation_or_show_slider_number} column. 
#' @param error_handling An option for how to handle errors returned by the API.
#'   see \code{\link{redcap_error}}
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
#' Field types may be one of \code{REDCAP_METADATA_FIELDTYPE}. In the event that a 
#'   new field type is added to REDCap and \code{redcapAPI} isn't yet updated, 
#'   you may add additional values via \code{c(REDCAP_METADATA_FIELDTYPE, "new_type")}.
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
                                               refresh = TRUE,
                                               ..., 
                                               field_types = REDCAP_METADATA_FIELDTYPE, # see redcapDataStructure
                                               validation_types = REDCAP_METADATA_VALIDATION_TYPE, # see redcapDataStructure
                                               error_handling = getOption("redcap_error_handling"), 
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
  
  checkmate::assert_logical(x = refresh, 
                            len = 1, 
                            any.missing = FALSE,
                            add = coll)
  
  checkmate::assert_character(x = field_types, 
                              add = coll)
  
  checkmate::assert_character(x = validation_types, 
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
  # Data Validations
  
  # Data has correct column names
  
  checkmate::assert_subset(x = names(data), 
                           choices = names(REDCAP_METADATA_STRUCTURE), 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Check for duplicate field names
  data$field_name <- trimws(tolower(data$field_name))
  data$form_name <- trimws(tolower(data$form_name))
  
  duplicate_field_name <- data$field_name[duplicated(data$field_name)]
  
  if (any(duplicate_field_name)){
    coll$push(sprintf("The following have duplicate field names: {%s}", 
                      duplicate_field_name))
  }

  isValidFieldName(field_name = data$field_name, 
                   coll = coll)
  
  isValidFormName(form_name = data$form_name,
                  coll = coll)
  
  isValidFieldType(field_type = data$field_type,
                   acceptable_field_types = field_types,
                   coll = coll)
  
  isValidFieldValidationType(field_validation = data$text_validation_type_or_show_slider_number, 
                             validation_types = validation_types, 
                             allow_na = TRUE,
                             coll = coll)
  
  isValidChoiceField(field_name = data$field_name, 
                     field_type = data$field_type, 
                     choices = data$select_choices_or_calculations, 
                     coll = coll)
  
  .isPropertyOnAppropriateField(field_name = data$field_name, 
                                field_type = data$field_type, 
                                permissible_field_type = c("text", "slider", "file"),
                                property = data$text_validation_type_or_show_slider_number, 
                                property_name = "text_validation_type_or_show_slider_number", 
                                coll = coll)

  .isPropertyOnAppropriateField(field_name = data$field_name, 
                                field_type = data$field_type, 
                                permissible_field_type = c("dropdown", "radio", "checkbox", "slider", "calc"),
                                property = data$select_choices_or_calculations, 
                                property_name = "select_choices_or_calculations", 
                                coll = coll)

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
  
  message(sprintf("Fields Imported: %s", response))
  
  if (refresh){
    if (rcon$has_metadata()){
      rcon$refresh_metadata()
    }
    
    if (rcon$has_instruments()){
      rcon$refresh_instruments()
    }
  }
}

#####################################################################
# Unexported
# Although I'm debating if some of these might be useful exports
# Perhaps a validationUtility file?

isValidFieldName <- function(field_name, 
                                coll = NULL){
  is_valid_field_name <- 
    grepl(REGEX_FIELD_NAME, # defined in constants.R
          field_name, 
          perl = TRUE)
  
  # if a collection object is used, push the message to it, otherwise
  # return the logical vector
  if (any(!is_valid_field_name) & !is.null(coll)){
    coll$push(sprintf("The following field names do not conform to REDCap field name standards: {%s}", 
                      paste0(field_name[!is_valid_field_name], 
                             collapse = ", ")))
  }
  
  is_valid_field_name
}



isValidFormName <- function(form_name, coll = NULL){
  is_valid_form_name <- 
    grepl(REGEX_FORM_NAME, # defined in constants.R 
          form_name, 
          perl = TRUE)
  
  if (any(!is_valid_form_name) && !is.null(coll)){
    coll$push(sprintf("The following form names do not conform to REDCap form name standards: {%s}", 
                      paste0(form_name[!is_valid_form_name], 
                             collapse = ", ")))
  }
  
  is_valid_form_name
}



isValidFieldType <- function(field_type, 
                             acceptable_field_types, 
                             coll = NULL){
  is_valid_field_type <- field_type %in% acceptable_field_types
  
  if (any(!is_valid_field_type) & !is.null(coll)){
    coll$push(sprintf("The following field types are not valid field types: {%s}", 
                      paste0(field_type[!is_valid_field_type], 
                             collapse = ", ")))
  }
  
  is_valid_field_type
}



isValidFieldValidationType <- function(field_validation, 
                                       validation_types = REDCAP_METADATA_VALIDATION_TYPE, # see redcapDataStructure, 
                                       allow_na = TRUE,
                                       coll = NULL){
  if (allow_na) validation_types <- c(validation_types, NA_character_)
  
  is_valid_field_validation <- field_validation %in% validation_types
  
  if (any(!is_valid_field_validation) && !is.null(coll)){
    coll$push(sprintf("The following 'text_validation_type_or_show_slider_number' values are not valid {%s}", 
                      paste0(field_validation[!is_valid_field_validation], 
                             collapse = ", ")))
  }
  
  is_valid_field_validation
}



.isPropertyOnAppropriateField <- function(field_name, 
                                          field_type, 
                                          property, 
                                          permissible_field_type,
                                          property_name,
                                          coll = NULL){
  is_correct <-  
    ((field_type %in% permissible_field_type)) | 
     (!(field_type %in% permissible_field_type) & is.na(property))
  
  
  field_shouldnt_have_property <- field_name[!is_correct]
  
  if (length(field_shouldnt_have_property) > 0 && !is.null(coll)){
    coll$push(sprintf("The following fields should not have a value in '%s': {%s}", 
                      property_name,
                      field_shouldnt_have_property))
  }
  
  is_correct
}


isValidChoiceField <- function(field_name, 
                               field_type, 
                               choices, 
                               coll = NULL){
  # make a logical. Assume all are valid to start
  is_valid_select <- rep(TRUE, length(field_name))
  
  # for multiple choice fields, update is_valid_select for invalid entries
  w_mult <- which(field_type %in% c("checkbox", "dropdown", "radio"))
  is_valid_mult <- grepl(REGEX_MULT_CHOICE_STRICT, # defined in constants.R 
                         choices[w_mult])
  is_valid_select[w_mult] <- is_valid_mult                  # set invalid rows
  
  # for slider fields, update is_valid_select for invalid entries
  w_slide <- which(field_type %in% "slider")
  is_valid_slide <- grepl(REGEX_SLIDER, # defined in constants.R
                          choices[w_slide]) | is.na(choices[w_slide])
  is_valid_select[w_slide] <- is_valid_slide                # set invalid rows
  
  # report the results
  if (any(!is_valid_select) && !is.null(coll)){
    coll$push(sprintf("The following fields have invalid 'select_choices_or_calculations': {%s}", 
                      paste0(field_name[!is_valid_select], collapse = ", ")))
  }
  
  is_valid_select
}

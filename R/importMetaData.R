#' @describeIn metaDataMethods Import New Meta Data (Data Dictionary) Definitions 
#' @order 2
#' @export

importMetaData <- function(rcon, 
                           data, 
                           ...){
  UseMethod("importMetaData")
}

#' @rdname metaDataMethods
#' @export

importMetaData.redcapApiConnection <- function(rcon, 
                                               data,
                                               ..., 
                                               field_types = REDCAP_METADATA_FIELDTYPE, # see redcapDataStructure
                                               validation_types = REDCAP_METADATA_VALIDATION_TYPE # see redcapDataStructure
                                               )
{
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
  
    
  # Convert "" to NA
  data[data==''] <- NA

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
                                permissible_field_type = c("text", "slider", "file", "dropdown"),
                                property = data$text_validation_type_or_show_slider_number, 
                                property_name = "text_validation_type_or_show_slider_number", 
                                coll = coll)

  .isPropertyOnAppropriateField(field_name = data$field_name, 
                                field_type = data$field_type, 
                                permissible_field_type = c("dropdown", "radio", "checkbox", "slider", "calc", "text"),
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

  ###################################################################
  # Call the API
  rcon$flush_metadata()
  rcon$flush_instruments()
  rcon$flush_fieldnames()
  invisible(as.character(
    makeApiCall(rcon, body, ...)
  ))
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

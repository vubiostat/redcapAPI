#' @name assembleCodebook
#' @title Assemble Codebook From the Data Dictionary
#' 
#' @description This method enables the user to construct a code book similar
#'   in style to the REDCap project codebook. The codebook is similar in 
#'   nature to the data dictionary, but multiple choice fields are represented
#'   with one line per coding. 
#'   
#' @inheritParams common-rcon-arg
#' @inheritParams common-dot-args
#' @param fields `character` or `NULL`. When `character`, the code book will
#'   be limited to the intersection of the fields designated by `fields`
#'   and `forms`. When `NULL`, all fields are included.
#' @param forms `character` or `NULL`. When `character`, the code book will 
#'   be limited to the intersection of the fields designated by `fields`
#'   and `forms`. When `NULL`, all forms are included.
#' @param drop_fields `character` or `NULL`. When given, fields named 
#'   will be removed from the code book. 
#' @param `field_types` `character` or `NULL`. When given, only the field
#'   types listed will be included in the code book. This will supercede
#'   the intersection of `fields` and `forms`. 
#' @param include_form_complete `logical(1)`. When `TRUE`, the 
#'   `[form name]_complete` fields will be included in the codebook.
#' @param expand_check `logical(1)`. When `FALSE`, the codebook for checkbox 
#'   fields will be similar to the codebook for dropdown and radio fields, 
#'   with one line per user-defined option. When `TRUE`, each checkbox option
#'   will be represented in two fields, one each for 0 (Unchecked) and 
#'   1 (Checked).
#'   
#' @return
#' Returns a data frame with the columns
#' 
#' * `field_name` - The name of the field.
#' * `form` - The name of the form on which the field is located.
#' * `field_type` - The field type.
#' * `code` - For multiple choice fields, the coding for the option.
#' * `label` - For multiple choice fields, the label for the option.
#' * `min` - For date and numeric fields, the minimum value in the validation, if any.
#' * `max` - For date and numeric fields, the maximum value in the validation, if any.
#' * `branching_logic` - For fields with branching logic, the string denoting the logic applied.
#' 
#' @seealso
#' [exportMetaData()]
#' 
#' @examples
#' \dontrun{
#' unlockREDCap(connections = c(rcon = "project_alias"), 
#'              url = "your_redcap_url", 
#'              keyring = "API_KEYs", 
#'              envir = globalenv())
#'              
#' # codebook for the entire project
#' assembleCodebook(rcon)
#' 
#' # codebook for multiple choice fields
#' assembleCodebook(rcon, 
#'                  field_types = c("dropdown", "radio", "checkbox", 
#'                                  "yesno", "truefalse"))
#' }
#' 
#' @export

assembleCodebook <- function(rcon, 
                             fields                = NULL,
                             forms                 = NULL, 
                             drop_fields           = NULL, 
                             field_types           = NULL, 
                             include_form_complete = TRUE, 
                             expand_check          = FALSE, 
                             ...){
  UseMethod("assembleCodebook")
}

#' @rdname assembleCodebook
#' @export

assembleCodebook.redcapConnection <- function(rcon, 
                                              fields                = NULL,
                                              forms                 = NULL, 
                                              drop_fields           = NULL, 
                                              field_types           = NULL, 
                                              include_form_complete = TRUE, 
                                              expand_check          = FALSE, 
                                              ...){
  
  ###################################################################
  # Argument Validation                                          ####
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapConnection", 
                          add = coll)
  
  checkmate::assert_character(x = fields, 
                              null.ok = TRUE, 
                              add = coll)
  
  checkmate::assert_character(x = forms, 
                              null.ok = TRUE, 
                              add = coll)
  
  checkmate::assert_character(x = drop_fields, 
                              null.ok = TRUE, 
                              add = coll)
  
  checkmate::assert_character(x = field_types, 
                              null.ok = TRUE, 
                              add = coll)
  
  checkmate::assert_logical(x = include_form_complete, 
                            len = 1,
                            add = coll)
  
  checkmate::assert_logical(x = expand_check, 
                            len = 1,
                            add = coll)
  
  checkmate::reportAssertions(coll)
  
  .exportRecordsTyped_validateFieldForm(rcon = rcon, 
                                        fields = fields, 
                                        drop_fields = drop_fields, 
                                        forms = forms, 
                                        coll = coll)
  
  ###################################################################
  # Combine fields, drop_fields, and forms into the fields that will 
  # be included in the codebook
  fields <- .exportRecordsTyped_fieldsArray(rcon        = rcon, 
                                            fields      = fields, 
                                            drop_fields = drop_fields, 
                                            forms       = forms)
  
  MetaData <- rcon$metadata()
  MetaData <- MetaData[MetaData$field_name %in% fields, ]
  
  FieldNames <- rcon$fieldnames()
  if (expand_check){
    field_names <- FieldNames$export_field_name[FieldNames$original_field_name %in% fields]
  } else {
    field_names <- FieldNames$original_field_name[FieldNames$original_field_name %in% fields]
  }  
  
  if (!include_form_complete){
    form_name <- unique(rcon$metadata()$form_name)
    form_name <- sprintf("%s_complete", form_name)
    field_names <- field_names[!field_names %in% form_name]
  }

  field_bases <- sub(REGEX_CHECKBOX_FIELD_NAME, #defined in constants.R
                     "\\1", field_names, perl = TRUE)
  field_text_types <- MetaData$text_validation_type_or_show_slider_number[match(field_bases, MetaData$field_name)]
  field_map <- match(field_bases, MetaData$field_name)

  field_types <- .castRecords_getFieldTypes(rcon             = rcon,
                                            field_map        = field_map,
                                            field_bases      = field_bases,
                                            field_text_types = field_text_types)
  form_names <- rep(NA_character_, length(field_names))
  branching_logic <- rep(NA_character_, length(field_names))
  
  for (i in seq_along(form_names)){
    form_names[i] <- 
      if (field_names[i] %in% sprintf("%s_complete", 
                                      MetaData$form_name)){
        sub("_complete$", "", field_names[i])
      } else {
        MetaData$form_name[MetaData$field_name %in% field_bases[i]]
      }
    
    if (length(MetaData$branching_logic[MetaData$field_name %in% field_bases[i]]))
      branching_logic[i] <- 
        MetaData$branching_logic[MetaData$field_name %in% field_bases[i]]
  }
  
  cbind(field_names, field_types)
}

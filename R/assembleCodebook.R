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
#' @param field_types `character` or `NULL`. When given, only the field
#'   types listed will be included in the code book. This will supercede
#'   the intersection of `fields` and `forms`. Matching of field types is 
#'   performed against the values in the `field_type` column of the meta data. 
#' @param include_form_complete `logical(1)`. When `TRUE`, the 
#'   `[form name]_complete` fields will be included in the codebook.
#' @param expand_check `logical(1)`. When `FALSE`, the codebook for checkbox 
#'   fields will be similar to the codebook for dropdown and radio fields, 
#'   with one line per user-defined option. When `TRUE`, each checkbox option
#'   will be represented in two fields, one each for 0 (Unchecked) and 
#'   1 (Checked).
#' @param x A `redcapCodebook` object as returned by `assembleCodebook`.
#'   
#' @return
#' Returns a `redcapCodebook` object. This inherits the `data.frame` class
#' and has the columns
#' 
#' * `field_name` - The name of the field.
#' * `form` - The name of the form on which the field is located.
#' * `field_type` - The field type.
#' * `code` - For multiple choice fields, the coding for the option.
#' * `label` - For multiple choice fields, the label for the option.
#' * `min` - For date and numeric fields, the minimum value in the validation, if any.
#' * `max` - For date and numeric fields, the maximum value in the validation, if any.
#' * `branching_logic` - For fields with branching logic, the string denoting the logic applied.
#' * `field_order` - The numeric order of the field in the data dictionary.
#' * `form_order` - The numeric order of the form in the data dictionary.
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
  
  checkmate::reportAssertions(coll)
  
  ###################################################################
  # Modified Meta Data Object for Project-Level Missing Codes    ####
  # When a project has project-level missing codes, checkboxes
  # have special treatment. In addition to the ___[option] fields
  # defined by the data dictionary, there will also be 
  # ___[missing_code] fields for each project level missing code. 
  # When building the codebook, it is easier if we add the 
  # project level missing code to the options in the 
  # data dictionary. 
  # It is important to use this modified object as meta data 
  # throughout this process and NOT the cached meta data.
  
  MetaData <- rcon$metadata()
  
  if (!is.na(rcon$projectInformation()$missing_data_codes)){
    MetaData$select_choices_or_calculations <- 
      ifelse(MetaData$field_type == "checkbox", 
             sprintf("%s | %s", 
                     MetaData$select_choices_or_calculations, 
                     rcon$projectInformation()$missing_data_codes), 
             MetaData$select_choices_or_calculations)
  }
  
  ###################################################################
  # Combine fields, drop_fields, and forms into the fields that will 
  # be included in the codebook
  
  if (!is.null(field_types)){
    ft_field <- MetaData$field_name[MetaData$field_type %in% field_types]
    fields <- c(fields, ft_field)
  }
  
  field_names <- .exportRecordsTyped_fieldsArray(rcon        = rcon, 
                                                 fields      = fields, 
                                                 drop_fields = drop_fields, 
                                                 forms       = forms, 
                                                 include_descriptive = TRUE, 
                                                 use_original = !expand_check)
  
  UnexpandedCodebook <- .assembleCodebook_unexpanded(field_names, 
                                                     rcon, 
                                                     MetaData)
  
  ExpandedCodebook <- .assembleCodebook_expandCoding(UnexpandedCodebook, rcon, MetaData)
  rownames(ExpandedCodebook) <- NULL
  
  structure(ExpandedCodebook, 
            class = c("redcapCodebook", "data.frame"))
}

#####################################################################
# Unexported                                                     ####

.assembleCodebook_unexpanded <- function(field_names, rcon, MetaData){
  
  # Identify field bases, field tyeps, and mapping order
  field_bases <- sub(REGEX_CHECKBOX_FIELD_NAME, #defined in constants.R
                     "\\1", field_names, perl = TRUE)
  field_text_types <- MetaData$text_validation_type_or_show_slider_number[match(field_bases, MetaData$field_name)]
  field_map <- match(field_bases, MetaData$field_name)
  
  field_types <- .castRecords_getFieldTypes(rcon             = rcon,
                                            field_map        = field_map,
                                            field_bases      = field_bases,
                                            field_text_types = field_text_types)
  

  
  # Assemble the field type data frame
  Codebook <- as.data.frame(cbind(field_name = field_names, 
                                  field_type = field_types, 
                                  field_map, 
                                  field_bases, 
                                  order = seq_along(field_names)), # To force it back to original order
                            stringsAsFactors = FALSE)
  
  # Merge the field types with other relevant data dictionary fields
  Codebook <- merge(Codebook,
                    MetaData[c("field_name",
                               "form_name", 
                               "branching_logic", 
                               "text_validation_min", 
                               "text_validation_max", 
                               "select_choices_or_calculations")],
                    by.x = "field_bases",
                    by.y = "field_name", 
                    all.x = TRUE, 
                    sort = FALSE)
  
  Codebook <- Codebook[order(Codebook$order), ]
  Codebook$order <- NULL
  
  # Populate form name for form_complete fields
  Codebook$form_name <- ifelse(is.na(Codebook$form_name), 
                               sub("_complete$", "", Codebook$field_name), 
                               Codebook$form_name)
  Codebook$form_name <- factor(Codebook$form_name, 
                               unique(rcon$metadata()$form_name))
  
  # Arrange the codebook in the order of the data dictionary
  Codebook <- Codebook[order(Codebook$form_name, Codebook$field_map), ]
  
  Codebook$field_order <- seq_len(nrow(Codebook))
  Codebook$form_order <- as.numeric(Codebook$form_name)
  Codebook$form_name <- as.character(Codebook$form_name)
  
  Codebook
}


.assembleCodebook_expandCoding <- function(Codebook, rcon, MetaData){
  
  # split into each row
  Codebook <- split(Codebook, 
                    Codebook$field_name)
  
  # Get the codings for each row and expand the data frame
  for (i in seq_along(Codebook)){
    this_field_type <- Codebook[[i]]$field_type
    this_field_name <- Codebook[[i]]$field_name
    

    
    coding <- 
      switch(this_field_type, 
             "yesno" = "0, No | 1, Yes",
             "truefalse" = "0, FALSE | 1, TRUE",
             "form_complete" = "0, Incomplete | 1, Unverified | 2, Complete", 
             "dropdown" = Codebook[[i]]$select_choices_or_calculations, 
             "radio" = Codebook[[i]]$select_choices_or_calculations, 
             "checkbox" = if (grepl(REGEX_CHECKBOX_FIELD_NAME, this_field_name)) "0, Unchecked | 1, Checked" else Codebook[[i]]$select_choices_or_calculations, 
             "NA, NA")
    
    # When there are project level missing data codes, add them to the coding
    # Except for checkboxes (those were handled in the modified meta data object)
    if (!this_field_type %in% c("checkbox", "form_complete") && 
        !is.na(rcon$projectInformation()$missing_data_codes) &&
        !this_field_name %in% getProjectIdFields(rcon)){
      coding <- sprintf("%s | %s", 
                        coding, 
                        rcon$projectInformation()$missing_data_codes)
    }
    
    coding <- fieldChoiceMapping(coding, this_field_name)
    
    # if the coding has more than one row, 
    # make n-coding rows of the data and bind it to the coding
    Codebook[[i]] <- 
      cbind(do.call("rbind", 
                    replicate(nrow(coding), 
                              Codebook[[i]], 
                              simplify = FALSE)), 
            coding)
  }
  
  # Reassemble into one data frame
  Codebook <- do.call("rbind", Codebook)
  Codebook <- Codebook[order(Codebook$form_order, 
                             Codebook$field_order), ]
  rownames(Codebook) <- NULL
  
  Codebook$field_labels <- 
    mapply(.castRecords_makeFieldLabel, 
           ifelse(Codebook$field_type == "checkbox" & !grepl(REGEX_CHECKBOX_FIELD_NAME, Codebook$field_name), 
                  sprintf("%s___%s", Codebook$field_name, Codebook$choice_value),
                  Codebook$field_name), 
           as.numeric(Codebook$field_map), 
           MoreArgs = list(MetaData = MetaData), 
           SIMPLIFY = TRUE, 
           USE.NAMES = FALSE)
  
  # Populate field_label for form_complete fields
  Codebook$field_label <- 
    .assembleCodebook_finalizeFieldLabel(field_label = Codebook$field_label, 
                                         field_type = Codebook$field_type, 
                                         form_name = Codebook$form_name, 
                                         rcon)
  
  # Assemble columns in desired order and with the desired names
  Codebook <- Codebook[c("field_name",
                         "field_label",
                         "form_name", 
                         "field_type", 
                         "choice_value", 
                         "choice_label", 
                         "text_validation_min", 
                         "text_validation_max", 
                         "branching_logic", 
                         "field_order", 
                         "form_order")]
  names(Codebook) <- c("field_name",
                       "field_label",
                       "form_name", 
                       "field_type", 
                       "value", 
                       "label", 
                       "min", 
                       "max", 
                       "branching_logic", 
                       "field_order", 
                       "form_order")
  
  Codebook <- rbind(.assembleCodebook_systemField(rcon, "redcap_event_name"),
                    .assembleCodebook_systemField(rcon, "redcap_data_access_group"),
                    .assembleCodebook_systemField(rcon, "redcap_repeat_instrument"),
                    .assembleCodebook_systemField(rcon, "redcap_repeat_instance"),
                    Codebook)
  
  Codebook
}

.assembleCodebook_finalizeFieldLabel <- function(field_label, field_type, form_name, rcon){
  Inst <- rcon$instruments()
  for (i in seq_along(field_label)){
    if (field_type[i] == "form_complete"){
      field_label[i] <- 
        sprintf("%s Complete", 
                Inst$instrument_label[Inst$instrument_name == form_name[i]])
    }
  }
  
  field_label
}

.assembleCodebook_systemField <- function(rcon, field_name){

  # If not repeating instruments are designated, there's no need to include
  # these in the codebook. 
  if (field_name %in% c("redcap_repeat_instrument", "redcap_repeat_instance") && 
      nrow(rcon$repeatInstrumentEvent()) == 0){
    return(NULL)
  }
  
  Coding <- .castRecords_getSystemCoding(field_name, rcon)
  
  if (is.na(Coding)) Coding <- "NA, NA"
  
  if (Coding == "") return(NULL)
  
  Coding <- fieldChoiceMapping(Coding, field_name)
  
  label <- switch(field_name, 
                  "redcap_event_name" = "REDCap Event Name", 
                  "redcap_data_access_group" = "REDCap Data Access Group", 
                  "redcap_repeat_instrument" = "REDCap Repeat Instrument", 
                  "redcap_repeat_instance"   = "REDCap Repeat Instance")
  
  field_ord <- switch(field_name, 
                      "redcap_event_name" = -4, 
                      "redcap_data_access_group" = -3, 
                      "redcap_repeat_instrument" = -2, 
                      "redcap_repeat_instance"   = -1)
  
  data.frame(field_name = rep(field_name, nrow(Coding)), 
             field_label = rep(label, nrow(Coding)), 
             form_name = rep("System Fields", nrow(Coding)), 
             field_type = rep("system_field", nrow(Coding)), 
             value = Coding[, 1], 
             label = Coding[, 2], 
             min = rep(NA_character_, nrow(Coding)), 
             max = rep(NA_character_, nrow(Coding)), 
             branching_logic = rep(NA_character_, nrow(Coding)), 
             field_order = rep(field_ord, nrow(Coding)), 
             form_order = seq_len(nrow(Coding)), 
             stringsAsFactors = FALSE)
}

#' @rdname assembleCodebook
#' @export

as.list.redcapCodebook <- function(x, ...){
  class(x) <- "data.frame"
  Temp <- split(x, x$field_name)
  
  CodeList <- vector("list", length(Temp))
  
  for (i in seq_along(CodeList)){
    CodeList[[i]] <- lapply(Temp[[i]], unique)
  }
  
  CodeList
}

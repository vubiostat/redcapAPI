#' @name metaDataMethods
#' @title Export and Import the Project Meta Data (Data Dictionary)
#' 
#' @description These methods provide the user access to a REDCap project's
#'   data dictionary. The data dictionary may be exported or altered via
#'   the import.
#'
#' @inheritParams common-rcon-arg
#' @inheritParams common-dot-args
#' @inheritParams common-api-args
#' @param fields `character` vector of field names for which the metadata is to 
#'   be retrieved. 
#' @param forms `character` vector of forms for which the metadata is to be
#'   retrieved. If a form name is given, all of the fields on that form
#'   will be returned, regardless of whether it is included in `fields` or 
#'   not.  Form names should match those in the second column of the data 
#'   dictionary, and not the display names shown on the web interface.
#' @param data `data.frame` with the Meta Data to import. 
#' @param field_types `character` giving the acceptable field types
#'   when validating the `field_type` column. This  
#' @param validation_types `character` giving the acceptable values 
#'   for the `text_validation_or_show_slider_number` column.
#'   
#' @details
#' When importing meta data, the following conditions apply:
#' 
#' Field names may start with a letter, have any number of letters, 
#'   numbers, or underscores, and end in either a letter or a number. All 
#'   letters must be lowercase (the function will coerce them to lower before
#'   checking for duplicate field names). 
#'   
#' Form names may start with a letter, have any number of letters, 
#'   numbers, or underscores, and end in either a letter or a number. All 
#'   letters must be lowercase (the function will coerce them to lower before
#'   checking for duplicate field names).
#'   
#' Field types may be one of `REDCAP_METADATA_FIELDTYPE`. In the event that a 
#'   new field type is added to REDCap and `redcapAPI` is not yet updated, 
#'   the user may add additional values via `c(REDCAP_METADATA_FIELDTYPE, "new_type")`.
#'   
#' Validation types may be one of `REDCAP_METADATA_VALIDATION_TYPE` or 
#'  `NA`. As with field types, additional values can be appended if
#'  necessary. Only fields that have a field type of "text" or "slider" 
#'  should have a validation type. "slider" fields should be either `NA`
#'  (do not display the selected number) or `"number"`.
#'  
#' For multiple choice fields, the selection choices take the format of 
#'   `"code1, label1 | ... | coden, labeln"`. For slider fields, the 
#'   format is `"left_value | mid_value | right_value"`. Any of those 
#'   values may be an empty character, but the two pipes are required, nonetheless.
#' 
#' For calculated fields, the values in `"select_choices_or_calculations"`
#'   are currently unvalidated.  
#' 
#' All of the values between brackets in the branching logic must be either a
#'   field name or an existing unique event name (such as `"event_1_arm_1"`)
#'   
#' @return
#' 
#' `exportMetaData` returns a data frame. Not all 18 (or more) columns are
#' documented here, but the most commonly used within `redcapAPI` are 
#' (these may appear in a different order in the data frame):
#' 
#' |                                  |                                    |
#' |----------------------------------|------------------------------------|
#' | `field_name`                     | The name of a field in the project.|
#' | `filed_label`                    | The human-readable form of the field name.|
#' | `form_name`                      | The name of the form on which the field is found.|
#' | `field_type`                     | One of two fields used to determine how a field is transformed into an R object.|
#' | `select_choices_or_calculations` | The second field used to determine how a field is translated into an R object.|
#' | `text_validation_type_or_show_slider_number` | Describes how fields are validated. For slider fields, it gives the limits and center point to display.|
#' | `field_annotation`               | Contains annotations such as units of measures. Also contains action tags. |
#'
#' 
#' `importMetaData` invisibly returns the number of fields that were imported.
#' 
#' @seealso 
#' [exportFieldNames()],\cr
#' [exportInstruments()],\cr
#' [exportMappings()],\cr
#' [importMappings()], \cr
#' [exportPdf()]
#' 
#' @examples
#' \dontrun{
#' unlockREDCap(connections = c(rcon = "project_alias"), 
#'              url = "your_redcap_url", 
#'              keyring = "API_KEYs", 
#'              envir = globalenv())
#'
#' # Export the MetaData from REDCap
#' exportMetaData(rcon)
#' 
#' # Export MetaData for select fields only (returns two rows)
#' exportMetaData(rcon, 
#'                fields = c("dropdown_test", "radio_test"))
#' 
#' # Export MetaData for select forms
#' exportMetaData(rcon, 
#'                forms = c("first_form", "second_form"))
#'                
#' # MetaData my be exported for a combination of fields and forms
#' exportMetaData(rcon, 
#'                fields = c("dropdown_test", "radio_test"), 
#'                forms = c("first_form", "second_form"))
#'                
#' # Alter and import new MetaData (change the record ID label)
#' Meta <- exportMetaData(rcon)
#' 
#' Meta$field_label[1] <- "A better description of the Record ID"
#' importMetaData(rcon, 
#'                data = Meta)
#' }
#' @usage NULL
#' @order 0
# dummy function to control the order of arguments in the help file.
metaDataMethodsArgs <- function(rcon, 
                                fields, 
                                forms, 
                                data,
                                ...,
                                field_types, 
                                validation_types)
{
  NULL
}

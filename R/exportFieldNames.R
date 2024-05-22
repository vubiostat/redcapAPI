#' @name exportFieldNames
#' @title Export the Complete Field Names for a REDCap Project
#' 
#' @description This method enables the user to access the complete field 
#'   names utilized during export and import methods. These are expecially
#'   relevant when working with checkbox fields.
#'   
#' @inheritParams common-rcon-arg
#' @inheritParams common-dot-args
#' @inheritParams common-api-args
#' @param fields `NULL` or `character(1)`. Field name to be returned.  By 
#'   default, all fields are returned.
#' 
#' @details
#' `exportFieldNames` returns a data frame of the field names the user
#' may use when performing export and import functions. This is most useful 
#' when working with checkbox fields, which have a different field name than 
#' the one used in the Meta Data. The exported/imported field names for 
#' checkbox fields have the pattern `[field_name]___[coded_checkbox_value]`
#' (there are exactly three underscores separating the field name and the
#' coded value).
#' 
#' Fields of types "calc", "file", and "descriptive" are not included in the
#' export. (Signature fields also have the "file" type and are not included)
#' 
#' @return 
#' `exportFieldNames` returns a data frame with the columns: 
#' 
#' |                       |                                                  |
#' |-----------------------|--------------------------------------------------|
#' | `original_field_name` | The field name as recorded in the data dictionary|
#' | `choice_value` | represents the raw coded value for a checkbox choice. For non-checkbox fields, this will always be `NA`.|
#' | `export_field_name` | The field name specific to the field. For non-checkbox fields, this is the same as `original_field_name`. For checkbox fields, it is the field name appended with `___[choice_value]`. |
#' 
#' @seealso 
#' [exportMetaData()],\cr
#' [importMetaData()], \cr
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
#' # Export all of the field names
#' exportFieldNames(rcon)
#' 
#' # Export MetaData for a specific field
#' exportFieldNames(rcon, 
#'                  fields = "checkbox_test")
#' }
#' @usage NULL
#' @order 0
# dummy function to control the order of arguments in the help file.
exportFieldNamesArgs <- function(rcon, 
                                 fields, 
                                 ...)
{
  NULL
}

# Complete documentation in documentation.R
#' @rdname exportFieldNames 
#' @export


exportFieldNames <- function(rcon, 
                             ...){
  UseMethod("exportFieldNames")
}

#' @rdname exportFieldNames
#' @export

exportFieldNames.redcapApiConnection <- function(rcon, 
                                                 fields         = character(0), 
                                                 ...)
{
  # Argument validation ---------------------------------------------
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::assert_character(x = fields,
                              max.len = 1,
                              add = coll)

  checkmate::reportAssertions(coll)
  
  if (length(fields) > 0){
    .exportFieldNames_validateFieldName(fields = fields, 
                                        rcon = rcon, 
                                        coll = coll)
  }

  # Build the Body List ---------------------------------------------
  body <- list(content = 'exportFieldNames', 
               format = 'csv',
               returnFormat = 'csv', 
               field = fields)
  
  # Make the API Call -----------------------------------------------
  as.data.frame(makeApiCall(rcon, body, ...))
}

# Unexported --------------------------------------------------------

.exportFieldNames_validateFieldName <- function(fields, rcon, coll){
  # Get project metadata
  MetaData <- rcon$metadata()
  
  if (!all(fields %in% MetaData$field_name)){
    coll$push(sprintf("Field does not exist in the database: %s", 
                      fields))
    checkmate::reportAssertions(coll)
  }
}

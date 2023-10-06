#' @name constructLinkToRedcapForm
#' @title Construct a Link to a REDCap Form
#' 
#' @description Uses information from the project and a record to link to 
#'   the form on which a data element is recorded. This is intended to be used
#'   within the report of invalid results when exporting or importing 
#'   records. It should be noted that when importing records, the records
#'   may not yet exist and the links may not work.
#'
#' @inheritParams common-rcon-arg
#' @param form_name `character(1)`. The name of the form on which the field
#'   name exists.
#' @param record_id `character(1)`. The ID of the record being linked to. 
#'   If passed as a numeric value, it will be coerced to character.
#' @param event_id `character(1)`. The ID of the unique event. If passed
#'   as a numeric value, it will be coerced to character.

constructLinkToRedcapForm <- function(rcon, 
                                      form_name, 
                                      record_id, 
                                      event_id){
  if (is.numeric(record_id)) record_id <- as.character(record_id)
  if (is.numeric(event_id)) event_id <- as.character(event_id)
  
  ###################################################################
  # Argument Validation                                          ####
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::assert_character(x = form_name,
                              len = 1,
                              any.missing = FALSE,
                              add = coll)
  
  checkmate::assert_character(x = record_id,
                              len = 1,
                              any.missing = FALSE,
                              add = coll)
  
  checkmate::assert_character(x = event_id,
                              len = 1,
                              any.missing = TRUE,
                              add = coll)
  
  checkmate::reportAssertions(coll)
  
  ###################################################################
  # Functionality                                                ####
  
  url <- sprintf("%s/redcap_v%s/DataEntry/index.php?pid=%s&page=%s&id=%s", 
                 dirname(rcon$url), 
                 rcon$version(), 
                 rcon$projectInformation()$project_id, 
                 form_name, 
                 record_id)
  
  if (!is.na(event_id)){
    url <- sprintf("%s&event_id=%s", 
                   url, 
                   event_id)
  }
  url
}

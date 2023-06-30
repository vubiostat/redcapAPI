#' @name exportRecords
#' @title Export Records from a REDCap Project
#' 
#' @description This method allows you to export a set of records for a project.
#'   Unlike \code{exportRecordsTyped}, this method is more closely 
#'   aligned with the REDCap API interface and does returns all of 
#'   its fields with class character.
#'   
#' @param rcon A \code{redcapConnection} object.
#' @param records Optional \code{character} vector of record IDs to export.
#' @param fields Optional \code{character} vector of fields to export. 
#' @param forms Optional \code{character} vector of forms to export.
#' @param events Optional \code{character} vector of events to export.
#' @param raw_or_label One of \code{c("raw", "label")}. export the raw coded 
#'   values or labels for the options of multiple choice fields
#' @param raw_or_label_headeres One of \code{c("raw", "label")}. export the 
#'   variable field names (\code{"raw"}) or the labels (\code{"label"}).
#' @param export_checkbox_label \code{logical(1)} specifies the format of 
#'   checkbox field values specifically when exporting the data as labels 
#'   (i.e., when \code{rawOrLabel = "label"}). When exporting labels, by 
#'   default (\code{FALSE}), all checkboxes will either have a value 
#'   'Checked' if they are checked or 'Unchecked' if not checked. 
#'   But if \code{TRUE}, it will instead export the checkbox value as the 
#'   checkbox option's label (e.g., 'Choice 1') if checked or it will be 
#'   blank/empty (no value) if not checked.
#' @param export_survey_fields \code{logical(1)} specifies whether or not to 
#'   export the survey 
#'   identifier field (e.g., 'redcap_survey_identifier') or survey timestamp 
#'   fields (e.g., instrument+'_timestamp') when surveys are utilized in the 
#'   project. 
#' @param export_dags \code{logical(1)} specifies whether or not to export the 
#'   'redcap_data_access_group' field when data access groups are 
#'   utilized in the project.
#' @param csv_delimiter \code{character}. One of 
#'   \code{c(",", "\t", ";", "|", "^")}. Designates the delimiter for the CSV
#'   file received from the API.
#' @param batch_size \code{integerish(1)} (or \code{NULL}). If length \code{NULL},
#'   all records are pulled. Otherwise, the records all pulled in batches of this size.
#' @param ... Additional arguments to pass to other methods.
#' @param error_handling An option for how to handle errors returned by the API.
#'   see \code{\link{redcap_error}}
#' @param config \code{list} Additional configuration parameters to pass to 
#'   \code{\link[httr]{POST}}. These are appended to any parameters in 
#'   \code{rcon$config}.
#' @param api_param \code{list} Additional API parameters to pass into the
#'   body of the API call. This provides users to execute calls with options
#'   that may not otherwise be supported by \code{redcapAPI}.
#'   
#' @description Please be aware that Data Export user rights will be applied 
#'   to this API request. For example, if you have 'No Access' data export 
#'   rights in the project, then the API data export will fail and return an 
#'   error. And if you have 'De-Identified' or 'Remove All Identifier Fields' 
#'   data export rights, then some data fields *might* be removed and filtered 
#'   out of the data set returned from the API. To make sure that no data is 
#'   unnecessarily filtered out of your API request, you should have 
#'   'Full Data Set' export rights in the project.
#'   
#' @export

exportRecords <- function(rcon, 
                          ...){
  UseMethod("exportRecords")
}

#' @rdname exportRecords3
#' @export

exportRecords.redcapApiConnection <- function(rcon, 
                                              records               = NULL, 
                                              fields                = NULL, 
                                              forms                 = NULL, 
                                              events                = NULL, 
                                              raw_or_label          = c("raw", "label"),
                                              raw_or_label_headers  = c("raw", "label"),
                                              export_checkbox_label = FALSE, 
                                              export_survey_fields  = FALSE, 
                                              export_dags           = FALSE,
                                              ..., 
                                              batch_size            = NULL, 
                                              csv_delimiter         = c(",", "\t", ";", "|", "^"), 
                                              error_handling        = getOption("redcap_error_handling"), 
                                              api_param             = list(), 
                                              config                = list()){
  
  if (is.numeric(records)) records <- as.character(records)
  
  ###################################################################
  # Argument Validation                                          ####
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_character(x = records, 
                              any.missing = FALSE, 
                              null.ok = TRUE, 
                              add = coll)
  
  checkmate::assert_character(x = fields, 
                              any.missing = FALSE, 
                              null.ok = TRUE, 
                              add = coll)
  
  checkmate::assert_character(x = forms, 
                              any.missing = FALSE, 
                              null.ok = TRUE, 
                              add = coll)
  
  checkmate::assert_character(x = events, 
                              any.missing = FALSE, 
                              null.ok = TRUE, 
                              add = coll)
  
  raw_or_label <- checkmate::matchArg(x = raw_or_label, 
                                      choices = c("raw", "label"), 
                                      add = coll)
  
  raw_or_label_headers <- checkmate::matchArg(x = raw_or_label_headers, 
                                              choices = c("raw", "label"), 
                                              add = coll)
  
  checkmate::assert_logical(x = export_checkbox_label, 
                            len = 1, 
                            any.missing = FALSE, 
                            add = coll)
  
  checkmate::assert_logical(x = export_survey_fields, 
                            len = 1, 
                            any.missing = FALSE, 
                            add = coll)
  
  checkmate::assert_logical(x = export_dags, 
                            len = 1, 
                            any.missing = FALSE, 
                            add = coll)
  
  checkmate::assert_integerish(x = batch_size, 
                               len = 1, 
                               any.missing = FALSE, 
                               null.ok = TRUE, 
                               add = coll)
  
  csv_delimiter <- checkmate::matchArg(x = csv_delimiter, 
                                       choices = c(",", "\t", ";", "|", "^"), 
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
  
  .exportRecordsTyped_validateFieldForm(rcon, 
                                        fields      = fields, 
                                        drop_fields = NULL, 
                                        forms       = forms, 
                                        coll        = coll)
  
  checkmate::reportAssertions(coll)
  
  ###################################################################
  # Make the Body List                                           ####
  
  body <- list(content = "record", 
               format = "csv", 
               type = "flat", 
               returnFormat = "csv", 
               rawOrLabel = raw_or_label, 
               rawOrLabelHeaders = raw_or_label_headers,
               exportCheckboxLabel = tolower(export_checkbox_label), 
               exportSurveyFields = tolower(export_survey_fields), 
               exportDataAccessGroups = tolower(export_dags), 
               csvDelimiter = csv_delimiter)
  body <- c(body,  
            vectorToApiBodyList(fields, "fields"), 
            vectorToApiBodyList(forms, "forms"), 
            vectorToApiBodyList(events, "events"))
  
  body <- body[lengths(body) > 0]
  
  ###################################################################
  # Make the API Call                                            ####
  
  if (length(batch_size) > 0){
    .exportRecordsTyped_Batched(rcon, 
                                body          = body, 
                                records       = records, 
                                config        = config, 
                                api_param     = api_param, 
                                csv_delimiter = csv_delimiter, 
                                batch_size    = batch_size)
  } else {
    .exportRecordsTyped_Unbatched(rcon, 
                                  body          = body, 
                                  records       = records, 
                                  config        = config, 
                                  api_param     = api_param, 
                                  csv_delimiter = csv_delimiter)
  }
}

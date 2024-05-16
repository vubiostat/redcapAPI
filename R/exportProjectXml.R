#' @name exportProjectXml
#' @title Export Entire Project as REDCap XML File
#' 
#' @description These methods enable the user to export a project's settings
#'   as an XML file in CDISC ODM format. This file may be used to transfer
#'   the project to another project, REDCap instance, or any other
#'   CDISC ODM compliant database.
#'   
#' @inheritParams common-rcon-arg
#' @inheritParams common-dot-args
#' @inheritParams common-api-args
#' @param file `character(1)` The file to which the XML export will be
#'   saved. 
#' @param return_metadata_only `logical(1)`. When `TRUE` (default)
#'   only meta data values are returned. When `FALSE`, project records
#'   data are also exported.
#' @param records `character` or `integerish`. A vector of study id's 
#'   to be returned.  When `NULL`, all subjects are returned.
#' @param fields `character`. Vector of fields to be returned.  When `NULL`, 
#'   all fields are returned (unless `forms` is specified).
#' @param events A `character`. Vector of events to be returned from a 
#'   longitudinal database.  When `NULL`, all events are returned. 
#' @param survey `logical(1)`. When `TRUE` the survey identifier fields 
#'   (e.g., `redcap_survey_identifier`) or survey timestamp fields 
#'   (e.g., `[form_name]_timestamp`) will be included in the export 
#'   when surveys are utilized in the project. 
#' @param dag `logical(1)`. When `TRUE` the `redcap_data_access_group` 
#'   field is exported when data access groups are utilized in the project. 
#' @param export_files `logical(1)`. When `TRUE` will cause the XML returned 
#'   to include all files uploaded for File Upload and Signature fields 
#'   for all records in the project. Setting this option to `TRUE` can 
#'   make the export very large and may prevent it from completing if the 
#'   project contains many files or very large files.
#'   
#' @details
#' The entire project (all records, events, arms, instruments, 
#' fields, and project attributes) can be downloaded as a single XML 
#' file, which is in CDISC ODM format (ODM version 1.3.1). This XML 
#' file can be used to create a clone of the project (including its data, 
#' optionally) on this REDCap server or on another REDCap server 
#' (it can be uploaded on the Create New Project page). Because it is in 
#' CDISC ODM format, it can also be used to import the project into 
#' another ODM-compatible system. 
#'   
#' When the `return_metadata_only` parameter is set to `FALSE`, the 
#' Data Export user rights will be applied to any data returned. For example, 
#' if the user has 'De-Identified' or 'Remove All Identifier Fields' 
#' data export rights, then some data fields might be removed and 
#' filtered out of the data set. To make sure that no data is 
#' unnecessarily filtered out of the API request, the user should have 
#' 'Full Data Set' export rights in the project.
#'   
#' @seealso 
#' [createRedcapProject()]
#' 
#' @examples
#' \dontrun{
#' unlockREDCap(connections = c(rcon = "token_alias"), 
#'              url = "your_redcap_url", 
#'              keyring = "API_KEYs", 
#'              envir = globalenv())
#'              
#' xml_file <- tempfile(file.ext = ".xml")
#' exportProjectXml(rcon, 
#'                  file = xml_file)
#' }
#' 
#'   
#'   
#' @export

exportProjectXml <- function(rcon,
                             file, 
                             return_metadata_only = TRUE, 
                             records = NULL, 
                             fields = NULL, 
                             events = NULL, 
                             survey = FALSE, 
                             dag = FALSE, 
                             export_files = FALSE, 
                             ...){
  UseMethod("exportProjectXml")
}

#' @rdname exportProjectXml
#' @export

exportProjectXml.redcapApiConnection <- function(rcon, 
                                                 file,
                                                 return_metadata_only = TRUE, 
                                                 records = NULL, 
                                                 fields = NULL, 
                                                 events = NULL, 
                                                 survey = FALSE, 
                                                 dag = FALSE, 
                                                 export_files = FALSE, 
                                                 ...)
{
  if (is.numeric(records)) records <- as.character(records)
  
  ###################################################################
  # Argument Validation                                          ####
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapConnection", 
                          add = coll)
  
  checkmate::assert_character(x = file, 
                              len = 1, 
                              add = coll)
  
  checkmate::assert_logical(x = return_metadata_only, 
                            len = 1, 
                            add = coll)
  
  checkmate::assert_character(x = records, 
                              null.ok = TRUE, 
                              any.missing = FALSE, 
                              add = coll)
  
  checkmate::assert_character(x = fields, 
                              null.ok = TRUE, 
                              any.missing = FALSE, 
                              add = coll)
  
  checkmate::assert_character(x = events, 
                              null.ok = TRUE, 
                              any.missing = FALSE, 
                              add = coll)
  
  checkmate::assert_logical(x = survey, 
                            len = 1, 
                            any.missing = FALSE, 
                            add = coll)
  
  checkmate::assert_logical(x = dag, 
                            len = 1, 
                            any.missing = FALSE, 
                            add = coll)
  
  checkmate::assert_logical(x = export_files, 
                            len = 1, 
                            any.missing = FALSE, 
                            add = coll)

  checkmate::reportAssertions(coll)
  
  
  .exportRecordsTyped_validateFieldForm(rcon = rcon, 
                                        fields = fields, 
                                        drop_fields = NULL, 
                                        forms = NULL, 
                                        coll = coll)
  
  checkmate::assert_subset(x = events, 
                           choices = rcon$events()$unique_event_name, 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  ###################################################################
  # API Body List                                                ####
  
  body <- list(content = "project_xml", 
               returnMetaDataOnly = tolower(return_metadata_only), 
               returnFormat = "csv", 
               exportSurveyFields = tolower(survey), 
               exportDataAccessGroups = tolower(dag), 
               exportFiles = tolower(export_files))
  body <- c(body, 
            vectorToApiBodyList(records, "records"), 
            vectorToApiBodyList(fields, "fields"), 
            vectorToApiBodyList(events, "events"))

  ###################################################################
  # Call the API                                                 ####
  
  response <- makeApiCall(rcon, body, ...)

  WriteFile <- reconstituteFileFromExport(response, 
                                          dir = dirname(file),
                                          filename = basename(file))
  
  nrow(WriteFile) > 0
}

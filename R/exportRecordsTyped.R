#' @name exportRecordsTyped
#' 
#' @title A replacement for \code{\link{exportRecords}} with full inversion of control over 
#'        type casting.
#' @description Exports records from a REDCap Database, allowing for 
#'   subsets of subjects, fields, records, and events. This function is
#'   the long term replacement for exportRecords. 
#'
#' @param rcon A REDCap connection object as created by \code{redcapConnection}.
#' @param fields \code{character} vector of fields to be returned.  If \code{NULL}, 
#'   all fields are returned (unless \code{forms} is specified).
#' @param drop_fields \code{character} A vector of field names to remove from 
#'   the export. Ignore if length = 0.
#' @param forms \code{character} vector of forms to be returned.  If \code{NULL}, 
#'   all forms are returned (unless \code{fields} is specified.
#' @param records \code{character} or \code{integerish}. A vector of study id's 
#'   to be returned.  If \code{NULL}, all subjects are returned.
#' @param events A \code{character} vector of events to be returned from a 
#'   longitudinal database.  If \code{NULL}, all events are returned. When 
#'   using a \code{redcapOfflineConnection} object, this argument is unvalidated, 
#'   and only rows that match one of the values given are returned; be advised
#'   that misspellings may result in unexpected results.
#' @param survey \code{logical(1)} specifies whether or not to export the survey identifier field 
#'   (e.g., "redcap_survey_identifier") or survey timestamp fields 
#'   (e.g., form_name+"_timestamp") when surveys are utilized in the project. 
#'   If you do not pass in this flag, it will default to "true". If set to 
#'   "true", it will return the redcap_survey_identifier field and also the 
#'   survey timestamp field for a particular survey when at least 
#'   one field from that survey is being exported. NOTE: If the survey 
#'   identifier field or survey timestamp fields are imported via API data 
#'   import, they will simply be ignored since they are not real fields in 
#'   the project but rather are pseudo-fields.
#' @param dag \code{logical(1)} specifies whether or not to export the "redcap_data_access_group" 
#'   field when data access groups are utilized in the project. If you do not 
#'   pass in this flag, it will default to "false". NOTE: This flag is only 
#'   viable if the user whose token is being used to make the API request is 
#'   *not* in a data access group. If the user is in a group, then this 
#'   flag will revert to its default value.
#' @param date_begin \code{POSIXct(1)}. Ignored if \code{NULL} (default). 
#'   Otherwise, records created or modified after this date will be returned.
#' @param date_end \code{POSIXct(1)}. Ignored if \code{NULL} (default). 
#'   Otherwise, records created or modified before this date will be returned.
#'
#' @param na  A named \code{list} of user specified functions to determine if the
#'   data is NA. This is useful when data is loaded that has coding for NA, e.g.
#'   -5 is NA. Keys must correspond to a truncated REDCap field type, i.e.
#'   {date_, datetime_, datetime_seconds_, time_mm_ss, time_hh_mm_ss, time, float,
#'   number, calc, int, integer, select, radio, dropdown, yesno, truefalse,
#'   checkbox, form_complete, sql}. The function will be provided the variables
#'   (x, field_name, coding). The function must return a vector of logicals
#'   matching the input. It defaults to \code{\link{isNAorBlank}} for all
#'   entries.
#' @param validation A named \code{list} of user specified validation functions. The 
#'   same named keys are supported as the na argument. The function will be 
#'   provided the variables (x, field_name, coding). The function must return a
#'   vector of logical matching the input length. Helper functions to construct
#'   these are \code{\link{valRx}} and \code{\link{valChoice}}. Only fields that
#'   are not identified as NA will be passed to validation functions. 
#' @param cast A named \code{list} of user specified class casting functions. The
#'   same named keys are supported as the na argument. The function will be 
#'   provided the variables (x, field_name, coding). The function must return a
#'   vector of logical matching the input length. See \code{\link{fieldValidationAndCasting}}.
#'   The field type \code{system} may also be used to determine how the fields
#'   \code{redcap_event_name}, \code{redcap_repeat_instrument}, and 
#'   \code{redcap_data_access_group} are cast.
#' @param assignment A named \code{list} of functions. These functions are provided, field_name,
#'   label, description and field_type and return a list of attributes to assign
#'   to the column. Defaults to creating a label attribute from the stripped
#'   HTML and UNICODE raw label and scanning for units={"UNITS"} in description
#'   to use as a units attribute.
#' @param config named \code{list}. Additional configuration parameters to pass to \code{httr::POST},
#'   These are appended to any parameters in \code{rcon$config}
#' @param api_param named \code{list}. Additional API parameters to pass into the body of the
#'   API call. This provides users to execute calls with options that may not
#'   otherwise be supported by redcapAPI.
#' @param csv_delimiter character. One of \code{c(",", "\t", ";", "|", "^")}. Designates the
#'   delimiter for the CSV file received from the API.
#' @param batch_size \code{integerish(1)} (or \code{NULL}). If length \code{NULL},
#'   all records are pulled. Otherwise, the records all pulled in batches of this size.
#' @param ... Consumes any additional parameters passed. Not used.
#' @param error_handling An option for how to handle errors returned by the API.
#'   see \code{\link{redcap_error}}
#'   
#' @details
#' 
#' In all calls, the project's ID field will be included--there is no option
#' provided to prevent this. Additionally, if the project has a secondary
#' unique field specified, it will also be included. Inclusion of these fields
#' is necessary to support some post-processing functions. 
#' 
#' By default, the system fields \code{redcap_event_name}, 
#' \code{redcap_repeat_instrument}, and \code{redcap_repeat_instance} are 
#' exported (when they are appropriate to the project). These are automatically
#' included by the API. However, \code{exportRecordsTyped} assumes that if 
#' you include only a subset of these system fields, then you truly only 
#' want that subset and unrequested fields will be removed. Be aware that 
#' this may cause problems with some post-processing functions that operate
#' on repeating instrument data. 
#' 
#' The combination of the project ID field, secondary unique field, and the
#' system fields are what uniquely identify an experimental unit. In nearly 
#' all cases, it is desirable to have them all included.
#' 
#' A record of exports through the API is recorded in the Logging section 
#' of the project.
#' 
#' The 'offline' version of the function operates on the raw (unlabeled) data 
#' file downloaded from REDCap along with the data dictionary.  
#' This is made available for instances where the API can not be accessed for 
#' some reason (such as waiting for API approval from the REDCap administrator).
#' 
#' A 'batched' export is one where the export is performed over a series of 
#' API calls rather than one large call.  For large projects on small servers, 
#' this may prevent a single user from tying up the server and forcing others 
#' to wait on a larger job.  The batched export is performed by first 
#' calling the API to export the subject identifier field (the first field
#' in the meta data).  The unique ID's are then assigned a batch number with 
#' no more than \code{batch_size} ID's in any single batch.  The batches are 
#' exported from the API and stacked together.
#' 
#' In longitudinal projects, \code{batch_size} may not necessarily be the 
#' number of records exported in each batch.  If \code{batch_size} is 10 and 
#' there are four records per patient, each batch will consist of 40 records.  
#' Thus, if you are concerned about tying up the server with a large, 
#' longitudinal project, it would be prudent to use a smaller batch size.
#' 
#' @section Inversion of Control:
#' 
#' The final product of calling this is a \code{data.frame} with columns
#' that have been type cast to most commonly used analysis class (e.g. factor).
#' This version allows the user to override any step of this process by
#' specifying a different function for each of the stages of the type casting.
#' The algorithm is as follows:
#' 
#' 1. Detect NAs in returned data (\code{na} argument).
#' 2. Run \code{validate} functions for the field_types.
#' 3. On the fields that are not NA and pass validate do the specified cast.
#' 
#' It is expected that the \code{na} and \code{validate} overrides should
#' rarely be used. Their exposure via the function parameters is to future
#' proof against possible bugs in the defaults, and allows for things that
#' higher versions of REDCap add as possible field types. I.e., the overrides
#' are for use to continue using the library when errors or changes to REDCap
#' occur. 
#' 
#' The cast override is one were users can specify things that were controlled
#' by an ever increasing set of flags before. E.g., \code{dates=as.Date} was
#' an addition to allow dates in the previous version to be overridden if the 
#' user wanted to use the Date class. In this version, it would appear called
#' as \code{cast=list(_date=as.Date))}. See \code{\link{fieldValidationAndCasting}}
#' for a full listing of package provided cast functions.
#' 
#' @section REDCap API Documentation (6.5.0):
#' This function allows you to export a set of records for a project
#' 
#' Note about export rights (6.0.0+): Please be aware that Data Export user rights will be 
#' applied to this API request. For example, if you have "No Access" data export rights 
#' in the project, then the API data export will fail and return an error. And if you 
#' have "De-Identified" or "Remove all tagged Identifier fields" data export rights, 
#' then some data fields *might* be removed and filtered out of the data set returned 
#' from the API. To make sure that no data is unnecessarily filtered out of your API 
#' request, you should have "Full Data Set" export rights in the project.
#' 
#' @section REDCap Version:
#' 5.8.2+ (Perhaps earlier) 
#' 
#' @section Known REDCap Limitations:
#' None
#' 
#' @section Deidentified Batched Calls:
#' Batched calls to the API are not a feature of the REDCap API, but may be imposed 
#' by making multiple calls to the API.  The process of batching the export requires
#' that an initial call be made to the API to retrieve only the record IDs.  The
#' list of IDs is then broken into chunks, each about the size of \code{batch.size}.
#' The batched calls then force the \code{records} argument in each call.
#' 
#' When a user's permissions require a de-identified data export, a batched call 
#' should be expected to fail.  This is because, upon export, REDCap will hash the 
#' identifiers.  When R attempts to pass the hashed identifiers back to REDCap, 
#' REDCap will try to match the hashed identifiers to the unhashed identifiers in the
#' database.  No matches will be found, and the export will fail.
#' 
#' Users who are exporting de-identified data will have to settle for using unbatched
#' calls to the API (ie, \code{batch_size = -1})
#' 
#' @importFrom utils modifyList
#' 
#' @examples
#' \dontrun{
#' #Offline Connection example
#' rcon_off <- 
#'   offlineConnection(
#'     meta_data = 
#'       system.file(file.path("extdata/offlineConnectionFiles", 
#'                             "TestRedcapAPI_DataDictionary.csv"), 
#'                   package = "redcapAPI"), 
#'     records = 
#'       system.file(file.path("extdata/offlineConnectionFiles",
#'                             "TestRedcapAPI_Records.csv"), 
#'                   package = "redcapAPI"))
#'
#' exportRecordsTyped(rcon_off)
#' }
#' @seealso \code{\link{exportBulkRecords}}
#' @export

exportRecordsTyped <-
  function(
    # API Call parameters
    rcon,

    # Limiters
    fields        = NULL,
    drop_fields   = NULL,
    forms         = NULL,
    records       = NULL,
    events        = NULL,
    # Removed arguments that cannot be used in the offline function
    ...)
    
    UseMethod("exportRecordsTyped")

#' @rdname exportRecordsTyped
#' @export

exportRecordsTyped.redcapApiConnection <- 
  function(
    # API Call parameters
    rcon,  
    
    # Limiters
    fields         = NULL,
    drop_fields    = NULL,
    forms          = NULL,
    records        = NULL,
    events         = NULL,
    survey         = TRUE,
    dag            = TRUE,
    date_begin     = NULL,
    date_end       = NULL,
    
    # Type Casting Default Overrides Function Lists
    na             = list(),
    validation     = list(),
    cast           = list(),
    assignment     = list(label=stripHTMLandUnicode,
                          units=unitsFieldAnnotation),
    ...,
    config         = list(),
    api_param      = list(),
    csv_delimiter  = ",",
    batch_size     = NULL, 
    error_handling = getOption("redcap_error_handling"))
{
  if (is.numeric(records)) records <- as.character(records)

   ###########################################################################
  # Check parameters passed to function
  coll <- checkmate::makeAssertCollection()

  checkmate::assert_class(x = rcon,
                          classes = "redcapApiConnection",
                          add = coll)
  
  .exportRecordsTyped_validateCommonArgument(fields      = fields,
                                             drop_fields = drop_fields,
                                             forms       = forms,
                                             records     = records,
                                             events      = events, 
                                             na          = na, 
                                             validation  = validation, 
                                             cast        = cast, 
                                             assignment  = assignment,
                                             coll        = coll)
  
  checkmate::assert_logical(x = survey, 
                            len = 1, 
                            any.missing = FALSE,
                            add = coll)
  
  checkmate::assert_logical(x = dag, 
                            len = 1, 
                            any.missing = FALSE,
                            add = coll)
  
  checkmate::assert_posixct(x = date_begin, 
                            max.len = 1, 
                            any.missing = FALSE,
                            null.ok = TRUE, 
                            add = coll)
  
  checkmate::assert_posixct(x = date_end, 
                            max.len = 1, 
                            any.missing = FALSE,
                            null.ok = TRUE, 
                            add = coll)
  
  checkmate::assert_integerish(x = batch_size,
                               lower = 1, 
                               max.len = 1, 
                               any.missing = FALSE,
                               null.ok = TRUE, 
                               add = coll)
  
  csv_delimiter <- checkmate::matchArg(x         = csv_delimiter, 
                                       choices   = c(",", "\t", ";", "|", "^"),
                                       .var.name = "csv_delimiter",
                                       add = coll)
  
  checkmate::assert_list(x = config, 
                         names = "named", 
                         add = coll)
  
  checkmate::assert_list(x = api_param, 
                         names = "named", 
                         add = coll)

  checkmate::reportAssertions(coll)
  
  .exportRecordsTyped_validateFieldForm(rcon = rcon, 
                                        fields = fields, 
                                        drop_fields = drop_fields, 
                                        forms = forms, 
                                        coll = coll)
  
  ###################################################################
  # Handle System Fields in the Request
  
  user_requested_system_fields <- length(fields) > 0 && any(fields %in% REDCAP_SYSTEM_FIELDS)
  user_requested_only_system_fields <- length(fields) > 0 && all(fields %in% REDCAP_SYSTEM_FIELDS)
  system_fields_user_requested <- REDCAP_SYSTEM_FIELDS[REDCAP_SYSTEM_FIELDS %in% fields]
  
  # The REDCap API won't accept system fields in the fields argument. 
  # we have to remove them from the request.
  fields <- fields[!fields %in% REDCAP_SYSTEM_FIELDS] # redcapDataStructures.R
  
  # But if the user only requested system fields, we need to provide 
  # at least one actual field to get anything back from the API
  if (user_requested_only_system_fields){
    fields <- rcon$metadata()$field_name[1]
  }
  
  # Check that the events exist in the project
  
  checkmate::assert_subset(x = events, 
                           choices = rcon$events()$unique_event_name, 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
   ###################################################################
  # Combine fields, drop_fields, and forms into the fields that will 
  # be exported
  fields <- .exportRecordsTyped_fieldsArray(rcon        = rcon, 
                                            fields      = fields, 
                                            drop_fields = drop_fields, 
                                            forms       = forms)
  
   ###################################################################
  # Call API for Raw Results
  
  # We don't need to pass forms to the API because we have 
  # absorbed that information directly into fields
  body <- c(list(content                = "record", 
                 format                 = "csv", 
                 returnFormat           = "csv", 
                 type                   = "flat", 
                 exportSurveyFields     = tolower(survey), 
                 exportDataAccessGroups = tolower(dag), 
                 dateRangeBegin         = format(date_begin, format = "%Y-%m-%d %H:%M:S"), 
                 dateRangeEnd           = format(date_end,   format = "%Y-%m-%d %H:M%:%S"), 
                 csvDelimiter           = csv_delimiter), 
            vectorToApiBodyList(fields, "fields"), 
            vectorToApiBodyList(events, "events"))
  
  body <- body[lengths(body) > 0]
  
  Raw <- 
    if (length(batch_size) == 0)
    {
      .exportRecordsTyped_Unbatched( rcon           = rcon, 
                                     body           = body, 
                                     records        = records, 
                                     config         = config, 
                                     api_param      = api_param, 
                                     csv_delimiter  = csv_delimiter, 
                                     error_handling = error_handling)
    } else
    {
      .exportRecordsTyped_Batched(  rcon           = rcon, 
                                    body           = body, 
                                    records        = records, 
                                    config         = config, 
                                    api_param      = api_param, 
                                    csv_delimiter  = csv_delimiter, 
                                    batch_size     = batch_size, 
                                    error_handling = error_handling)
    }
  
  if (identical(Raw, data.frame())){
    return(Raw)
  }
  
  Raw <- filterEmptyRow(Raw, rcon)
  
  if (user_requested_system_fields){
    if (user_requested_only_system_fields){
      Raw <- Raw[-1]
    }
    
    unrequested_fields <- REDCAP_SYSTEM_FIELDS[!REDCAP_SYSTEM_FIELDS %in% system_fields_user_requested]
    Raw <- Raw[!names(Raw) %in% unrequested_fields]
  }
  
  # See fieldCastingFunctions.R for definition of .castRecords
  .castRecords(Raw              = Raw, 
               Records          = NULL,
               rcon             = rcon, 
               na               = na, 
               validation       = validation, 
               cast             = cast, 
               assignment       = assignment, 
               default_cast     = .default_cast, 
               default_validate = .default_validate)
}



# offline function ----------------------------------------------------
#' @rdname exportRecordsTyped
#' @export

exportRecordsTyped.redcapOfflineConnection <- function(rcon, 
                                                       fields        = NULL,
                                                       drop_fields   = NULL,
                                                       forms         = NULL,
                                                       records       = NULL,
                                                       events        = NULL,
                                                       
                                                       # Type Casting Default Overrides Function Lists
                                                       na            = list(),
                                                       validation    = list(),
                                                       cast          = list(),
                                                       assignment    = list(label=stripHTMLandUnicode,
                                                                            units=unitsFieldAnnotation),
                                                       ...){
  
  if (is.numeric(records)) records <- as.character(records)
  
  ###################################################################
  # Argument Validation
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapOfflineConnection", 
                          add = coll)
  
  .exportRecordsTyped_validateCommonArgument(fields = fields, 
                                             drop_fields = drop_fields, 
                                             forms = forms, 
                                             records = records, 
                                             events = events, 
                                             na          = na, 
                                             validation  = validation, 
                                             cast        = cast, 
                                             assignment  = assignment,
                                             coll = coll)
  
  checkmate::reportAssertions(coll)
  
  checkmate::assert_data_frame(x = rcon$metadata(),
                               .var.name = "rcon$metadata()",
                               add = coll)
  
  checkmate::assert_data_frame(x = rcon$record(),
                               .var.name = "rcon$record()",
                               add = coll)
  
  checkmate::reportAssertions(coll)
  
  .exportRecordsTyped_validateFieldForm(rcon = rcon, 
                                        fields = fields, 
                                        drop_fields = drop_fields, 
                                        forms = forms, 
                                        coll = coll)
  
  checkmate::reportAssertions(coll)
  
  ###################################################################
  # Handle System Fields in the Request
  
  user_requested_system_fields <- length(fields) > 0 && any(fields %in% REDCAP_SYSTEM_FIELDS)
  user_requested_only_system_fields <- length(fields) > 0 && all(fields %in% REDCAP_SYSTEM_FIELDS)
  system_fields_user_requested <- REDCAP_SYSTEM_FIELDS[REDCAP_SYSTEM_FIELDS %in% fields]
  
  # The REDCap API won't accept system fields in the fields argument. 
  # we have to remove them from the request.
  fields <- fields[!fields %in% REDCAP_SYSTEM_FIELDS] # redcapDataStructures.R
  
  # But if the user only requested system fields, we need to provide 
  # at least one actual field to get anything back from the API
  if (user_requested_only_system_fields){
    fields <- rcon$metadata()$field_name[1]
  }
  
  ###################################################################
  # Combine fields, drop_fields, and forms into the fields that will 
  # be exported
  
  MetaData <- rcon$metadata()
  
  system_field <- REDCAP_SYSTEM_FIELDS[REDCAP_SYSTEM_FIELDS %in% names(rcon$records())]
  
  fields <- .exportRecordsTyped_fieldsArray(rcon         = rcon, 
                                            fields       = fields, 
                                            drop_fields  = drop_fields, 
                                            forms        = forms, 
                                            use_original = FALSE)
  
  fields <- fields[!fields %in% MetaData$field_name[MetaData$field_type == "descriptive"]]
  
  
  id_index <- which(fields == MetaData$field_name[1])
  
  if (length(id_index) > 0){
    fields <- c(fields[id_index], 
                system_field, 
                fields[-id_index])
  } else {
    fields <- c(system_field, 
                fields)
  }

  ###################################################################
  # Raw Data comes from the rcon object for offlineConnections
  
  Raw <- rcon$records()[fields]
  
  if (length(records) > 0)
    Raw <- Raw[Raw[[ rcon$metadata()$field_name[1] ]] %in% records, ]

  
  if (length(events) > 0)
    Raw <- Raw[Raw$redcap_event_name %in% events, ]
  
  
  if (user_requested_system_fields){
    if (user_requested_only_system_fields){
      Raw <- Raw[-1]
    }
    
    unrequested_fields <- REDCAP_SYSTEM_FIELDS[!REDCAP_SYSTEM_FIELDS %in% system_fields_user_requested]
    Raw <- Raw[!names(Raw) %in% unrequested_fields]
  }
  
  Raw <- filterEmptyRow(Raw, rcon)
  
  ###################################################################
  # Process meta data for useful information
  
  # See fieldCastingFunctions.R for definition of .castRecords
  .castRecords(Raw              = Raw, 
               Records          = NULL,
               rcon             = rcon, 
               na               = na, 
               validation       = validation, 
               cast             = cast, 
               assignment       = assignment, 
               default_cast     = .default_cast, 
               default_validate = .default_validate)
}


#######################################################################
# Unexported

# .exportRecordsTyped_validateCommonArgument ------------------------

.exportRecordsTyped_validateCommonArgument <- function(fields,
                                                       drop_fields,
                                                       forms,
                                                       records,
                                                       events,
                                                       na, 
                                                       validation, 
                                                       cast, 
                                                       assignment,
                                                       coll){
  checkmate::assert_character(x = fields, 
                              any.missing = FALSE, 
                              null.ok = TRUE,
                              add = coll)
  
  checkmate::assert_character(x = drop_fields, 
                              any.missing = FALSE,
                              null.ok = TRUE,
                              add = coll)
  
  checkmate::assert_character(x = forms, 
                              any.missing = FALSE,
                              null.ok = TRUE,
                              add = coll)
  
  checkmate::assert_character(x = records, 
                              any.missing = FALSE,
                              null.ok = TRUE,
                              add = coll)
  
  checkmate::assert_character(x = events, 
                              any.missing = FALSE,
                              null.ok = TRUE,
                              add = coll)
  
  checkmate::assert_list(x = na, 
                         names = "named", 
                         add = coll)
  
  checkmate::assert_list(x = validation, 
                         names = "named", 
                         add = coll)
  
  checkmate::assert_list(x = cast, 
                         names = "named", 
                         add = coll)
  
  checkmate::assert_list(x = assignment, 
                         names = "named", 
                         types = "function",
                         add = coll)
  

}


# .exportRecordsTyped_validateFieldForm -----------------------------
.exportRecordsTyped_validateFieldForm <- function(rcon, 
                                                  fields, 
                                                  drop_fields, 
                                                  forms, 
                                                  coll){
  # Check that fields (and drop_fields) exist in the project
  
  MetaData <- rcon$metadata()
  ProjectFields <- rcon$fieldnames()
  available_fields <- unique(c(ProjectFields$original_field_name, 
                               ProjectFields$export_field_name, 
                               MetaData$field_name[MetaData$field_type %in% c("calc", "file")], 
                               REDCAP_SYSTEM_FIELDS))
  
  checkmate::assert_subset(x = fields, 
                           choices = available_fields, 
                           add = coll)
  
  checkmate::assert_subset(x = drop_fields, 
                           choices = available_fields, 
                           add = coll)
  
  # Check that the forms exist in the project
  
  checkmate::assert_subset(x = forms, 
                           choices = rcon$instruments()$instrument_name, 
                           add = coll)
}


# .exportRecordsTyped_fieldsArray -----------------------------------

.exportRecordsTyped_fieldsArray <- function(rcon, 
                                            fields, 
                                            drop_fields, 
                                            forms, 
                                            use_original = TRUE)
{
  MetaData <- rcon$metadata()
  
  # exportFieldNames excludes fields of type calc, descriptive, and file
  # We need to wedge them in here or we'll never get them out of the API
  ProjectFields <- rcon$fieldnames()
  
  MissingFromFields <- MetaData[MetaData$field_type %in% c("calc", 
                                                           "file"), ]
  if (nrow(MissingFromFields) > 0){
    # FIXME: We need a test on a project that has no calc or file fields.
    MissingFromFields <- 
      data.frame(original_field_name = MissingFromFields$field_name, 
                 choice_value = NA, 
                 export_field_name = MissingFromFields$field_name, 
                 stringsAsFactors = FALSE)
  
    ProjectFields <- rbind(ProjectFields, MissingFromFields)
  }
  ProjectFields$index <- seq_len(nrow(ProjectFields))
  
  # Make a reference table between fields and forms
  FieldFormMap <- MetaData[c("field_name", "form_name")]
  
  FieldFormMap <- 
    merge(ProjectFields, 
          FieldFormMap, 
          by.x = c("original_field_name"), 
          by.y = "field_name", 
          all.x = TRUE)
  
  # Assign [form]_complete fields to their forms
  FieldFormMap$form_name <- 
    ifelse(is.na(FieldFormMap$form_name) &   # if form name is missing and end in _complete
             grepl(pattern = "_complete$", 
                   x = FieldFormMap$original_field_name), 
           yes = sub(pattern = "(^.+)(_complete$)", 
                     replacement = "\\1", # replace with anything before _complete 
                     FieldFormMap$original_field_name), 
           no = FieldFormMap$form_name)
  
  # If fields and forms are both NULL, the default behavior is to grab 
  # all of the fields.
  # Otherwise, we will only include those specified through those arguments
  
  FieldFormMap$is_in_fields <-
    rep((length(fields) == 0 && length(forms) == 0), 
        nrow(FieldFormMap))
  
  # For the forms, we can't assume they are in forms. Instead, we initialize
  # this to FALSE and have to provide positive proof that they are in forms.
  FieldFormMap$is_in_forms <- rep(FALSE, nrow(FieldFormMap))
  
  # Change is_in_fields to TRUE for those in fields
  if (length(fields) > 0){
    FieldFormMap$is_in_fields <- 
      FieldFormMap$original_field_name %in% fields | 
      FieldFormMap$export_field_name %in% fields
  }
  
  # Change is_in_forms to TRUE for fields that are in one of forms.
  if (length(forms) > 0){
    FieldFormMap$is_in_forms <- 
      FieldFormMap$form_name %in% forms
  }
  
  FieldFormMap <- 
    split(FieldFormMap, 
          FieldFormMap$original_field_name)
  
  # If any of the checkbox options are included in fields, 
  # mark all of the options for inclusion
  FieldFormMap <- lapply(FieldFormMap, 
                         function(FFM){
                           if (any(FFM$is_in_fields)){
                             FFM$is_in_fields <- rep(TRUE, nrow(FFM))
                           }
                           FFM
                         })
  
  # If any of the checkbox options are listed if drop_fields
  # Remove all of the checkbox options.
  # Also sets the is_in_forms to FALSE to ensure it isn't 
  # included in the API call.
  
  if (length(drop_fields) > 0){
    FieldFormMap <- 
      lapply(FieldFormMap, 
             function(FFM, drop){
               all_field <- c(FFM$original_field_name, 
                              FFM$export_field_name)
               if (any(all_field %in% drop)){
                 FFM$is_in_fields <- rep(FALSE, nrow(FFM))
                 FFM$is_in_forms <- rep(FALSE, nrow(FFM))
               }
               FFM
             }, 
             drop = drop_fields)
  }
  
  # Combine the list
  FieldFormMap <- do.call("rbind", FieldFormMap)
  rownames(FieldFormMap) <- NULL
  
  # Reduce to fields in either fields or forms
  Fields <- FieldFormMap[FieldFormMap$is_in_fields | 
                           FieldFormMap$is_in_forms, ]
  Fields <- Fields[order(Fields$index), ]
  
  fields_to_request <- 
    if (use_original){
      unique(Fields$original_field_name)
    } else {
      Fields$export_field_name
    }
  
  # Lastly, we need to ensure that the identifier fields are included.
  # We will include the record ID field if it is not already included.
  # We will also include the secondary unique ID field if one is specified.

  id_fields <- getProjectIdFields(rcon)

  fields_to_request <- c(id_fields, fields_to_request)
  fields_to_request <- fields_to_request[!duplicated(fields_to_request)]
  fields_to_request
}

# .exportRecordsTyped_Unbatched -------------------------------------
.exportRecordsTyped_Unbatched <- function( rcon, 
                                           body, 
                                           records, 
                                           config, 
                                           api_param, 
                                           csv_delimiter, 
                                           error_handling)
{
  response <- makeApiCall(rcon, 
                          body = c(body, 
                                   api_param, 
                                   vectorToApiBodyList(records, "records")), 
                          config = config)
  
  if (response$status_code != 200){
    redcap_error(response, 
                 error_handling = error_handling)
  } 
  
  if (trimws(as.character(response)) == ""){
    message("No data found in the project.")
    return(data.frame())
  }
  
  read.csv(text = as.character(response), 
           stringsAsFactors = FALSE, 
           na.strings = "", 
           colClasses = "character", 
           sep = csv_delimiter)
}

# .exportRecordsTyped_Batched ---------------------------------------
.exportRecordsTyped_Batched <- function( rcon, 
                                         body, 
                                         records, 
                                         config, 
                                         api_param, 
                                         csv_delimiter, 
                                         batch_size, 
                                         error_handling)
{
  # If records were not provided, get all the record IDs from the project
  if (length(records) == 0)
  {
    target_field <- rcon$metadata()$field_name[1]
    record_response <- makeApiCall(rcon, 
                                   body = c(list(content = "record", 
                                                 format = "csv", 
                                                 outputFormat = "csv"), 
                                            vectorToApiBodyList(target_field, 
                                                                "fields")))
    
    if (record_response$status_code != 200){
      redcap_error(record_response, 
                   error_handling = error_handling)
    }
    
    if (trimws(as.character(record_response)) == ""){
      message("No data found in the project.")
      return(data.frame())
    }
    
    records <- read.csv(text = as.character(record_response), 
                        stringsAsFactors = FALSE, 
                        na.strings = "", 
                        sep = csv_delimiter)
    records <- records[[target_field]]
  }
  
  # group is a vector of integers where each integer is repeated up to 
  # batch_size times. Used to separate records into a list where
  # each element has a maximum length of batch_size
  group <- rep(seq((length(records) %/% batch_size) + 1), 
               each = batch_size, 
               length.out = length(records))
  
  records <- split(records, group)
  
  # Call the API for each batch of records
  Batched <- 
    lapply(records, 
           function(r){ 
             .exportRecordsTyped_Unbatched(rcon = rcon, 
                                           body = body, 
                                           records = r, 
                                           config = config, 
                                           api_param = api_param, 
                                           csv_delimiter = csv_delimiter)})
  
  # Combine the data
  Batched <- do.call("rbind", Batched)
  rownames(Batched) <- NULL
  Batched
}


#' @name exportBulkRecords
#' @title A helper function to export multiple records and forms using
#' a single call.
#' @description Exports records from multiple REDCap Databases using
#' multiple calls to \code{\link{exportRecordsTyped}}
#'
#' @param rcon A named list of REDCap connection object as created by \code{\link{redcapConnection}}.
#' @param forms A named list that is a subset of rcon's names. A specified \code{rcon}
#'              will provide a list of forms for repeated calls to \code{exportRecordsType}.
#'              If a connection reference is missing it will default to all forms. To override
#'              this default specify a connection's forms with NA to just get all
#'              data. 
#' @param envir A environment to write the resulting Records in as variables
#'   given by their name in rcon or if from a form their rcon named pasted to 
#'   their form name joined by \code{sep}. If not specified the function
#'   will return a named list with the results. Will accept a number of the
#'   environment.
#' @param sep A character string to use when joining the rcon name to the form name
#' for storing variables. 
#' @param post A function that will run on all returned sets of Records. 
#' @param \dots Any additional variables to pass to \code{\link{exportRecordsTyped}}.
#' @return Will return a named list of the resulting records if \code{envir} is 
#'    NULL. Otherwise will assign them to the specified \code{envir}.
#' @examples
#' \dontrun{
#' unlockREDCap(c(test_conn    = 'TestRedcapAPI',
#'                sandbox_conn = 'SandboxAPI'),
#'              keyring      = 'MyKeyring',
#'              envir        = globalenv(),
#'              url          = 'https://<REDCAP_URL>/api/') 
#'
#'# After user interaction to unlock the local encrypted keyring
#'# the global environment will contain the REDCap connections
#'# `test_conn` and `sandbox_conn`
#'# 
#'# Next the user wants to bulk specify importing all the forms
#'# of interest and post process
#'
#'exportBulkRecords(
#'  rcon  = list(test = test_conn,
#'               sand = sandbox_conn),
#'  forms = list(test = c('form1', 'form2'),
#'  envir = globalenv(),
#'  post  = function(Records, rcon)
#'          {
#'            Records              |>
#'            mChoiceCast(rcon)    |>
#'            guessDat(rcon)       |>
#'            widerRepeating(rcon)
#'          }
#'  )
#'  
#'# The environment now contains the data.frames: `test.form1`, `test.form2`, `sand`.
#'# Each of these were retrieved, possibly using the forms argument and all were
#'# post processed in the same manner as specified by `post`.
#' }
#' @export
exportBulkRecords <- function(rcon, forms=NULL, envir=NULL, sep="_", post=NULL, ...)
{
  if(is.numeric(envir)) envir <- as.environment(envir)
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_list(     x       = rcon,
                              types   = "redcapApiConnection",
                              min.len = 1,
                              names   = "named",
                              add     = coll)
  
  checkmate::assert_list(     x       = forms,   # First, just verify that it is actually a list that was passed. 
                              names   = "named",
                              null.ok = TRUE,
                              add     = coll)
  
  checkmate::assert_class(    x       = envir,
                              classes = "environment",
                              null.ok = TRUE,
                              add     = coll)
  
  checkmate::assert_character(x       = sep,
                              len     = 1,
                              add     = coll)
  
  checkmate::assert_function( x       = post,
                              nargs   = 2,
                              null.ok = TRUE,
                              add     = coll)
  
  checkmate::reportAssertions(coll)
  
  if(!is.null(forms))
  {
    forms[is.na(forms)] <- NA_character_ 
    
    checkmate::assert_subset( x       = names(forms),
                              choices = names(rcon),
                              add     = coll)
    
    checkmate::assert_list( x       = forms,
                            types   = c("character"),
                            add     = coll)
  }
  
  checkmate::reportAssertions(coll)
  
  dest <- list()
  
  if(is.null(forms)) forms <- list()
  
  # For each dataset requested
  for(i in names(rcon))
  {
    conn  <- rcon[[i]]
    f     <- forms[[i]]
    
    lform <- if(is.null(f))                 conn$instruments()$instrument_name else
             if(length(f) == 1 && is.na(f)) NULL                               else
                                            forms[[i]]
    lform <- lform[!is.na(lform)] # Just in case NA's are spread about
    
    if(is.null(lform))
    {
      dest[[i]] <- exportRecordsTyped(conn, ...)
      if(!is.null(post)) dest[[i]] <- post(dest[[i]], conn)
    } else
    {
      for(j in lform)
      {
        name <- paste0(i, sep, j)
        dest[[name]] <- exportRecordsTyped(conn, forms=j, ...)
        if(!is.null(post)) dest[[name]] <- post(dest[[name]], conn)
      }
    }
  }
  
  if(is.null(envir)) dest else list2env(dest, envir=envir)
}
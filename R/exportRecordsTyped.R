
# Style Guideline Note
# 
# Exported function names: dromedaryCase
# Internal function names: .dromedaryCase
# Constant data exported: UPPERCASE
# Function parameters: snake_case
# Function variables: snake_case
#  * (exception) data.frame variable: CamelCase

# Punch List #10, #25, #40
# [DONE] Agree on name/naming and change code to match
# [DONE] Finish cleanup of main typing processing
# [DONE] Deal with coding
# [DONE] Attach error report to "invalid" attr #35
# [DONE] Solve checkbox so all previous outputs are still supported easily
# [DONE] Review existing code and handle all the odd cases
#   * [DONE] Check that all fields exist in the meta data
#   * [DONE] Check that all form names exist in the meta data
#   * [DONE] Check that all event names exist in the events list
#   * [DONE] synchronize underscore codings between records and meta data. NOTE: Only affects calls in REDCap versions earlier than 5.5.21
# Need a callback for cleanup of html and unicode on labels. #24 and Create default assignment function for attaching units #45
#   * [DONE] Create interface
#   * Code review interface
#   * Documentation cleanup
#   * Add actual functions to do work
# [EXTERNALIZED CONCERN-NOT DONE HERE]Handle retries--with batched backoff(?)
# Offline version? testing?
# Cleanup / review / editing pass
# Test cases (If we put in broken data, this will break existing method). Thus get the existing tests working with new method and expect the old one to break.
# [DONE] Figure out the mChoice strategy (dealing with an out of defined scope request from a power user).
# [DONE] Change message from prior to recommend using the new method.
# Fix stop messages to be clear about what caused the stoppage when a user provides an invalid callback.
# [DEFERED] `sql` coding type needs adding #46 (this is too complex to include with this patch).

#' @name fieldValidationAndCasting
#' @title Helper functions for \code{exportRecordsTyped} Validation and Casting
#' @description This set of functions assists in validating that the content of 
#'   fields coming from REDCap match the MetaData, allowing for a 
#'   validation report to provided. The cast helpers allow for transforming
#'   the REDCap data into R data types and allowing the user to customize 
#'   the end product.
#'   
#' @param x character. A vector to check.
#' @param rx character. The regular expression pattern to check.
#' @param coding named character vector. The defined coding from the meta data.
#' @param field_name character(1). Name of the field
#' @param ... Consumes anything else passed to function. I.e., field_name and 
#' coding.
#'
#' @details \code{isNAorBlank} returns TRUE/FALSE if field is NA or blank. Helper
#' function for constructing na overrides in \code{\link{exportRecordsTyped}}.
#' 
#' \code{valRx} constructs a validation function from a regular expression pattern. 
#' The function returns a TRUE/FALSE if the value matches the pattern.
#' 
#' \code{valchoice} constructs a validation function from a set of choices 
#' defined in the MetaData. The functions returns a TRUE/FALSE if the value
#' matches one of the choices.
#' 
#' \code{castLabel} constructs a casting function for multiple choice variables. 
#' The field will be cast to return the choice label (generally more human readable)
#' 
#' \code{castCode} constructs a casting function for multiple choice variables.
#' Similar to \code{castLabel}, but the choice value is returned instead. The
#' values are typically more compact and their meaning may not be obvious.
#' 
#' \code{castRaw} constructs a casting function that returns the content
#' from REDCap as it was received. It is functionally equivalent to \code{identity}
#' 
#' \code{castChecked} constructs a casting function for checkbox fields. It
#' returns values in the form of Unchecked/Checked.
#' 
#' \code{castCheckLabel} and \code{castCheckCode} also construct casting functions
#' for checkbox fields. For both, unchecked variables are cast to an empty 
#' string (""). Checked variables are cast to the option label and option code, 
#' respectively.
#' 
#' @author Shawn Garbett
#' 
#' @export
isNAorBlank <- function(x, ...) is.na(x) | x==''

#' @rdname fieldValidationAndCasting
#' @export
valRx <- function(rx) { function(x, ...) grepl(rx, x) }

#' @rdname fieldValidationAndCasting
#' @export
valChoice <- function(x, field_name, coding) grepl(paste0(coding,collapse='|'), x)

.default_validate <- list(
  date_              = valRx("[0-9]{1,4}-(0?[1-9]|1[012])-(0?[1-9]|[12][0-9]|3[01])"),
  datetime_          = valRx("[0-9]{1,4}-(0?[1-9]|1[012])-(0?[1-9]|[12][0-9]|3[01])\\s([0-9]|0[0-9]|1[0-9]|2[0-3]):[0-5][0-9]"),
  datetime_seconds_  = valRx("[0-9]{1,4}-(0?[1-9]|1[012])-(0?[1-9]|[12][0-9]|3[01])\\s([0-9]|0[0-9]|1[0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]"),
  time_mm_ss         = valRx("[0-5][0-9]:[0-5][0-9]"),
  time_hh_mm_ss      = valRx("([0-9]|0[0-9]|1[0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]"),
  time               = valRx("([0-9]|0[0-9]|1[0-9]|2[0-3]):[0-5][0-9]"),
  float              = valRx("[-+]?(([0-9]+\\.?[0-9]*)|(\\.[0-9]+))([Ee][+-]?[0-9]+)?"),
  number             = valRx("[-+]?(([0-9]+\\.?[0-9]*)|(\\.[0-9]+))([Ee][+-]?[0-9]+)?"),
  calc               = valRx("[-+]?(([0-9]+\\.?[0-9]*)|(\\.[0-9]+))([Ee][+-]?[0-9]+)?"),
  int                = valRx("^[-+]?[0-9]+(|\\.|\\.[0]+)$"),
  integer            = valRx("[-+]?[0-9]+"),
  yesno              = valRx("^(?i)(0|1|yes|no)$"),
  truefalse          = valRx("(0|1|true|false)"),
  checkbox           = valRx("^(?i)(0|1|yes|no)$"),
  form_complete      = valRx("[012]"),
  select             = valChoice,
  radio              = valChoice,
  dropdown           = valChoice,
  sql                = NA # This requires a bit more effort !?
)

#' @export
raw_cast <- list(
  date_              = NA,
  datetime_          = NA,
  datetime_seconds_  = NA,
  time_mm_ss         = NA,
  time_hh_mm_ss      = NA,
  time               = NA,
  float              = NA,
  number             = NA,
  calc               = NA,
  int                = NA,
  integer            = NA,
  yesno              = NA,
  truefalse          = NA,
  checkbox           = NA,
  form_complete      = NA,
  select             = NA,
  radio              = NA,
  dropdown           = NA,
  sql                = NA
)

#' @rdname fieldValidationAndCasting
#' @export
castLabel      <- function(x, coding, field_name) factor(x, levels=coding, labels=names(coding))
#' @rdname fieldValidationAndCasting
#' @export
castCode       <- function(x, coding, field_name) factor(x, levels=coding, labels=coding)
#' @rdname fieldValidationAndCasting
#' @export
castRaw        <- function(x, coding, field_name) x
#' @rdname fieldValidationAndCasting
#' @export
castChecked    <- function(x, coding, field_name) factor(c("Unchecked", "Checked")[(x=='1'|x=='yes')+1], levels=c("Unchecked", "Checked"))
#' @rdname fieldValidationAndCasting
#' @export
castCheckLabel <- function(x, coding, field_name) factor(c("", gsub(".*___(.*)", "\\1",field_name))[(x=='1'|x=='yes')+1], levels=coding, labels=names(coding))
#' @rdname fieldValidationAndCasting
#' @export
castCheckCode  <- function(x, coding, field_name) factor(c("", gsub(".*___(.*)", "\\1",field_name))[(x=='1'|x=='yes')+1], levels=coding, labels=coding)
#' @rdname stripHTMLandUNICODE,
#' @export
stripHTMLandUNICODE <- function(field_name, field_label, field_annotation)
{
  # FIXME FIXME
  x
}
#' @rdname unitsFieldAnnotation
#' @export
unitsFieldAnnotation <- function(field_name, field_label, field_annotation)
{
  m <- gsub('^.*units\\s*=\\s*\\{\\s*"([^"]*)"\\s*\\}.*$', "\\1", field_annotation)

  if(length(m) == 0 || m == field_annotation) NA else m
}


.default_cast <- list(
  date_              = function(x, ...) as.POSIXct(x, format = "%Y-%m-%d"),
  datetime_          = function(x, ...) as.POSIXct(x, format = "%Y-%m-%d %H:%M"),
  datetime_seconds_  = function(x, ...) as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S"),
  time_mm_ss         = function(x, ...) chron::times(ifelse(is.na(x),NA,paste0("00:",x)), format=c(times="h:m:s")),
  time_hh_mm_ss      = function(x, ...) chron::times(x, format=c(times="h:m:s")),
  time               = function(x, ...) chron::times(gsub("(^\\d{2}:\\d{2}$)", "\\1:00", x), 
                                                     format=c(times="h:m:s")),
  float              = as.numeric,
  number             = as.numeric,
  calc               = as.numeric,
  int                = as.integer,
  integer            = as.numeric,
  yesno              = castLabel,
  truefalse          = function(x, ...) x=='1' | tolower(x) =='true',
  checkbox           = castChecked,
  form_complete      = castLabel,
  select             = castLabel,
  radio              = castLabel,
  dropdown           = castLabel,
  sql                = castLabel
)


#' @name exportRecordsTyped
#' 
#' @title A replacement for \code{\link{exportRecords}} with full inversion of control over 
#'        type casting.
#' @description Exports records from a REDCap Database, allowing for 
#'   subsets of subjects, fields, records, and events. This function is
#'   the long term replacement for exportRecords. 
#'
#' @param rcon A REDCap connection object as created by \code{redcapConnection}.
#'
#' @param batch_size integerish(0/1). If length 0, all records are pulled.
#' Otherwise, the records all pulled in batches of this size.
#' 
#' @param fields A character vector of fields to be returned.  If \code{NULL}, 
#'   all fields are returned.
#' @param drop_fields character.: A vector of field names to remove from the export. Ignore if length = 0.
#' @param forms A character vector of forms to be returned.  If \code{NULL}, 
#'   all forms are returned.
#' @param records A vector of study id's to be returned.  If \code{NULL}, all 
#'   subjects are returned.
#' @param events A character vector of events to be returned from a 
#'   longitudinal database.  If \code{NULL}, all events are returned.
#' @param survey specifies whether or not to export the survey identifier field 
#'   (e.g., "redcap_survey_identifier") or survey timestamp fields 
#'   (e.g., form_name+"_timestamp") when surveys are utilized in the project. 
#'   If you do not pass in this flag, it will default to "true". If set to 
#'   "true", it will return the redcap_survey_identifier field and also the 
#'   survey timestamp field for a particular survey when at least 
#'   one field from that survey is being exported. NOTE: If the survey 
#'   identifier field or survey timestamp fields are imported via API data 
#'   import, they will simply be ignored since they are not real fields in 
#'   the project but rather are pseudo-fields.
#' @param dag specifies whether or not to export the "redcap_data_access_group" 
#'   field when data access groups are utilized in the project. If you do not 
#'   pass in this flag, it will default to "false". NOTE: This flag is only 
#'   viable if the user whose token is being used to make the API request is 
#'   *not* in a data access group. If the user is in a group, then this 
#'   flag will revert to its default value.
#' @param date_begin POSIXct(0/1). Ignored if length = 0 (default). Otherwise, records created or modified after this date will be returned.
#' @param date_end POSIXct(0/1). Ignored if length = 0 (default). Otherwise, records created or modified before this date will be returned.
#'
#' @param na list. A list of user specified functions to determine if the
#'   data is NA. This is useful when data is loaded that has coding for NA, e.g.
#'   -5 is NA. Keys must correspond to a truncated REDCap field type, i.e.
#'   {date_, datetime_, datetime_seconds_, time_mm_ss, time_hh_mm_ss, time, float,
#'   number, calc, int, integer, select, radio, dropdown, yesno, truefalse,
#'   checkbox, form_complete, sql}. The function will be provided the variables
#'   (x, field_name, coding). The function must return a vector of logicals
#'   matching the input. It defaults to \code{\link{isNAorBlank}} for all
#'   entries.
#' @param validation list. A list of user specified validation functions. The 
#'   same named keys are supported as the na argument. The function will be 
#'   provided the variables (x, field_name, coding). The function must return a
#'   vector of logical matching the input length. Helper functions to construct
#'   these are \code{\link{valRx}} and \code{\link{valChoice}}. Only fields that
#'   are not identified as NA will be passed to validation functions. 
#' @param cast list. A list of user specified class casting functions. The
#'   same named keys are supported as the na argument. The function will be 
#'   provided the variables (x, field_name, coding). The function must return a
#'   vector of logical matching the input length. See \code{\link{fieldValidationAndCasting}}
#' @param assignment list of functions. These functions are provided, field_name,
#'   label, description and field_type and return a list of attributes to assign
#'   to the column. Defaults to creating a label attribute from the stripped
#'   HTML and UNICODE raw label and scanning for units={"UNITS"} in description
#'   to use as a units attribute.
#' @param mChoice logical. Should a multiple choice variable
#'   be summarized into an \code{Hmisc::mChoice} class and returned with the
#'   \code{data.frame}. If \code{Hmisc} is loaded it will default to TRUE. 
#'   
#' @param data_file For the offline version, a character string giving the location
#'   of the dataset downloaded from REDCap.  Note that this should be the raw
#'   (unlabeled) data set.
#' @param meta_data_file A text string giving the location of the data dictionary 
#'   downloaded from REDCap.
#' @param config named list. Additional configuration parameters to pass to httr::POST,
#' These are appended to any parameters in rcon$config
#' @param api_param named list. Additional API parameters to pass into the body of the
#' API call. This provides users to execute calls with options that may not
#' otherwise be supported by redcapAPI.
#' @param csv_delimiter character. One of \code{c(",", "\t", ";", "|", "^")}. Designates the
#' delimiter for the CSV file received from the API.
#' @details
#' 
#' A record of exports through the API is recorded in the Logging section 
#' of the project.
#' 
#' The 'offline' version of the function operates on the raw (unlabeled) data 
#' file downloaded from REDCap along with the data dictionary.  
#' This is made available for instances where the API can not be accessed for 
#' some reason (such as waiting for API approval from the REDCap administrator).
#' 
#' It is unnecessary to include "redcap_event_name" in the fields argument.  
#' This field is automatically exported for any longitudinal database.  
#' If the user does include it in the fields argument, it is removed quietly 
#' in the parameter checks.
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
#' @export

exportRecordsTyped <-
  function(
    # API Call parameters
    rcon,
    config        = list(),
    api_param     = list(),
    csv_delimiter = ",",
    batch_size    = NULL,

    # Limiters
    fields        = NULL,
    drop_fields   = NULL,
    forms         = NULL,
    records       = NULL,
    events        = NULL,
    survey        = TRUE,
    dag           = TRUE,
    date_begin    = NULL,
    date_end      = NULL,
    ...)
    
    UseMethod("exportRecords")

#' @rdname exportRecordsTyped
#' @export
exportRecordsTyped.redcapApiConnection <- 
  function(
    # API Call parameters
    rcon,  
    config        = list(),
    api_param     = list(),
    csv_delimiter = ",",
    batch_size    = NULL,
    
    # Limiters
    fields        = NULL,
    drop_fields   = NULL,
    forms         = NULL,
    records       = NULL,
    events        = NULL,
    survey        = TRUE,
    dag           = TRUE,
    date_begin    = NULL,
    date_end      = NULL,
    
    # Type Casting Default Overrides Function Lists
    na            = list(),
    validation    = list(),
    cast          = list(),
    assignment    = list(label=stripHTMLandUNICODE,
                         units=unitsFieldAnnotation),
    mChoice       = NULL,
    ...)
{
  if (is.numeric(records)) records <- as.character(records)

   ###########################################################################
  # Check parameters passed to function
  coll <- checkmate::makeAssertCollection()

  checkmate::assert_class(x = rcon,
                          classes = "redcapApiConnection",
                          add = coll)
  
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
  
  checkmate::assert_character(x = events, 
                              any.missing = FALSE,
                              null.ok = TRUE,
                              add = coll)
  
  checkmate::assert_character(x = records, 
                              any.missing = FALSE,
                              null.ok = TRUE,
                              add = coll)
  
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
  
  checkmate::assert_list(x = na, 
                         names = "named", 
                         add = coll)
  
  checkmate::assert_list(x = validation, 
                         names = "named", 
                         add = coll)
  
  checkmate::assert_list(x = cast, 
                         names = "named", 
                         add = coll)
  
  # FIXME: Is field label supposed to be an argument?
  # checkmate::assert_function(x = field_label, 
  #                            null.ok = TRUE, 
  #                            add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Check that fields (and drop_fields) exist in the project
  
  ProjectFields <- rcon$fieldnames()
  available_fields <- unique(c(ProjectFields$original_field_name, 
                               ProjectFields$export_field_name))
  
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
  
  # Check that the events exist in the project
  
  checkmate::assert_subset(x = events, 
                           choices = rcon$events()$unique_event_name, 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
   ###################################################################
  # Combine fields, drop_fields, and forms into the fields that will 
  # be exported
  fields <- .exportRecordsTyped_fieldsArray(rcon = rcon, 
                                                fields = fields, 
                                                drop_fields = drop_fields, 
                                                forms = forms)
  
   ###################################################################
  # See if mChoice argument is passed, otherwise default to state of Hmisc
  if("package:Hmisc" %in% search()) # Hmisc Loaded?
  {
    if(is.null(mChoice)) mChoice <- TRUE
    # Otherwise do what user requests for mChoice
  } else # Hmisc not loaded
  {
    if(is.null(mChoice)) 
    {
      mChoice <- FALSE
    } else if(mChoice)
    {
      warning("mChoice=TRUE requires the package Hmisc to be loaded to function properly.")
      mChoice <- FALSE
    }
  }
  
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
      .exportRecordsFormattedUnbatched( rcon          = rcon, 
                                        body          = body, 
                                        records       = records, 
                                        config        = config, 
                                        api_param     = api_param, 
                                        csv_delimiter = csv_delimiter)
    } else
    {
      .exportRecordsFormattedBatched(  rcon           = rcon, 
                                       body           = body, 
                                       records        = records, 
                                       config         = config, 
                                       api_param      = api_param, 
                                       csv_delimiter  = csv_delimiter, 
                                       batch_size     = batch_size)
    }
  
   ###################################################################
  # Process meta data for useful information
  MetaData <- rcon$metadata()
  
   ###################################################################
  # Derive field information
  field_names <- names(Raw)
  field_bases <- gsub("___.+$", "", field_names)
  field_text_types <- MetaData$text_validation_type_or_show_slider_number[match(field_bases, MetaData$field_name)]
  
  field_types <- MetaData$field_type[match(field_bases, MetaData$field_name)]
  field_types[grepl("_complete$", field_bases)] <- "form_complete"


  # autocomplete was added to the text_validation... column for
  # dropdown menus with the autocomplete feature.
  field_types[field_types == "text" & !is.na(field_text_types)] <-
    field_text_types[field_types == "text" & !is.na(field_text_types)]
  
  field_types <- gsub("_(dmy|mdy|ymd)$", "_", field_types)
  field_types[is.na(field_types)] <- "text"
  
   ###################################################################
  # Derive codings (This is probably a good internal helper)
# FIXME Could this be replaced with fieldChoiceMapping ?
  codebook <- MetaData$select_choices_or_calculations[match(field_bases, MetaData$field_name)]
  codebook[field_types == "form_complete"] <- "0, Incomplete | 1, Unverified | 2, Complete"
  codings <- lapply(
    codebook,
    function(x)
    {
      if(is.na(x) | is.null(x)) return(NA)
      
      x <- strsplit(x, "\\s*\\|\\s*")[[1]]
      y <- gsub("^\\s*(.*),\\s*.*$", "\\1", x)
      names(y) <- gsub("^\\s*.*,\\s*(.*)$", "\\1", x)
      y
    }
  )

   ###################################################################
  # Common provided args for na / validate functions
  args <- lapply(seq_along(Raw),
                 function(x) list(x          = Raw[[x]],
                                  field_name = field_names[x],
                                  coding     = codings[[x]]))
  
   ###################################################################
  # Locate NA's
  funs <- lapply(field_types, function(x) if(is.null(na[[x]])) isNAorBlank else na[[x]])
  nas  <- mapply(do.call, funs, args)
  if(!is.matrix(nas) || (nrow(nas) > 0 && !is.logical(nas[1,1])))
  {
    # FIXME -- Need to provide user the exact failing validate
    stop("User supplied na method not returning vector of logical of correct length")
  }
  
   ###################################################################
  # Run Validation Functions
  validate <- modifyList(.default_validate, validate)
  funs <- lapply(
    field_types,
    function(x)
    { 
      f <- validate[[x]]
      # No validate function is an auto pass
      if(is.null(f)) function(...) rep(TRUE,nrow(Raw)) else f 
    })
  validations <- mapply(do.call, funs, args)
  if(!is.matrix(validations) || (nrow(validations) > 0 && !is.logical(validations[1,1])))
  {
    # FIXME -- Need to provide user the exact failing validate
    stop("User supplied validation method not returning vector of logical of appropriate length")
  }
  
   ###################################################################
  # Type Casting
  Records <- Raw
  cast <- modifyList(.default_cast, cast)
  for(i in seq_along(Raw))
  {
    if(field_types[i] %in% names(cast))
    {
      x <- Raw[[i]]
      x[ nas[[i]] | !validations[[i]] ] <- NA
      Records[[i]] <- cast[[ field_types[i] ]](x, field_name=field_names[i], coding=codings[[i]])
    }
  }
  names(Records) <- names(Raw)
  
   ###################################################################
  # Handle Attributes assignments on columns, #24, #45
  for(i in names(assignment))
  {
    x <- assignment[[i]](field_names, ???Xfield_label, ???Xfield_annotation)
    
    for(j in seq_along(Records)) if(!is.na(x[j])) attr(Records[j], i) <- x[j]
  }

  
   ###################################################################
  # drop_fields
  # FIXME: Benjamin it looks like you dealt with this elsewhere. 
  # Should this be deleted?
  if(length(drop_fields)) Records <- Records[!names(Records) %in% drop_fields]
  
   ###################################################################
  # Attach invalid record information
  selector <- !validations & !nas
  attr(Records, "invalid") <-
    do.call(rbind, lapply(seq_along(Raw), function(i)
    {
      sel <- selector[[i]]
      if(any(sel))
      {
        data.frame(row=seq_len(nrow(Raw))[sel],
                   record_id=Raw[sel, 1],
                   field_name=field_names[i],
                   value=Raw[sel, i])
      } else NULL
    }))
  if(!is.null(Records, "invalid")) warning("Some records failed validation. See 'invalid' attr.")
  
   ###################################################################
  # Convert checkboxes to mChoice if Hmisc is installed and requested
  # FIXME: Will this cause a failure if fields were "dropped" or
  # simply not requested?
  if(mChoice)
  {
    checkbox_meta <- meta_data[which(meta_data$field_type == 'checkbox'),]
    for(i in seq_len(nrow(checkbox_meta)))
    {
      checkbox_fieldname <- checkbox_meta$field_name[i]
      fields <- recordnames[grepl(sprintf("^%s", checkbox_fieldname), recordnames)]
      if(length(fields) > 0)
      {
        # FIXME: Issue-38 when merged will provide this as a function
        opts   <- strsplit(strsplit(checkbox_meta[i,'select_choices_or_calculations'],"\\s*\\|\\s*")[[1]],
                           "\\s*,\\s*")
        levels <- sapply(opts, function(x) x[1+labels])
        # END FIXME
        opts <- as.data.frame(matrix(rep(seq_along(fields), nrow(records)), nrow=nrow(records), byrow=TRUE))
        checked <- records_raw[,fields] != '1'
        opts[which(checked,arr.ind=TRUE)] <- ""
        z <- structure(
          gsub(";$|^;", "",gsub(";{2,}",";", do.call('paste', c(opts, sep=";")))),
          label  = checkbox_fieldname,
          levels = levels,
          class  = c("mChoice", "labelled"))
  
        records[[checkbox_fieldname]] <- z
      }
    }

  } # mChoice 
  
   ###################################################################
  # Return Results 
  Records
}

# Unexported --------------------------------------------------------

.exportRecordsTyped_fieldsArray <- function(rcon = rcon, 
                                           fields = fields, 
                                           drop_fields = drop_fields, 
                                           forms = forms)
{
  FieldFormMap <- rcon$metadata()[c("field_name", "form_name")]
  ProjectFields <- rcon$fieldnames()
  ProjectFields$index <- seq_len(nrow(ProjectFields))
  
  # Make a reference table between fields and forms
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
  
  # By default, we include all fields, so set all is_in_fields to TRUE to start
  FieldFormMap$is_in_fields <- rep(TRUE, nrow(FieldFormMap))
  
  # For the forms, we can't assume they are in forms. Instead, we initialize
  # this to FALSE and have to provide positive proof that they are in forms.
  FieldFormMap$is_in_forms <- rep(FALSE, nrow(FieldFormMap))
  
  # Change is_in_fields to FALSE for those not in fields
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
  
  # FIXME: If we drop fields in the post processing, this block needs to be removed.
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
  
  FieldFormMap <- FieldFormMap[!duplicated(FieldFormMap$original_field_name), ]
  
  # Reduce to fields in either fields or forms
  Fields <- FieldFormMap[FieldFormMap$is_in_fields | 
                           FieldFormMap$is_in_forms, ]
  Fields <- Fields[order(Fields$index), ]
  Fields$original_field_name
}

.exportRecordsFormattedUnbatched <- function( rcon, 
                                              body, 
                                              records, 
                                              config, 
                                              api_param, 
                                              csv_delimiter)
{
  response <- makeApiCall(rcon, 
                          body = c(body, 
                                   api_param, 
                                   vectorToApiBodyList(records, "records")), 
                          config = config)
  
  read.csv(text = as.character(response), 
           stringsAsFactors = FALSE, 
           na.strings = "", 
           colClasses = "character", 
           sep = csv_delimiter)
}

.exportRecordsFormattedBatched <- function( rcon, 
                                            body, 
                                            records, 
                                            config, 
                                            api_param, 
                                            csv_delimiter, 
                                            batch_size)
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
             .exportRecordsFormattedUnbatched(rcon = rcon, 
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

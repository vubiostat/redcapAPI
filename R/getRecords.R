#' @name is_na_or_blank
#' @title Helper function for exportRecords to determine if NA or blank.
#' @description returns TRUE/FALSE if field is NA or blank. Helper
#' function for constructing na overrides in \code{\link{exportRecords}}.
#' 
#' @param x character. A vector to check for NA or blank.
#' @param ... Consumes anything else passed to function. I.e., field_name and 
#' coding.
#' @return logical.
#' @export
is_na_or_blank <- function(x, ...) is.na(x) | x==''

#' @name val_rx
#' @title Construct a validate function from a regex.
#' @description returns function that will validate using the given regex.
#' 
#' @param rx character. The regular expression to check.
#' @param ... Consumes anything else passed to function. I.e., field_name and coding.
#' @return logical.
#' @export
val_rx <- function(rx) { function(x, ...) grepl(rx, x) }

#' @name val_choice
#' @title Validate function for choice variables.
#' @description returns TRUE/FALSE if field matches the coding. Helper for
#' \code{\link{exportRecords}}
#' 
#' @param rx character. The regular expression to check.
#' @param ... Consumes anything else passed to function. I.e., field_name and coding.
#' @return logical.
#' @export
val_choice <- function(x, field_name, coding) grepl(paste0(coding,col='|'), x)

default_na <- list(
  date_              = is_na_or_blank,
  datetime_          = is_na_or_blank,
  datetime_seconds_  = is_na_or_blank,
  time_mm_ss         = is_na_or_blank,
  time_hh_mm_ss      = is_na_or_blank,
  time               = is_na_or_blank,
  float              = is_na_or_blank,
  number             = is_na_or_blank,
  calc               = is_na_or_blank,
  int                = is_na_or_blank,
  integer            = is_na_or_blank,
  yesno              = is_na_or_blank,
  truefalse          = is_na_or_blank,
  checkbox           = is_na_or_blank,
  form_complete      = is_na_or_blank,
  select             = is_na_or_blank,
  radio              = is_na_or_blank,
  dropdown           = is_na_or_blank,
  sql                = is_na_or_blank
)

default_validate <- list(
  date_              = val_rx("[0-9]{1,4}-(0?[1-9]|1[012])-(0?[1-9]|[12][0-9]|3[01])"),
  datetime_          = val_rx("[0-9]{1,4}-(0?[1-9]|1[012])-(0?[1-9]|[12][0-9]|3[01])\\s([0-9]|0[0-9]|1[0-9]|2[0-3]):[0-5][0-9]"),
  datetime_seconds_  = val_rx("[0-9]{1,4}-(0?[1-9]|1[012])-(0?[1-9]|[12][0-9]|3[01])\\s([0-9]|0[0-9]|1[0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]"),
  time_mm_ss         = val_rx("[0-5][0-9]:[0-5][0-9]"),
  time_hh_mm_ss      = val_rx("([0-9]|0[0-9]|1[0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]"),
  time               = val_rx("([0-9]|0[0-9]|1[0-9]|2[0-3]):[0-5][0-9]:[0-5][0-9]"),
  float              = val_rx("[-+]?(([0-9]+\\.?[0-9]*)|(\\.[0-9]+))([Ee][+-]?[0-9]+)?"),
  number             = val_rx("[-+]?(([0-9]+\\.?[0-9]*)|(\\.[0-9]+))([Ee][+-]?[0-9]+)?"),
  calc               = val_rx("[-+]?(([0-9]+\\.?[0-9]*)|(\\.[0-9]+))([Ee][+-]?[0-9]+)?"),
  int                = val_rx("^[-+]?[0-9]+(|\\.|\\.[0]+)$"),
  integer            = val_rx("[-+]?[0-9]+"),
  yesno              = val_rx("^(?i)(0|1|yes|no)$"),
  truefalse          = val_rx("(0|1|true|false)"),
  checkbox           = val_rx("^(?i)(0|1|yes|no)$"),
  form_complete      = val_rx("[012]"),
  select             = val_choice,
  radio              = val_choice,
  dropdown           = val_choice,
  sql                = val_choice # This requires a bit more effort !?
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

default_cast <- list(
  date_              = function(x, ...) as.POSIXct(x, format = "%Y-%m-%d"),
  datetime_          = function(x, ...) as.POSIXct(x, format = "%Y-%m-%d %H:%M"),
  datetime_seconds_  = function(x, ...) as.POSIXct(x, format = "%Y-%m-%d %H:%M:%S"),
  time_mm_ss         = function(x, ...) chron::times(paste0("00:",x), format=c(times="h:m:s")),
  time_hh_mm_ss      = function(x, ...) chron::times(x, format=c(times="h:m:s")),
  time               = function(x, ...) chron::times(gsub("(^\\d{2}:\\d{2}$)", "\\1:00", x), 
                                                     format=c(times="h:m:s")),
  float              = as.numeric,
  number             = as.numeric,
  calc               = as.numeric,
  int                = as.integer,
  integer            = as.numeric,
  yesno              = function(x, coding, ...) factor(x, levels=coding, labels=coding),
  truefalse          = function(x, ...) x == 'true',
  checkbox           = function(x, ...) x == '1', # FIXME!! Under discussion
  form_complete      = function(x, coding, ...) factor(x, levels=coding, labels=coding),
  select             = function(x, coding, ...) factor(x, levels=coding, labels=coding),
  radio              = function(x, coding, ...) factor(x, levels=coding, labels=coding),
  dropdown           = function(x, coding, ...) factor(x, levels=coding, labels=coding),
  sql                = function(x, coding, ...) factor(x, levels=coding, labels=coding)
)


#' @name getRecords
#' 
#' @title A replacement for \code{\link{exportRecords}} with full inversion of control over casting.
#' @description Exports records from a REDCap Database, allowing for 
#'   subsets of subjects, fields, records, and events.
#'   
#' @param rcon A REDCap connection object as created by \code{redcapConnection}.
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
#' @param csv_delimiter character. One of c(",", "\t", ";", "|", "^"). Designates the
#' delimiter for the CSV file received from the API.
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
#'   matching the input. It defaults to \code{\link{is_na_or_blank}} for all
#'   entries.
#' @param validation list. A list of user specified validation functions. The 
#'   same named keys are supported as the na argument. The function will be 
#'   provided the variables (x, field_name, coding). The function must return a
#'   vector of logical matching the input length. Helper functions to construct
#'   these are \code{\link{val_rx}} and \code{\link{val_choice}}. Only fields that
#'   are not identified as NA will be passed to validation functions. 
#' @param cast list. A list of user specified class casting functions. The
#'   same named keys are supported as the na argument. The function will be 
#'   provided the variables (x, field_name, coding). The function must return a
#'   vector of logical matching the input length.
#'  
#' @details
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
#  #' @section Checkbox Variables:
#  #' 
#  #' There are four ways the data from checkbox variables may be
#  #' represented depending on the values of \code{factors} and
#  #' \code{checkboxLabels}. The most common are the first and third
#  #' rows of the table below.  When \code{checkboxLabels = TRUE}, either
#  #' the coded value or the labelled value is returned if the box is
#  #' checked, or an empty string if it is not.
#  #' 
#  #' \tabular{lll}{
#  #' \code{factors} \tab \code{checkboxLabels} \tab Output \cr
#  #' \code{FALSE}   \tab \code{FALSE}          \tab 0 / 1 \cr
#  #' \code{FALSE}   \tab \code{TRUE}           \tab "" / value \cr
#  #' \code{TRUE}    \tab \code{FALSE}          \tab Unchecked / Checked \cr
#  #' \code{TRUE}    \tab \code{TRUE}           \tab "" / label
#  #' }
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
#' @export

getRecords <-
  function(rcon,        
           config=NULL, api_parm=NULL,    csv_delimiter=",", batch_size=0,
           fields=NULL, drop_fields=NULL, forms=NULL,        records=NULL,  events=NULL,
           survey=TRUE, dag=TRUE,         date_begin=NULL,   date_end=NULL,
           na=NULL,     validation=NULL,  cast=NULL, ...)
    
    UseMethod("exportRecords")

#' @rdname exportRecords
#' @export

getRecords.redcapApiConnection <- 
  function(rcon,        
           config=NULL, api_parm=NULL,    csv_delimiter=",", batch_size=0,
           fields=NULL, drop_fields=NULL, forms=NULL,        records=NULL,  events=NULL,
           survey=TRUE, dag=TRUE,         date_begin=NULL,   date_end=NULL,
           na=NULL,     validation=NULL,  cast=NULL,         ...)
{
  if (is.numeric(records)) records <- as.character(records)

  ###########################################################################
  # Check parameters passed to function
  coll <- checkmate::makeAssertCollection()

  checkmate::assert_class(x = rcon,
                          classes = "redcapApiConnection",
                          add = coll)

  massert(~ factors + labels + dates + survey + dag + checkboxLabels +
            form_complete_auto,
          fun = checkmate::assert_logical,
          fixed = list(len = 1,
                       add = coll))

  massert(~ fields + forms + records + events,
          fun = checkmate::assert_character,
          fixed = list(null.ok = TRUE,
                       add = coll))

  checkmate::assert_integerish(x = batch.size,
                               len = 1,
                               add = coll)

  error_handling <- checkmate::matchArg(x = error_handling,
                                        choices = c("null", "error"),
                                        add = coll)
  
  if (is.list(colClasses)){
    colClasses <- unlist(colClasses)
  }
  
    checkmate::assert_character(x = colClasses, 
                                names = "named", 
                                add = coll)

  checkmate::reportAssertions(coll)
  
  ###################################################################
  # Begin main processing
  meta_data <- rcon$metadata()
  
  #* for purposes of the export, we don't need the descriptive fields.
  #* Including them makes the process more error prone, so we'll ignore them.
  meta_data <- meta_data[!meta_data$field_type %in% "descriptive", ]

  #* Secure the events table
  events_list <- rcon$events()

  #* Secure the REDCap version
  version <- rcon$version()

  form_complete_fields <-
    sprintf("%s_complete",
            unique(meta_data$form_name))
  form_complete_fields <-
    form_complete_fields[!is.na(form_complete_fields)]

  #* Check that all fields exist in the meta data
  if (!is.null(fields))
  {
    bad_fields <- fields[!fields %in% c(meta_data$field_name,
                                        form_complete_fields)]
    if (length(bad_fields))
      coll$push(paste0("The following are not valid field names: ",
                       paste0(bad_fields, collapse = ", ")))
  }

  #* Check that all form names exist in the meta data
  if (!is.null(forms))
  {
    bad_forms <- forms[!forms %in% meta_data$form_name]
    if (length(bad_forms))
      coll$push(paste0("The following are not valid form names: ",
                       paste0(bad_forms, collapse = ", ")))
  }

  #* Check that all event names exist in the events list
  if (!is.null(events) && inherits(events_list, "data.frame"))
  {
    bad_events <- events[!events %in% events_list$unique_event_name]
    if (length(bad_events))
      coll$push(paste0("The following are not valid event names: ",
                       paste0(bad_events, collapse = ", ")))
  }

  checkmate::reportAssertions(coll)

  #* Create the vector of field names
  if (!is.null(fields)) #* fields were provided
  {
    # redcap_event_name is automatically included in longitudinal projects
    field_names <- fields[!fields %in% "redcap_event_name"]
  }
  else if (!is.null(forms))
  {
    field_names <- meta_data$field_name[meta_data$form_name %in% forms]
  }
  else
    #* fields were not provided, default to all fields.
    field_names <- meta_data$field_name

  #* Expand 'field_names' to include fields from specified forms.
  if (!is.null(forms))
    field_names <-
    unique(c(field_names,
             meta_data$field_name[meta_data$form_name %in% forms]))


  suffixed <-
    checkbox_suffixes(
      # The subset prevents `[form]_complete` fields from
      # being included here.
      fields = field_names[field_names %in% meta_data$field_name],
      meta_data = meta_data)

  # Identify the forms from which the chosen fields are found
  included_form <-
    unique(
      meta_data$form_name[meta_data$field_name %in% field_names]
    )

  # Add the form_name_complete column to the export
  if (form_complete_auto){
    field_names <- c(field_names,
                     sprintf("%s_complete", included_form))
  }

  body <- list(token = rcon$token,
               content = 'record',
               format = 'csv',
               type = 'flat',
               exportSurveyFields = tolower(survey),
               exportDataAccessGroups = tolower(dag),
               returnFormat = 'csv')

  body[['fields']] <- paste0(field_names, collapse=",")
  if (!is.null(forms)) body[['forms']] <- paste0(forms, collapse=",")
  if (!is.null(events)) body[['events']] <- paste0(events, collapse=",")
  if (!is.null(records)) body[['records']] <- paste0(records, collapse=",")
  
  if (batch.size < 1){
    x <- unbatchedGet(rcon = rcon,
                   body = body,
                   id = meta_data$field_name[1],
                   colClasses = colClasses,
                   error_handling = error_handling)
  }
  else
  {
    x <- batchedGet(rcon = rcon,
                 body = body,
                 batch.size = batch.size,
                 id = meta_data$field_name[1],
                 colClasses = colClasses,
                 error_handling = error_handling)
  }

  #* synchronize underscore codings between records and meta data
  #* Only affects calls in REDCap versions earlier than 5.5.21
  if (utils::compareVersion(version, "6.0.0") == -1)
    meta_data <- syncUnderscoreCodings(x, meta_data)

  x <- fieldToVar(records = x,
                  meta_data = meta_data,
                  factors = factors,
                  dates = dates,
                  labels = labels,
                  checkboxLabels = checkboxLabels,
                  ...)

  if (labels){
    x[,suffixed$name_suffix] <-
      mapply(nm = suffixed$name_suffix,
             lab = suffixed$label_suffix,
             FUN = function(nm, lab){
               if(is.null(x[[nm]])){
                  warning("Missing field for suffix ", nm)
               } else {
                  labelVector::set_label(x[[nm]], lab)
               }
             },
             SIMPLIFY = FALSE)
  }

  
  
  # drop
  if(length(drop)) {
    x <- x[!names(x) %in% drop]
  } # end drop
  
  x
}



#*** UNBATCHED EXPORT
unbatchedGet <- function(rcon, body, id, colClasses, error_handling)
{
  colClasses[[id]] <- "character"
  colClasses <- colClasses[!vapply(colClasses,
                                   is.na,
                                   logical(1))]
  
  x <- httr::POST(url = rcon$url, 
                  body = body, 
                  config = rcon$config)
  
  if (x$status_code != 200) redcap_error(x, error_handling = error_handling)
  
  x <- as.character(x)
  # probably not necessary for data.  Useful for meta data though. (See Issue #99)
  # x <- iconv(x, "utf8", "ASCII", sub = "")
  utils::read.csv(text = x, 
                  stringsAsFactors = FALSE, 
                  na.strings = "",
                  colClasses = colClasses)
}


#*** BATCHED EXPORT
batchedGet <- function(rcon, body, batch.size, id, colClasses, error_handling)
{
  colClasses[[id]] <- "character"
  colClasses <- colClasses[!vapply(colClasses,
                                   is.na,
                                   logical(1))]
  
  #* 1. Get the IDs column
  #* 2. Restrict to unique IDs
  #* 3. Determine if the IDs look hashed (de-identified)
  #* 4. Give warning about potential problems joining hashed IDs
  #* 5. Read batches
  #* 6. Combine tables
  #* 7. Return full data frame
  
  
  #* 1. Get the IDs column
  id_body <- body
  id_body[['fields']] <- id
  IDs <- httr::POST(url = rcon$url,
                    body = id_body,
                    config = rcon$config)
  
  if (IDs$status_code != 200) redcap_error(IDs, error_handling)
  
  IDs <- as.character(IDs)
  # probably not necessary for data.  Useful for meta data though. (See Issue #99)
  # IDs <- iconv(IDs, "utf8", "ASCII", sub = "")
  IDs <- utils::read.csv(text = IDs,
                         stringsAsFactors = FALSE,
                         na.strings = "",
                         colClasses = colClasses[id])
  
  #* 2. Restrict to unique IDs
  unique_id <- unique(IDs[[id]])
  
  #* 3. Determine if the IDs look hashed (de-identified)
  #* 4. Give warning about potential problems joining hashed IDs
  if (all(nchar(unique_id) == 32L))
  {
    warning("The record IDs in this project appear to be de-identified. ",
            "Subject data may not match across batches. ",
            "See 'Deidentified Batched Calls' in '?exportRecords'")
  }
  
  #* Determine batch numbers for the IDs.
  batch.number <- rep(seq_len(ceiling(length(unique_id) / batch.size)),
                      each = batch.size,
                      length.out = length(unique_id))
  
  #* Make a list to hold each of the batched calls
  #* Borrowed from http://stackoverflow.com/a/8099431/1017276
  batch_list <- vector("list", max(batch.number))

  #* 5. Read batches
  for (i in unique(batch.number))
  {
    body[['records']] <- paste0(unique_id[batch.number == i], collapse = ",")
    x <- httr::POST(url = rcon$url, 
                    body = body, 
                    config = rcon$config)
    
    if (x$status_code != 200) redcap_error(x, error_handling = "error")
    
    x <- as.character(x)
    # probably not necessary for data.  Useful for meta data though. (See Issue #99)
    # x <- iconv(x, "utf8", "ASCII", sub = "")
    batch_list[[i]] <- utils::read.csv(text = x,
                                       stringsAsFactors = FALSE,
                                       na.strings = "",
                                       colClasses = colClasses)
    Sys.sleep(1)
  }
  
  #* 6. Combine tables and return
  do.call("rbind", batch_list)
}

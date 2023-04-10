# Style Guideline Note
# 
# Exported function names: dromedaryCase
# Internal function names: .dromedaryCase
# Constant data exported: UPPERCASE
# Function parameters: snake_case
# Function variables: snake_case
#  * (exception) data.frame variable: CamelCase

# FIXME: Test cases (If we put in broken data, this will break existing method). Thus get the existing tests working with new method and expect the old one to break.
# [EXTERNALIZED CONCERN-NOT DONE HERE]Handle retries--with batched backoff(?)
# [DEFERRED] Offline version? testing? [A lot of the code could be reused if we
#            can identify pieces to turn into subroutines.]
# [DEFERED] `sql` coding type needs adding #46 (this is too complex to include with this patch).

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
#'   the export. Ignored if length = 0.
#' @param forms \code{character} vector of forms to be returned.  If \code{NULL}, 
#'   all forms are returned (unless \code{fields} is specified).
#' @param records \code{character} or \code{integerish}. A vector of study id's 
#'   to be returned.  If \code{NULL}, all subjects are returned.
#' @param events A \code{character} vector of events to be returned from a 
#'   longitudinal database.  If \code{NULL}, all events are returned.
#' @param survey \code{logical(1)} specifies whether or not to export the survey identifier field 
#'   (e.g., "redcap_survey_identifier") or survey timestamp fields 
#'   (e.g., form_name+"_timestamp") when surveys are utilized in the project. 
#' @param dag \code{logical(1)} specifies whether or not to export the "redcap_data_access_group" 
#'   field when data access groups are utilized in the project. NOTE: This argument is only 
#'   viable if the user whose token is being used to make the API request is 
#'   not in a data access group. If the user is in a group, then this 
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
#'   same named keys are supported as the \code{na} argument. The function will be 
#'   provided the variables (x, field_name, coding). The function must return a
#'   vector of logical matching the input length. Helper functions to construct
#'   these are \code{\link{valRx}} and \code{\link{valChoice}}. Only fields that
#'   are not identified as NA will be passed to validation functions. 
#' @param cast A named \code{list} of user specified class casting functions. The
#'   same named keys are supported as the \code{na} argument. The function will be 
#'   provided the variables (x, field_name, coding). The function must return a
#'   vector of logical matching the input length. See \code{\link{fieldValidationAndCasting}}
#' @param assignment A named \code{list} of functions. These functions are provided, field_name,
#'   label, description and field_type and return a list of attributes to assign
#'   to the column. Defaults to creating a label attribute from the stripped
#'   HTML and UNICODE raw label and scanning for units={"UNITS"} in description
#'   to use as a units attribute.
#' @param mChoice character. Must be NULL, logical(1), \code{"coded"} or \code{"labelled"}.
#'   If NULL, \code{coded}, or \code{labelled} and \code{Hmisc} is loaded, it will summarize a multiple choice variable
#'   convert. If NULL and \code{Hmisc} is not loaded it will
#'   do no conversion. If FALSE it is disabled entirely. The summarized
#'   \code{Hmisc::mChoice} class is returned as an additional column in the
#'   \code{data.frame}.
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
#' @details
#' 
#' A record of exports through the API is recorded in the Logging section 
#' of the project.
#' 
# The 'offline' version of the function operates on the raw (unlabeled) data 
# file downloaded from REDCap along with the data dictionary.  
# This is made available for instances where the API can not be accessed for 
# some reason (such as waiting for API approval from the REDCap administrator).
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
#' \itemize{
#'   \item{1. }{Detect NAs in returned data (\code{na} argument).}
#'   \item{2. }{Run \code{validate} functions for the field_types.}
#'   \item{3. }{On the fields that are not NA and pass validate do the specified cast.}
#' }
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
#' as \code{cast=list(date_=as.Date))}. See \code{\link{fieldValidationAndCasting}}
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
#' list of IDs is then broken into chunks, each about the size of \code{batch_size}.
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

    # Limiters
    fields        = NULL,
    drop_fields   = NULL,
    forms         = NULL,
    records       = NULL,
    events        = NULL,
    ...)
    
    UseMethod("exportRecordsTyped")

#' @rdname exportRecordsTyped
#' @export

exportRecordsTyped.redcapApiConnection <- 
  function(
    # API Call parameters
    rcon,  
    
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
    assignment    = list(label=stripHTMLandUnicode,
                         units=unitsFieldAnnotation),
    mChoice       = NULL,
    ..., 
    config        = list(),
    api_param     = list(),
    csv_delimiter = ",",
    batch_size    = NULL)
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
  
  checkmate::assert_list(x = assignment, 
                         names = "named", 
                         types = "function",
                         add = coll)
  
  # mChoice is a bit loose with user expectations
  checkmate::assert(
    checkmate::check_character(
      x = mChoice,
      len=1,
      null.ok=TRUE,
      pattern="coded|labelled"),
    checkmate::check_logical(
      x = mChoice, 
      len = 1, 
      null.ok=TRUE),
    add = coll
  )

  checkmate::reportAssertions(coll)
  
  # Check that fields (and drop_fields) exist in the project
  
  MetaData <- rcon$metadata()
  ProjectFields <- rcon$fieldnames()
  available_fields <- unique(c(ProjectFields$original_field_name, 
                               ProjectFields$export_field_name, 
                               MetaData$field_name[MetaData$field_type %in% c("calc", "file")]))
  
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
  fields <- .exportRecordsTyped_fieldsArray(rcon        = rcon, 
                                            fields      = fields, 
                                            drop_fields = drop_fields, 
                                            forms       = forms)
  
   ###################################################################
  # Figure out defaults for mChoice
  if("package:Hmisc" %in% search()) # Hmisc Loaded?
  {
    if(is.null(mChoice) || mChoice == TRUE) mChoice <- "coded"
    if(mChoice == FALSE) mChoice <- NULL
    # Otherwise do what user requests for mChoice
  } else # Hmisc not loaded
  {
    if(is.null(mChoice) || mChoice==FALSE)
    {
      mChoice <- NULL
    } else 
    {
      warning("mChoice requires the package Hmisc to be loaded to function properly.")
      mChoice <- NULL
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
  
   ###################################################################
  # Derive field information
  field_names <- names(Raw)
  field_bases <- gsub("___.+$", "", field_names)
  field_text_types <- MetaData$text_validation_type_or_show_slider_number[match(field_bases, MetaData$field_name)]
  field_map <- match(field_bases, MetaData$field_name)
  
  field_types <- MetaData$field_type[field_map]
  field_types[grepl("_complete$", field_bases)] <- "form_complete"

  # autocomplete was added to the text_validation... column for
  # dropdown menus with the autocomplete feature.
  field_types[field_types == "text" & !is.na(field_text_types)] <-
    field_text_types[field_types == "text" & !is.na(field_text_types)]
  
  field_types <- gsub("_(dmy|mdy|ymd)$", "_", field_types)
  field_types[is.na(field_types)] <- "text"
  
   ###################################################################
  # Derive codings (This is probably a good internal helper)
  codebook <- MetaData$select_choices_or_calculations[field_map]
  codebook[field_types == "form_complete"] <- "0, Incomplete | 1, Unverified | 2, Complete"

  codings <- vector("list", length = length(codebook))

  for (i in seq_along(codings)){
    codings[[i]] <-
      if (is.na(codebook[i])){
        NA_character_
      } else {
        this_mapping <- fieldChoiceMapping(object = codebook[i],
                                           field_name = field_names[i])
        this_coding <- this_mapping[, 1]
        names(this_coding) <- this_mapping[, 2]
        this_coding
      }
  }

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
  if(!is.matrix(nas))
  {
    m <- unique(field_types[sapply(nas, class) != "logical"])
    stop(paste("User supplied na method for [",
               paste(m, collapse=", "),
               "] not returning vector of logical of correct length"))
  } else if (nrow(nas) > 0 && !is.logical(nas[1,1]))
  {
    stop("Supplied na methods must return logical vectors")
  }
   
   ###################################################################
  # Run Validation Functions
  validate <- modifyList(.default_validate, validation)
  funs <- lapply(
    field_types,
    function(x)
    { 
      f <- validate[[x]]
      # No validate function is an auto pass
      if(is.null(f)) function(...) rep(TRUE,nrow(Raw)) else f 
    })
  validations <- mapply(do.call, funs, args)

  if(!is.matrix(validations))
  {
    m <- unique(field_types[sapply(validations, class) != "logical"])
    stop(paste("User supplied validation method for [",
               paste(m, collapse=", "),
               "] not returning vectors of correct length logical"))
  } else if (nrow(validations) > 0 && !is.logical(validations[1,1]))
  {
    stop("Supplied validation methods must return logical vectors")
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
      x[ nas[,i] | !validations[,i] ] <- NA
      typecast <- cast[[ field_types[i] ]]
      if(is.function(typecast))
        Records[[i]] <- typecast(x, field_name=field_names[i], coding=codings[[i]])
    }
  }
  names(Records) <- names(Raw)
  
   ###################################################################
  # Handle Attributes assignments on columns, #24, #45
  for(i in names(assignment))
  {
    x <- assignment[[i]](field_names, MetaData$field_label[field_map], MetaData$field_annotation[field_map])
    for(j in seq_along(Records)) if(!is.na(x[j])) attr(Records[,j], i) <- x[j]
  }

   ###################################################################
  # Attach invalid record information
  selector <- !validations & !nas
  attr(Records, "invalid") <-
    do.call(rbind, lapply(seq_along(Raw), function(i)
    {
      sel <- selector[,i]
      if(any(sel))
      {
        if("record_id" %in% colnames(Raw))
        {
          data.frame(row=seq_len(nrow(Raw))[sel],
                     record_id=Raw[sel, "record_id"],
                     field_name=field_names[i],
                     value=Raw[sel, i])
        } else
        {
          data.frame(row=seq_len(nrow(Raw))[sel],
                     field_name=field_names[i],
                     value=Raw[sel, i])
        }
      } else NULL
    }))
  if(!is.null(attr(Records, "invalid"))) warning("Some records failed validation. See 'invalid' attr.")
  
   ###################################################################
  # Convert checkboxes to mChoice if Hmisc is installed and requested
  if(!is.null(mChoice))
  {
    CheckboxMetaData <- MetaData[MetaData$field_type == "checkbox", ]
    
    checkbox_fields <- fields[fields %in% CheckboxMetaData$field_name]
    
    for (i in seq_along(checkbox_fields))
      Records[[ checkbox_fields[i] ]] <- 
        .mChoiceField(rcon, 
                     records_raw = Raw, 
                     checkbox_fieldname = checkbox_fields[i], 
                     style = mChoice)
  } 
  
   ###################################################################
  # Return Results 
  Records
}


 #######################################################################
# Unexported

 #######################################################################
# mChoice Function
.mChoiceField <- function(rcon, 
                         records_raw, 
                         checkbox_fieldname, 
                         style = c("coded", "labelled")){

   ##################################################################
  # Argument Validation 
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::assert_data_frame(records_raw, 
                               add = coll)
  
  checkmate::assert_character(x = checkbox_fieldname, 
                              len = 1, 
                              any.missing = FALSE, 
                              add = coll)
  
  style <- checkmate::matchArg(x = style, 
                               choices = c("coded", "labelled"), 
                               add = coll)
  
  checkmate::reportAssertions(coll)
  
  FieldNames <- rcon$fieldnames()
  
  checkmate::assert_subset(x = checkbox_fieldname, 
                           choices = FieldNames$original_field_name, 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
  MetaData <- rcon$metadata()
  
  field_type <- MetaData$field_type[MetaData$field_name == checkbox_fieldname]
  
  if (field_type != "checkbox"){
    coll$push(sprintf("'%s' is not a checkbox field; it cannot be made into an mChoice field", 
                      checkbox_fieldname))
    
    checkmate::reportAssertions(coll)
  }
  
   ##################################################################
  # Make the mChoice field
  
  # get the suffixed field names
  fields <- FieldNames$export_field_name[FieldNames$original_field_name %in% checkbox_fieldname]
  
  if (length(fields) == 0) return(NULL)

  # get the options
  opts   <- fieldChoiceMapping(rcon, checkbox_fieldname)
  levels <- opts[, 1+(style == "labelled"), drop = TRUE]
  
  # Make the data frame to store the status of the options
  opts <- as.data.frame(matrix(rep(seq_along(fields), nrow(records_raw)), nrow=nrow(records_raw), byrow=TRUE))
  checked <- records_raw[,fields] != '1' # Logical value indicating if the choice was checked
  opts[which(checked,arr.ind=TRUE)] <- "" # Make unchecked choices an empty string
  
  # Consolidate choices into the mChoice object
  structure(
    gsub(";$|^;", "",gsub(";{2,}",";", do.call('paste', c(opts, sep=";")))),
    label  = checkbox_fieldname,
    levels = levels,
    class  = c("mChoice", "labelled"))
}

.exportRecordsTyped_fieldsArray <- function(rcon = rcon, 
                                            fields = fields, 
                                            drop_fields = drop_fields, 
                                            forms = forms)
{
  MetaData <- rcon$metadata()
  
  # exportFieldNames excludes fields of type calc, descriptive, and file
  # We need to wedge them in here or we'll never get them out of the API
  ProjectFields <- rcon$fieldnames()
  
  MissingFromFields <- MetaData[MetaData$field_type %in% c("calc", 
                                                           "file"), ]
  MissingFromFields <- 
    data.frame(original_field_name = MissingFromFields$field_name, 
               choice_value = NA, 
               export_field_name = MissingFromFields$field_name, 
               stringsAsFactors = FALSE)
  
  ProjectFields <- rbind(ProjectFields, MissingFromFields)
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

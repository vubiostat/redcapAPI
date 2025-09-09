#' @describeIn recordsTypedMethods Export records with type casting.
#' @order 1
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

#' @rdname recordsTypedMethods
#' @order 3
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
    dag            = FALSE,
    date_begin     = NULL,
    date_end       = NULL,

    # Type Casting Default Overrides Function Lists
    na             = list(),
    validation     = list(),
    cast           = list(),
    assignment     = list(label=stripHTMLandUnicode,
                          units=unitsFieldAnnotation),
    filter_empty_rows = TRUE,
    warn_zero_coded = TRUE,
    ...,
    csv_delimiter  = ",",
    batch_size     = NULL)
{
  if (is.numeric(records)) records <- as.character(records)

   ###########################################################################
  # Check parameters passed to function
  coll <- checkmate::makeAssertCollection()

  checkmate::assert_class(x = rcon,
                          classes = "redcapApiConnection",
                          add = coll)

  .exportRecordsTyped_validateCommonArgument(
    fields          = fields,
    drop_fields     = drop_fields,
    forms           = forms,
    records         = records,
    events          = events,
    na              = na,
    validation      = validation,
    cast            = cast,
    assignment      = assignment,
    coll            = coll,
    warn_zero_coded = warn_zero_coded,
    ...)

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

  checkmate::assert_logical(x = filter_empty_rows,
                            len = 1,
                            any.missing = FALSE,
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

  # dag must be set to TRUE to pull the dag field
  if(length(fields) > 0 && "redcap_data_access_group" %in% fields) dag <- TRUE

  # The REDCap API will not accept system fields in the fields argument.
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

  # We do not need to pass forms to the API because we have
  # absorbed that information directly into fields
  body <- c(list(content                = "record",
                 format                 = "csv",
                 returnFormat           = "csv",
                 type                   = "flat",
                 exportSurveyFields     = tolower(survey),
                 exportDataAccessGroups = tolower(dag),
                 csvDelimiter           = csv_delimiter),
            vectorToApiBodyList(fields, "fields"),
            vectorToApiBodyList(events, "events"))

  if(!is.null(date_begin)) body$dateRangeBegin = format(date_begin, format = "%Y-%m-%d %H:%M:%S")
  if(!is.null(date_end))   body$dateRangeEnd   = format(date_end,   format = "%Y-%m-%d %H:%M:%S")

  Raw <-
    if (length(batch_size) == 0)
    {
      .exportRecordsTyped_Unbatched( rcon           = rcon,
                                     body           = body,
                                     records        = records,
                                     csv_delimiter  = csv_delimiter,
                                     ...)
    } else
    {
      .exportRecordsTyped_Batched(  rcon           = rcon,
                                    body           = body,
                                    records        = records,
                                    csv_delimiter  = csv_delimiter,
                                    batch_size     = batch_size,
                                    ...)
    }

  if (identical(Raw, data.frame())){
    return(Raw)
  }

  if (user_requested_system_fields){
    if (user_requested_only_system_fields){
      Raw <- Raw[-1]
    }

    unrequested_fields <- REDCAP_SYSTEM_FIELDS[!REDCAP_SYSTEM_FIELDS %in% system_fields_user_requested]
    Raw <- Raw[!names(Raw) %in% unrequested_fields]
  }

  if(!is.null(forms)         &&
     nrow(rcon$mapping()) >0 &&
     "redcap_event_name" %in% names(Raw))
  {
    map <- rcon$mapping()
    form_events <- map$unique_event_name[map$form %in% forms]
    Raw <- Raw[Raw$redcap_event_name %in% form_events,]
  }

  # See fieldCastingFunctions.R for definition of .castRecords
  CastData <- .castRecords(Raw              = Raw,
                           Records          = NULL,
                           rcon             = rcon,
                           na               = na,
                           validation       = validation,
                           cast             = cast,
                           assignment       = assignment,
                           default_cast     = .default_cast,
                           default_validate = .default_validate,
                           warn_zero_coded  = warn_zero_coded)

  if(filter_empty_rows) CastData <- filterEmptyRow(CastData, rcon)

  CastData
}



#' @describeIn recordsTypedMethods Export records without API access.
#' @order 4
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
                                                       warn_zero_coded = TRUE,
                                                       ...){

  if (is.numeric(records)) records <- as.character(records)

  ###################################################################
  # Argument Validation

  coll <- checkmate::makeAssertCollection()

  checkmate::assert_class(x = rcon,
                          classes = "redcapOfflineConnection",
                          add = coll)

  .exportRecordsTyped_validateCommonArgument(
    fields          = fields,
    drop_fields     = drop_fields,
    forms           = forms,
    records         = records,
    events          = events,
    na              = na,
    validation      = validation,
    cast            = cast,
    assignment      = assignment,
    coll            = coll,
    warn_zero_coded = warn_zero_coded,
    ...)

  checkmate::reportAssertions(coll)

  checkmate::assert_data_frame(x = rcon$metadata(),
                               .var.name = "rcon$metadata()",
                               add = coll)

  checkmate::assert_data_frame(x = rcon$records(),
                               .var.name = "rcon$records()",
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

  # The REDCap API will not accept system fields in the fields argument.
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
               default_validate = .default_validate,
               batch_size       = NULL,
               warn_zero_coded  = warn_zero_coded)
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
                                                       coll,
                                                       warn_zero_coded,
                                                       ...)
{
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
                         null.ok = TRUE,
                         add = coll)

  checkmate::assert_list(x = cast,
                         names = "named",
                         add = coll)

  checkmate::assert_list(x = assignment,
                         names = "named",
                         types = "function",
                         add = coll)

  checkmate::assert_logical(x = warn_zero_coded,
                            len = 1,
                            any.missing = FALSE,
                            add = coll)

  add_args <- names(list(...))
  if("labels" %in% add_args ||
     "dates"  %in% add_args)
  {
    logWarning("The 'labels' and 'dates' flags passed to exportRecordsTyped are ignored.\nSee documentation for exportRecordsTyped 'cast' argument.\nAlternatively read the vignette(\"redcapAPI-best-practices\") for in depth explanation.")
  }
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
                                            use_original = TRUE,
                                            include_descriptive = FALSE)
{
  MetaData <- rcon$metadata()

  # exportFieldNames excludes fields of type calc, descriptive, and file
  # We need to wedge them in here or we'll never get them out of the API
  ProjectFields <- rcon$fieldnames()

  restore_types <- c("calc",
                     "file",
                     if (include_descriptive) "descriptive" else character(0))

  MissingFromFields <- MetaData[MetaData$field_type %in% restore_types, ]
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

  # For the forms, we cannot assume they are in forms. Instead, we initialize
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
  # Also sets the is_in_forms to FALSE to ensure it is not
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
                                           csv_delimiter,
                                           ...)
{
  response <- makeApiCall(rcon,
                          body = c(body,
                                   vectorToApiBodyList(records, "records")),
                          ...)

  response <- as.data.frame(response,
                            colClasses = "character",
                            sep = csv_delimiter)

  if (nrow(response) == 0) logMessage("No data found in the project.")

  response
}

# .exportRecordsTyped_Batched ---------------------------------------
.exportRecordsTyped_Batched <- function( rcon,
                                         body,
                                         records,
                                         csv_delimiter,
                                         batch_size,
                                         ...)
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
                                                                "fields")),
                                   ...)

    records <- as.data.frame(record_response, sep = csv_delimiter)

    if (nrow(records) == 0)
    {
      logMessage("No data found in the project.")
      return(data.frame())
    }

    records <- unique(records[[target_field]])
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
                                           csv_delimiter = csv_delimiter,
                                           ...)})

  # Combine the data
  Batched <- do.call("rbind", Batched)
  rownames(Batched) <- NULL
  Batched
}



#####################################################################
# exportBulkRecord                                               ####

#' @name exportBulkRecords
#' @title A helper function to export multiple records and forms using
#' a single call.
#' @description Exports records from multiple REDCap Databases using
#' multiple calls to [exportRecordsTyped()]
#'
#' @param lcon  A named list of connections. The name is used as a prefix for data.frame
#'              names in the environment specified. It may also be used as a reference from the
#'              forms argument.
#' @param forms A named list that is a subset of rcon's names. A specified `rcon`
#'              will provide a list of forms for repeated calls to `exportRecordsType`.
#'              If a connection reference is missing it will default to all forms. To override
#'              this default specify a connection's forms with NA to just get all
#'              data.
#' @param envir A environment to write the resulting Records in as variables
#'   given by their name in rcon or if from a form their rcon named pasted to
#'   their form name joined by `sep`. If not specified the function
#'   will return a named list with the results. Will accept a number of the
#'   environment.
#' @param sep A character string to use when joining the rcon name to the form name
#' for storing variables.
#' @param post A function that will run on all returned sets of Records.
#' @param \dots Any additional variables to pass to [exportRecordsTyped()].
#' @return Will return a named list of the resulting records if `envir` is
#'    NULL. Otherwise will assign them to the specified `envir`.
#'
#' @seealso
#' ## Other records exporting functions
#'
#' [exportRecordsTyped()], \cr
#' [exportRecords()], \cr
#' [exportReports()]
#'
#' ## Field validations and casting
#'
#' [fieldValidationAndCasting()], \cr
#' [reviewInvalidRecords()]
#'
#' ## Post-processing functionality
#'
#' [recastRecords()], \cr
#' [guessCast()], \cr
#' [guessDate()], \cr
#' [castForImport()], \cr
#' [mChoiceCast()], \cr
#' [splitForms()], \cr
#' [widerRepeated()]
#'
#' ## Vignettes
#'
#' `vignette("redcapAPI-offline-connection")`\cr
#' `vignette("redcapAPI-casting-data")`\cr
#' `vignette("redcapAPI-missing-data-detection")`\cr
#' `vignette("redcapAPI-data-validation)`\cr
#' `vignette("redcapAPI-faq)`
#'
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

exportBulkRecords <- function(lcon, forms=NULL, envir=NULL, sep="_", post=NULL, ...)
{
  if(is.numeric(envir)) envir <- as.environment(envir)

  coll <- checkmate::makeAssertCollection()

  checkmate::assert_list(     x       = lcon,
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
                              choices = names(lcon),
                              add     = coll)

    checkmate::assert_list( x       = forms,
                            types   = c("character"),
                            add     = coll)
  }

  checkmate::reportAssertions(coll)

  dest <- list()

  if(is.null(forms)) forms <- list()

  # For each dataset requested
  for(i in names(lcon))
  {
    conn  <- lcon[[i]]
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


#' @describeIn recordsMethods Export records from a project.
#' @order 1
#' @export

exportRecords <-
  function(rcon,
           factors = TRUE,
           fields = NULL,
           forms = NULL,
           records = NULL,
           events = NULL,
           labels = TRUE,
           dates = TRUE,
           drop = NULL,
           survey = TRUE,
           dag = TRUE,
           checkboxLabels = FALSE,
           colClasses = character(0),
           ...)

    UseMethod("exportRecords")

#' @rdname recordsMethods
#' @order 4
#' @export

exportRecords.redcapApiConnection <-
  function(rcon,
           factors            = TRUE,
           fields             = NULL,
           forms              = NULL,
           records            = NULL,
           events             = NULL,
           labels             = TRUE,
           dates              = TRUE,
           drop               = NULL,
           survey             = TRUE,
           dag                = TRUE,
           checkboxLabels     = FALSE,
           colClasses         = character(0),
           ...,
           batch.size         = -1,
           form_complete_auto = TRUE)
{
  logMessage("Please use exportRecordsTyped instead. exportRecords is will undergo breaking changes in version 3.0.0.")

  if (is.numeric(records)) records <- as.character(records)

   ##################################################################
  # Argument Validation

  coll <- checkmate::makeAssertCollection()

  checkmate::assert_class(x = rcon,
                          classes = "redcapApiConnection",
                          add = coll)

  checkmate::assert_logical(x = factors,
                            len = 1,
                            any.missing = FALSE,
                            add = coll)

  checkmate::assert_character(x = fields,
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

  checkmate::assert_logical(x = labels,
                            len = 1,
                            any.missing = FALSE,
                            add = coll)

  checkmate::assert_logical(x = dates,
                            len = 1,
                            any.missing = FALSE,
                            add = coll)

  checkmate::assert_character(x = drop,
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

  checkmate::assert_logical(x = checkboxLabels,
                            len = 1,
                            any.missing = FALSE,
                            add = coll)

  checkmate::assert_logical(x = form_complete_auto,
                            len = 1,
                            any.missing = FALSE,
                            add = coll)

  checkmate::assert_integerish(x = batch.size,
                               len = 1,
                               add = coll)

  if (is.list(colClasses)){
    colClasses <- unlist(colClasses)
  }

  checkmate::assert_character(x = colClasses,
                              names = "named",
                              add = coll)

  checkmate::reportAssertions(coll)

  MetaData <- rcon$metadata()

  #* for purposes of the export, we do not need the descriptive fields.
  #* Including them makes the process more error prone, so we'll ignore them.
  MetaData <- MetaData[!MetaData$field_type %in% "descriptive", ]

  #* Secure the events table
  events_list <- rcon$events()

  #* Secure the REDCap version
  version <- rcon$version()

  form_complete_fields <-
    sprintf("%s_complete",
            unique(MetaData$form_name))
  form_complete_fields <-
    form_complete_fields[!is.na(form_complete_fields)]

  #* Check that all fields exist in the meta data
  if (!is.null(fields))
  {
    bad_fields <- fields[!fields %in% c(MetaData$field_name,
                                        form_complete_fields)]
    if (length(bad_fields))
      coll$push(paste0("The following are not valid field names: ",
                       paste0(bad_fields, collapse = ", ")))
  }

  #* Check that all form names exist in the meta data
  if (!is.null(forms))
  {
    bad_forms <- forms[!forms %in% MetaData$form_name]
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

   ##################################################################
  # Create the vector of field names
  if (!is.null(fields)) #* fields were provided
  {
    # redcap_event_name is automatically included in longitudinal projects
    field_names <- fields[!fields %in% "redcap_event_name"]
  }
  else if (!is.null(forms))
  {
    field_names <- MetaData$field_name[MetaData$form_name %in% forms]
  }
  else
    #* fields were not provided, default to all fields.
    field_names <- MetaData$field_name

  #* Expand 'field_names' to include fields from specified forms.
  if (!is.null(forms))
    field_names <-
    unique(c(field_names,
             MetaData$field_name[MetaData$form_name %in% forms]))

   ##################################################################
  # Checkbox Suffixes

  suffixed <-
    checkbox_suffixes(
      # The subset prevents `[form]_complete` fields from
      # being included here.
      fields = field_names[field_names %in% MetaData$field_name],
      meta_data = MetaData)

   ##################################################################
  # Identify the forms from which the chosen fields are found
  included_form <-
    unique(
      MetaData$form_name[MetaData$field_name %in% field_names]
    )

   ##################################################################
  # Add the form_name_complete column to the export
  if (form_complete_auto){
    field_names <- c(field_names,
                     sprintf("%s_complete", included_form))
  }

   ##################################################################
  # Make API Body List
  body <- list(content = 'record',
               format = 'csv',
               type = 'flat',
               exportSurveyFields = tolower(survey),
               exportDataAccessGroups = tolower(dag),
               returnFormat = 'csv')

  body <- c(body,
            vectorToApiBodyList(field_names, "fields"),
            vectorToApiBodyList(forms, "forms"),
            vectorToApiBodyList(events, "events"),
            vectorToApiBodyList(records, "records"))

   ##################################################################
  # Call API
  if (batch.size < 1){
    Records <- unbatched(rcon = rcon,
                         body = body,
                         id = MetaData$field_name[1],
                         colClasses = colClasses,
                         ...)
  }
  else
  {
    Records <- batched(rcon = rcon,
                       body = body,
                       batch.size = batch.size,
                       id = MetaData$field_name[1],
                       colClasses = colClasses,
                       ...)
  }

  #* synchronize underscore codings between records and meta data
  #* Only affects calls in REDCap versions earlier than 5.5.21
  if (utils::compareVersion(version, "6.0.0") == -1)
    MetaData <- syncUnderscoreCodings(Records, MetaData)

  Records <- fieldToVar(records = Records,
                        meta_data = MetaData,
                        factors = factors,
                        dates = dates,
                        labels = labels,
                        checkboxLabels = checkboxLabels,
                        ...)

  if (labels){
    Records[,suffixed$name_suffix] <-
      mapply(nm = suffixed$name_suffix,
             lab = suffixed$label_suffix,
             FUN = function(nm, lab){
               if(is.null(Records[[nm]])){
                 logWarning("Missing field for suffix ", nm)
               } else {
                 labelVector::set_label(Records[[nm]], lab)
               }
             },
             SIMPLIFY = FALSE)
  }



  # drop
  if(length(drop)) {
    Records <- Records[!names(Records) %in% drop]
  } # end drop

  Records
}



#*** UNBATCHED EXPORT
unbatched <- function(rcon, body, id, colClasses, ...)
{
  colClasses[[id]] <- "character"
  colClasses <- colClasses[!vapply(colClasses,
                                   is.na,
                                   logical(1))]

  as.data.frame(
    makeApiCall(rcon, body, ...),
    colClasses=colClasses
  )
}


#*** BATCHED EXPORT
batched <- function(rcon, body, batch.size, id, colClasses, ...)
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
  id_body <- c(body[!grepl("^fields", names(body))],
               vectorToApiBodyList(id, "fields"))

  IDs <- makeApiCall(rcon,
                     body = body,
                     ...)

  IDs <- as.data.frame(IDs, colClasses = colClasses[id])

  #* 2. Restrict to unique IDs
  unique_id <- unique(IDs[[id]])

  #* 3. Determine if the IDs look hashed (de-identified)
  #* 4. Give warning about potential problems joining hashed IDs
  if (all(nchar(unique_id) == 32L))
  {
    logWarning("The record IDs in this project appear to be de-identified. ",
               "Subject data may not match across batches.")
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
    this_body <- c(body[!grepl("^records", names(body))],
                   vectorToApiBodyList(unique_id[batch.number == i], "records"))

    this_response <- makeApiCall(rcon, body, ...)
    batch_list[[i]] <- as.data.frame(this_response, colClasses = colClasses)

    Sys.sleep(1)
  }

  #* 6. Combine tables and return
  do.call("rbind", batch_list)
}

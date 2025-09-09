#' @name importRecords
#' @title Import Records to a Project
#'
#' @description These methods enable the user to import new records or update
#'   existing records to a project.
#'
#' @inheritParams common-rcon-arg
#' @inheritParams common-dot-args
#' @inheritParams common-api-args
#' @param data A `data.frame` to be imported to the project.
#' @param overwriteBehavior `character(1)`. One of `c("normal", "overwrite")`.
#'   `"normal"` prevents blank fields from overwriting populated fields.
#'   `"overwrite"` causes blanks to overwrite data in the database.
#' @param force_auto_number `logical(1)`. If record auto-numbering has been
#'   enabled in the project, it may be desirable to import records where each
#'   record's record name is automatically determined by REDCap (just as it
#'   does in the user interface). When `TRUE`, the
#'   record names provided in the request will not be used (although they
#'   are still required in order to associate multiple rows of data to an
#'   individual record in the request); instead those records in the
#'   request will receive new record names during the import process.
#'   It is recommended that the user use `returnContent = "auto_ids"`
#'   when `force_auto_number = TRUE`
#' @param returnContent `character(1)`.
#'   One of `c("count", "ids", "nothing", "auto_ids")`.
#'   'count' returns the number of records imported;
#'   'ids' returns the record ids that are imported;
#'   'nothing' returns no message;
#'   'auto_ids' returns a list of pairs of all record IDs that were imported.
#'   If used when `force_auto_number = FALSE`, the value will be changed to `'ids'`.
#' @param returnData `logical(1)`. When `TRUE`, prevents the REDCap
#'   import and instead returns the data frame that would have been given
#'   for import. This is sometimes helpful if the API import fails without
#'   providing an informative message. The data frame can be written to a csv
#'   and uploaded using the interactive tools to troubleshoot the
#'   problem.
#' @param logfile `character(1)`. An optional filepath (preferably .txt)
#'   in which to print the log of errors and warnings about the data.
#'   When `""`, the log is printed to the console.
#' @param batch.size `integerish(1)`.  Specifies the number of subjects to be included
#'   in each batch of a batched export or import.  Non-positive numbers
#'   export/import the entire operation in a single batch.
#'   Batching may be beneficial to prevent tying up smaller servers.
#'   See Details.
#'
#' @details
#' `importRecords` prevents the most common import errors by testing the
#' data before attempting the import.  Namely
#'
#' 1. Check that all variables in `data` exist in the REDCap data dictionary.
#' 2. Check that the record id variable exists
#' 3. Force the record id variable to the first position in the data frame (with a warning)
#' 4. Remove calculated fields (with a warning)
#' 5. Verify that REDCap date fields are represented in the data frame as either `character`, `POSIXct`, or `Date` class objects.
#' 6. Determine if values are within their specified validation limits.
#'
#' See the documentation for [validateImport()] for detailed
#' explanations of the validation.
#'
#' A 'batched' import is one where the export is performed over a series of
#' API calls rather than one large call.  For large projects on small servers,
#' this may prevent a single user from tying up the server and forcing others
#' to wait on a larger job.
#'
#' ## BioPortal Fields
#'
#' Text fields that are validation enabled using the BioPortal Ontology service
#' may be imported by providing the coded value. Importing the coded value
#' does not, however, guarantee that the labeled value will be immediately
#' available. Labels for BioPortal values are cached on the REDCap server
#' in a process that occurs when viewing data in the user interface. Thus,
#' if the label has not be previously cached on the server, the code will be
#' used to represent both the code and the label.
#'
#' @return
#' `importRecords`, when `returnData = FALSE`, returns the content from the
#'   API response designated by the `returnContent` argument.
#'
#' `importRecords`, when `returnData = TRUE`, returns the
#'   data frame that was internally prepared for import. This data frame has
#'   values transformed from R objects to character values the API will
#'   accept.
#'
#' @seealso
#' [exportRecords()], \cr
#' [deleteRecords()], \cr
#' [exportRecordsTyped()]
#'
#' @examples
#' \dontrun{
#' unlockREDCap(connections = c(rcon = "project_alias"),
#'              url = "your_redcap_url",
#'              keyring = "API_KEYs",
#'              envir = globalenv())
#'
#' # Import records
#' NewData <- data.frame(record_id = c(1, 2, 3),
#'                       age = c(27, 43, 32),
#'                       date_of_visit = rep(Sys.Date(), 3))
#' importRecords(rcon,
#'               data = NewData)
#'
#'
#' # Import records and save validation info to a file
#' NewData <- data.frame(record_id = c(1, 2, 3),
#'                       age = c(27, 43, 32),
#'                       date_of_visit = rep(Sys.Date(), 3))
#' importRecords(rcon,
#'               data = NewData,
#'               logfile = "import-validation-notes.txt")
#'
#' }
#'
#' @export

importRecords <- function(rcon,
                          data,
                          overwriteBehavior = c('normal', 'overwrite'),
                          returnContent     = c('count', 'ids', 'nothing', 'auto_ids'),
                          returnData        = FALSE,
                          logfile           = "",
                          ...){
  UseMethod("importRecords")
}

#' @rdname importRecords
#' @export

importRecords.redcapApiConnection <- function(rcon,
                                              data,
                                              overwriteBehavior = c('normal', 'overwrite'),
                                              returnContent     = c('count', 'ids', 'nothing', 'auto_ids'),
                                              returnData        = FALSE,
                                              logfile           = "",
                                              force_auto_number = FALSE,
                                              ...,
                                              batch.size        = -1)
{
  if(is.null(attr(data, "castForImport")))
    logMessage("importRecords will change how it validates data in version 3.0.0.\n",
            "We recommend preparing your data for import using castForImport .")

   ##################################################################
  # Argument Validation

  coll <- checkmate::makeAssertCollection()

  checkmate::assert_class(x = rcon,
                          classes = "redcapApiConnection",
                          add = coll)

  checkmate::assert_data_frame(x = data,
                               add = coll)

  overwriteBehavior <-
    checkmate::matchArg(x = overwriteBehavior,
                        choices = c('normal', 'overwrite'),
                        .var.name = "overwriteBehavior",
                        add = coll)

  returnContent <-
    checkmate::matchArg(x = returnContent,
                        choices = c('count', 'ids', 'nothing', 'auto_ids'),
                        .var.name = "returnContent",
                        add = coll)

  checkmate::assert_logical(x = returnData,
                            len = 1,
                            add = coll)

  checkmate::assert_character(x = logfile,
                              len = 1,
                              add = coll)

  checkmate::assert_logical(x = force_auto_number,
                            len = 1,
                            add = coll)

  checkmate::assert_integerish(x = batch.size,
                               len = 1,
                               add = coll)

  checkmate::reportAssertions(coll)

  MetaData <- rcon$metadata()

  version <- rcon$version()

  with_complete_fields <- rcon$fieldnames()$export_field_name

  # Remove survey identifiers and data access group fields from data
  w.remove <-
    which(names(data) %in%
            c("redcap_survey_identifier",
              paste0(unique(MetaData$form_name), "_timestamp")))
  if (length(w.remove) > 0) data <- data[-w.remove]

  mchoices <- which(vapply(data, inherits, logical(1), 'mChoice'))
  if(length(mchoices) > 0)
  {
    coll$push(paste0(
      "The variable(s) ",
      paste0(names(data)[mchoices], collapse=", "),
      " are mChoice formatted and cannot be imported."))
  }

  # Validate field names
  unrecognized_names <- !(names(data) %in% c(with_complete_fields, REDCAP_SYSTEM_FIELDS))
  if (any(unrecognized_names))
  {
    coll$push(paste0(
      "The variable(s) ",
      paste0(names(data)[unrecognized_names], collapse=", "),
      " are not found in the project and/or cannot be imported."))
  }

  # Check that the study id exists in data
  if (!MetaData$field_name[1] %in% names(data))
  {
    coll$push(paste0("The variable '",
                     MetaData$field_name[1],
                     "' cannot be found in 'data'. ",
                     "Please include this variable and place it in the first column."))
  }

  # If the study id is not in the the first column, move it and print a warning
  if (MetaData$field_name[1] %in% names(data) &&
      MetaData$field_name[1] != names(data)[1])
  {
    logMessage("The variable'", MetaData$field_name[1],
            "' was not in the first column. ",
            "It has been moved to the first column.")
    w <- which(names(data) == MetaData$field_name[1])
    data <- data[c(w, (1:length(data))[-w])]
  }

  # Confirm that date fields are either character, Date class, or POSIXct
  date_vars <- MetaData$field_name[grepl("date_", MetaData$text_validation_type_or_show_slider_number)]

  if (any(date_vars %in% names(data))){
    date_vars <- date_vars[date_vars %in% names(data)]
    bad_date_fmt <-
      !vapply(X = data[date_vars],
              FUN = function(x) is.character(x) | "Date" %in% class(x) | "POSIXct" %in% class(x),
              FUN.VALUE = logical(1))

    if (any(bad_date_fmt))
    {
      coll$push(paste0("The variables '",
                       paste(date_vars[bad_date_fmt], collapse="', '"),
                       "' must have class Date, POSIXct, or character."))
    }
  }

  # Remove calculated fields
  calc_field <- MetaData$field_name[MetaData$field_type == "calc"]
  calc_field <- calc_field[calc_field %in% names(data)]

  if (length(calc_field) > 0)
  {
    logMessage("The variable(s) '",
               paste(calc_field, collapse="', '"),
               "' are calculated fields and cannot be imported. ",
               "They have been removed from the imported data frame.")
    data <- data[!names(data) %in% calc_field]
  }

  checkmate::reportAssertions(coll)

  if (!force_auto_number && returnContent == 'auto_ids'){
    returnContent = 'ids'
  }


  idvars <-
    if ("redcap_event_name" %in% names(data))
      c(MetaData$field_name[1], "redcap_event_name")
  else
    MetaData$field_name[1]

  data <- validateImport(data = data,
                         meta_data = MetaData,
                         logfile = logfile)

  if (returnData) return(data)

  #** Format the data for REDCap import
  #** Thanks go to:
  #**   https://github.com/etb/my-R-code/blob/master/R-pull-and-push-from-and-to-REDCap.R
  #**   https://stackoverflow.com/questions/12393004/parsing-back-to-messy-api-strcuture/12435389#12435389

  if (batch.size > 0)
  {
    import_records_batched(rcon = rcon,
                           data = data,
                           batch.size = batch.size,
                           overwriteBehavior = overwriteBehavior,
                           returnContent = returnContent,
                           force_auto_number = force_auto_number,
                           ...)
  }
  else
  {
    import_records_unbatched(rcon = rcon,
                             data = data,
                             overwriteBehavior = overwriteBehavior,
                             returnContent = returnContent,
                             force_auto_number = force_auto_number,
                             ...)
  }
}

#####################################################################
## UNEXPORTED FUNCTIONS
#####################################################################

import_records_batched <- function(rcon,
                                   data,
                                   batch.size,
                                   overwriteBehavior,
                                   returnContent,
                                   force_auto_number,
                                   ...)
{
  n.batch <- nrow(data) %/% batch.size + 1

  ID <- data.frame(row = 1:nrow(data))

  ID$batch.number <- rep(1:n.batch,
                         each = batch.size,
                         length.out = nrow(data))

  data[is.na(data)] <- ""

  data <- split(data,
                f = ID$batch.number)

  out <- lapply(X = data,
                FUN = data_frame_to_string)

  att <- list("Content-Type" =
                structure(c("text/html", "utf-8"),
                          .Names = c("", "charset")))
  out <- lapply(X = out,
                FUN = function(d){
                  attributes(d) <- att;
                  return(d)
                })

   ##################################################################
  # Make API Body List

  body <- list(content = 'record',
               format = 'csv',
               type = 'flat',
               overwriteBehavior = overwriteBehavior,
               returnContent = returnContent,
               forceAutoNumber = tolower(force_auto_number),
               returnFormat = 'csv')

   ##################################################################
  # Call the API
  responses <- vector("list", length = length(out))

  allvalid <- TRUE
  for (i in seq_along(out))
  {
    responses[[i]] <-
      tryCatch(
        as.character(
          makeApiCall(
            rcon,
            body   = c(body, list(data = out[[i]])),
            ...)),
        error=function(e) { allvalid <<- FALSE; e }
      )
  }
  if(!allvalid) stop(paste(responses[nchar(responses) > 4], collapse="\n"))

  unlist(responses)
}


import_records_unbatched <- function(rcon,
                                     data,
                                     overwriteBehavior,
                                     returnContent,
                                     force_auto_number,
                                     ...)
{
  out <- data_frame_to_string(data)

  ## Reattach attributes
  attributes(out) <-
    list("Content-Type" = structure(c("text/html", "utf-8"),
                                    .Names = c("", "charset")))

   ##################################################################
  # Make API Body List

  body <- list(content = 'record',
               format = 'csv',
               type = 'flat',
               overwriteBehavior = overwriteBehavior,
               returnContent = returnContent,
               returnFormat = 'csv',
               forceAutoNumber = tolower(force_auto_number),
               data = out)

   ##################################################################
  # Call the API
  response <- makeApiCall(rcon, body, ...)

  if (returnContent %in% c("ids", "auto_ids"))
    as.data.frame(response) else
    as.character(response)
}

#####################################################################
# Unexported

data_frame_to_string <- function(data)
{
  paste0(
    utils::capture.output(
      utils::write.csv(data,
                       row.names = FALSE,
                       na = "")
    ),
    collapse = "\n"
  )
}

#' @name exportFilesMultiple
#' @title Export Multiple Files From a Project
#'
#' @description This method enables the user to export multiple files from
#'   a REDCap project with a single call. The REDCap API only allows for one
#'   file to be exported per call, and the [exportFiles()] methods are written
#'   to mirror that limitation. This extension allows the user to pass
#'   vectors of arguments for records, fields, events, or repeat instances.
#'   Files that can be matched to any combination of these values will
#'   be exported.
#'
#' @inheritParams common-rcon-arg
#' @inheritParams common-dot-args
#' @inheritParams common-api-args
#' @inheritParams fileMethods
#' @param record `character` or `integerish`. The record ID in
#'   which the desired file is stored.
#' @param field `character`. The field name in which the file is stored.
#' @param event `character` or `NULL`. The event name for the file.
#'   This applies only to longitudinal projects.  If the event is not
#'   supplied for a longitudinal project, the API will return an error message
#' @param repeat_instance `integerish` or `NULL`. The repeat instance number of
#'   the repeating event or the repeating instrument. When available in your
#'   instance of REDCap, and passed as `NULL`, the API will assume a value of 1.
#' @param quiet `logical(1)`. When `TRUE`, any errors encountered while
#'   exporting files will be converted to messages.
#'
#' @details `exportFilesMultiple` will construct all combinations of
#'   the `record`, `field`, `event`, and `repeat_instance` arguments and
#'   attempt to export the file associated with each combination. Should any
#'   of these combinations produce an error (for example, if a record does not
#'   have a third repeat instance), the error is captured and returned
#'   with the output.
#'
#' @return Invisibly returns a `data.frame` with the following columns:
#'
#' | Column | Description |
#' |--------|-------------|
#' | `record` | The record ID |
#' | `field`  | The name of the field in which the file is stored. |
#' | `event`  | The name of the event associated with the file. |
#' | `repeat_instance` | For repeat instances, the instance associated with the file. |
#' | `is_exported` | `logical` indicating if the file was successfully exported. |
#' | `saved_to` | The file path to which the file was saved. |
#' | `error`  | If an error was encountered, the text of the error. |
#'
#' @seealso
#' [exportFiles()]
#'
#' @examples
#' \dontrun{
#' unlockREDCap(connections = c(rcon = "project_alias"),
#'              url = "your_redcap_url",
#'              keyring = "API_KEYs",
#'              envir = globalenv())
#'
#' save_to_dir <- tempdir()
#'
#' # Export files for multiple records
#' # Results are returned invisibly - saving to an object is
#' #   helpful to be able to view the results
#'
#' Export <-
#'   exportFilesMultiple(rcon,
#'                       record = 1:4,
#'                       field = "file_upload_field",
#'                       event = "event_1_arm_1",
#'                       dir = save_to_dir)
#' Export
#'
#' # Export files for multiple instances
#'
#' Export <-
#'   exportFilesMultiple(rcon,
#'                       record = 1,
#'                       field = "file_upload_field",
#'                       event = "event_1_arm_1",
#'                       repeat_instance = 1:4,
#'                       dir = save_to_dir)
#' Export
#'
#' # Export files for multiple records, fields, events, and instances
#'
#' Export <-
#'   exportFilesMultiple(rcon,
#'                       record = 1:10,
#'                       field = c("registration", "waiver"),
#'                       events = c("event_1_arm_1", "event_2_arm_1"),
#'                       repeat_instance = 1:3,
#'                       dir = save_to_dir)
#' Export
#' }
#'
#'
#' @export

exportFilesMultiple <- function(rcon,
                                record,
                                field,
                                event       = NULL,
                                dir,
                                file_prefix = TRUE,
                                ...)
{
  UseMethod("exportFilesMultiple")
}

#' @rdname exportFilesMultiple
#' @export

exportFilesMultiple.redcapApiConnection <- function(rcon,
                                                    record,
                                                    field,
                                                    event           = NULL,
                                                    dir,
                                                    file_prefix     = TRUE,
                                                    repeat_instance = NULL,
                                                    ...,
                                                    quiet = TRUE)
{
  if (is.numeric(record)) record <- as.character(record)

  ###################################################################
  # Argument Validation                                          ####

  coll <- checkmate::makeAssertCollection()

  checkmate::assert_class(x = rcon,
                          classes = "redcapApiConnection",
                          add = coll)

  checkmate::assert_character(x = record,
                              any.missing = FALSE,
                              add = coll)

  checkmate::assert_character(x = field,
                              any.missing = FALSE,
                              add = coll)

  checkmate::assert_character(x = event,
                              any.missing = FALSE,
                              null.ok = TRUE,
                              add = coll)

  checkmate::assert_character(x = dir,
                              len = 1,
                              any.missing = FALSE,
                              add = coll)

  checkmate::assert_logical(x = file_prefix,
                            len = 1,
                            add = coll)

  checkmate::assert_integerish(x = repeat_instance,
                               any.missing = FALSE,
                               null.ok = TRUE,
                               add = coll)

  checkmate::assert_logical(x = quiet,
                            len = 1,
                            add = coll)

  checkmate::reportAssertions(coll)

  ###################################################################
  # Functionality                                                ####

  # Data frame to store the results
  args <- list(record=record, field=field, stringsAsFactors=FALSE)
  if(!is.null(event)) args[['event']] <- event
  if(!is.null(repeat_instance)) args[['repeat_instance']] <- repeat_instance
  ResultsFrame <- do.call(expand.grid, args)

  ResultsFrame$is_exported <- rep(FALSE, nrow(ResultsFrame))
  ResultsFrame$saved_to <- rep(NA_character_, nrow(ResultsFrame))
  ResultsFrame$error <- rep(NA_character_, nrow(ResultsFrame))

  # Export the Files

  for (i in seq_len(nrow(ResultsFrame))){
    this_attempt <-
      .exportFilesMultiple_attemptExport(
        rcon = rcon,
        record = ResultsFrame$record[i],
        field = ResultsFrame$field[i],
        event = ResultsFrame$event[i],
        repeat_instance = ResultsFrame$repeat_instance[i],
        dir = dir,
        file_prefix = file_prefix,
        ...
      )

    if (this_attempt$is_ok){
      ResultsFrame$is_exported[i] <- TRUE
      ResultsFrame$saved_to[i] <- this_attempt$output
    } else {
      ResultsFrame$is_exported[i] <- FALSE
      ResultsFrame$error[i] <- this_attempt$output
      if (!quiet){
        logMessage(this_attempt$output)
      }
    }
  }

  invisible(ResultsFrame)
}

#####################################################################
# Unexported                                                     ####

.exportFilesMultiple_attemptExport <- function(...){
  is_ok <- TRUE

  output <-
    tryCatch(exportFiles(...),
             error = function(cond){
               is_ok <<- FALSE
               cond$message
             })
  list(is_ok = is_ok,
       output = output)
}

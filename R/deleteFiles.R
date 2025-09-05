#' @describeIn fileMethods Delete a file from a REDCap project.
#' @order 3
#' @export

deleteFiles <- function(rcon,
                        record,
                        field,
                        event, ...){
  UseMethod("deleteFiles")
}

#' @rdname fileMethods
#' @order 6
#' @export

deleteFiles.redcapApiConnection <- function(rcon,
                                            record          = NULL,
                                            field           = NULL,
                                            event           = NULL,
                                            repeat_instance = NULL,
                                            ...)
{
  if (is.numeric(record)) record <- as.character(record)

  ###########################################################################
  # Check parameters passed to function

  coll <- checkmate::makeAssertCollection()

  checkmate::assert_class(x = rcon,
                          classes = "redcapApiConnection",
                          add = coll)

  checkmate::assert_character(x = record,
                              len = 1,
                              any.missing = FALSE,
                              add = coll)

  checkmate::assert_character(x = field,
                              len = 1,
                              any.missing = FALSE,
                              add = coll)

  checkmate::assert_character(x = event,
                              len = 1,
                              any.missing = FALSE,
                              null.ok = TRUE,
                              add = coll)

  checkmate::assert_integerish(x = repeat_instance,
                               len = 1,
                               any.missing = FALSE,
                               null.ok = TRUE,
                               add = coll)

  checkmate::reportAssertions(coll)

  # make sure 'field' exist in the project and are 'file' fields
  MetaData <- rcon$metadata()

  if (!field %in% MetaData$field_name)
    coll$push(paste0("'", field, "' does not exist in the project."))

  if (MetaData$field_type[MetaData$field_name == field] != "file")
    coll$push(paste0("'", field, "' is not of field type 'file'"))

  # make sure 'event' exists in the project

  is_project_longitudinal <- as.logical(rcon$projectInformation()$is_longitudinal)

  if (is_project_longitudinal)
  {
    EventsList <- rcon$events()

    if (nrow(EventsList) == 0)
    {
      logMessage("No events defined in this project. Ignoring the 'event' argument.")
      event <- NULL
    } else {
      checkmate::assert_subset(x = event,
                               choices = EventsList$unique_event_name,
                               add = coll)
      checkmate::reportAssertions(coll)
    }
  } else {
    event <- NULL
  }

  checkmate::reportAssertions(coll)

   ###########################################################################
  # Build the body list
  body <- list(content = 'file',
               action = 'delete',
               record = record,
               field = field,
               returnFormat = 'csv',
               event = event,
               repeat_instance = repeat_instance)

  ###########################################################################
  # Make the API Call
  makeApiCall(rcon, body, ...)
  invisible(TRUE)
}

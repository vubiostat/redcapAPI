#' @name reviewInvalidRecords
#' @title Review Invalid Records Following Field Validation
#'
#' @description This function retrieves a summary of data elements that
#'   failed validation during field validation and casting.
#'
#' @param data `data.frame`. The result of a function that performed
#'   field validation.
#' @param quiet `logical(1)`. When `TRUE`, a message will be printed
#'   if the `invalid` attribute is not found on `data`. Otherwise,
#'   the message is suppressed.
#'
#' @details
#'   When discussing field validation and invalid data, it is helpful to
#'   establish the following terminology:
#'
#'   A _Records data frame_ is a data frame returned by a function
#'   where the fields (columns) in the data frame have been cast for
#'   subsequent analysis.
#'
#'   Some casting function also perform field validation and return an
#'   _Invalid data frame_, which is a listing of data elements that
#'   have failed validation. The Invalid data frame is attached as an
#'   attribute to the Records data frame. If no data elements fail the
#'   validation, the Invalid data frame will have zero rows. If at least
#'   one data element fails validation, a warning is printed to notify
#'   the user so that the user may review the Invalid data frame and
#'   mitigate the failed validations.
#'
#'   The Invalid data frame has an additional class (`c("invalid", "data.frame")`)
#'   and comes with a print method. The `print.invalid` method displays the
#'   content of the Invalid data frame neatly in both the console
#'   and within reports utilizing markdown.
#'
#' @return
#' If `data` has the `"invalid"` attribute, an object with class
#' `c("invalid", "data.frame")` is returned. (`NULL` will be returned if
#' `data` does not have the attribute).
#'
#' The colums in the Invalid data frame are
#' |              |                                                                                               |
#' |--------------|-----------------------------------------------------------------------------------------------|
#' | `row`        | The row number from the Records data frame for which validation failed.                       |
#' | `record_id`  | The record ID for the failed validation.                                                      |
#' | `field_name` | The field name (column) of the failed validation.                                             |
#' | `field_type` | The field type of the failed validation.                                                      |
#' | `value`      | The original value that failed validation. It will be replaced with `NA` in the Records data. |
#'
#' The Invalid data frame has additional attributes
#'
#' * `time` - The date/time at which the validation was performed.
#' * `version` - The REDCap version number (as retrieved by `exportVersion`).
#' * `project` - The title of the REDCap project (as retrieved by `exportProjectInformation`).
#'
#' @seealso
#' [exportRecordsTyped()], \cr
#' [exportReportsTyped()], \cr
#' [castForImport()], \cr
#' [guessCast()]
#'
#' @examples
#' \dontrun{
#' unlockREDCap(connections = c(rcon = "project_alias"),
#'              url = "your_redcap_url",
#'              keyring = "API_KEYs",
#'              envir = globalenv())
#'
#' # Review the Invalid data frame after export
#' Records <- exportRecordsTyped(rcon)
#' reviewInvalidRecords(Records)
#'
#'
#'
#' # Review Invalid data frame before import
#' Records <- castForImport(rcon)
#' reviewInvalidRecords(Records)
#'
#'
#'
#' # Access the Invalid data frame the attributes
#' Records <- exportRecordsTyped(rcon)
#' attr(Records, "invalid")
#' attributes(Records)$invalid
#' }
#'
#' @export

reviewInvalidRecords <- function(data, quiet = TRUE){
  coll <- checkmate::makeAssertCollection()

  checkmate::assert_data_frame(x = data,
                               add = coll)

  checkmate::assert_logical(x = quiet,
                            len = 1,
                            any.missing = FALSE,
                            add = coll)

  checkmate::reportAssertions(coll)

  Invalid <- attr(data, "invalid")

  if (is.null(Invalid) && !quiet){
    logMessage(sprintf("`%s` does not have an 'invalid' attribute.",
                       as.character(substitute(data))))
  }
  Invalid
}


#' @name widerRepeated
#' @title Transform Data Into Wide Format
#'
#' @description Converts a dataframe into wide format given a single REDCap form.
#' This function assumes that the Records argument is the result of `exportRecordsTyped`,
#' and that all empty values have been previously dropped. This will only widen data frames
#' that have a unique identification variable (e.g. 'record_id'), "redcap_event_name" and
#' "redcap_repeat_instrument" in the fields. Otherwise, the data passed in will be returned
#' unchanged.
#'
#' @inheritParams common-rcon-arg
#' @param Records `data.frame` containing the records from [exportRecordsTyped()]
#'
#' @seealso
#' ## Other post-processing functions
#'
#' [recastRecords()], \cr
#' [guessCast()], \cr
#' [guessDate()], \cr
#' [castForImport()], \cr
#' [mChoiceCast()], \cr
#' [splitForms()]
#'
#'
#' @examples
#' \dontrun{
#' unlockREDCap(connections = c(rcon = "project_alias"),
#'              url = "your_redcap_url",
#'              keyring = "API_KEYs",
#'              envir = globalenv())
#'
#' Records <- exportRecordsTyped(rcon)
#'
#' widerRepeated(Records, rcon)
#' }
#'
#' @export

widerRepeated <- function(Records, rcon)
{
  idvar <- getProjectIdFields(rcon)
  ###########################################################################
  # Check parameters passed to function
  coll <- checkmate::makeAssertCollection()

  checkmate::assert_data_frame(x = Records,
                               add = coll)
  checkmate::assert_class(x = rcon,
                          classes = "redcapConnection",
                          add = coll)
  checkmate::reportAssertions(coll)

  form_ids <- c(idvar, "redcap_data_access_group",
                "redcap_event_name", "redcap_repeat_instrument")
  form_ids <- form_ids[form_ids %in% names(Records)]

  # check that redcap_repeat_instrument is present and that all values are the same
  if ("redcap_repeat_instrument" %in% colnames(Records) && all(!is.na(Records$redcap_repeat_instrument))) {
    if (length(unique(Records$redcap_repeat_instrument)) == 1) {
      # preserve column attributes
      Records <- data.frame(Records, check.names = FALSE)
      RecordsWide <- stats::reshape(
        data = Records,
        idvar = form_ids,
        timevar = "redcap_repeat_instance",
        direction = "wide"
      )

      if (!is.null(attr(Records, "invalid"))){
        attr(RecordsWide, "invalid") <- attr(Records, "invalid")
        attr(RecordsWide, "invalid")$row <- NA
      }

      for(rw in names(RecordsWide)){
        if(rw %in% form_ids){
          attributes(RecordsWide[[rw]]) <- attributes(Records[[rw]])
        } else {
          r <- sub("^(.*)[.].*", "\\1", rw) # Remove text from last period and after.
          attributes(RecordsWide[[rw]]) <-  attributes(Records[[r]])
        }
      }

      return(RecordsWide)

    } else {
      logStop("The redcap_repeat_instrument column does not have unique values.\n")
    }
  } else {
    return(Records)
  }
}


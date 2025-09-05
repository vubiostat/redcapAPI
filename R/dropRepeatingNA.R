#' @name dropRepeatingNA
#' @title Drop Row Where Repeat Instrument Is NA
#'
#' @description Drops rows where the repeat instrument is NA. Returns a data frame of records where repeat instruments have a value.
#'
#' @inheritParams common-rcon-arg
#' @param Records A `data.frame` containing the records from [exportRecordsTyped()]
#' @param quiet `logical(1)`. When `FALSE`, a message is printed
#'   indicating how many rows were in `Records` at the start and
#'   completion of the subset.
#'
#' @seealso
#' [exportRecordsTyped()], \cr
#' [exportReportsTyped()]
#'

dropRepeatingNA <- function(Records,
                            rcon,
                            quiet = FALSE)
{

  coll <- checkmate::makeAssertCollection()

  checkmate::assert_data_frame(x = Records,
                               add = coll)

  checkmate::assert_class(x = rcon,
                          classes = "redcapConnection",
                          add = coll)

  checkmate::assert_logical(x = quiet,
                            len = 1,
                            null.ok = FALSE,
                            any.missing = FALSE,
                            add = coll)

  checkmate::reportAssertions(coll)

  if('redcap_repeat_instrument' %in% names(Records))
  {
    x <- Records$redcap_repeat_instrument

    if(any(is.na(x)))
    {
      if(!quiet)
        logMessage(
          paste('Project', rcon$projectInformation()$project_title,
                'had', length(x), 'rows, subsetted to',
                sum(! is.na(x)), 'rows'))

      return(Records[! is.na(x),])
    }
  }

  Records
}

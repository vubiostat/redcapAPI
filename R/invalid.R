#' @rdname invalidSummary
#' @export
#' 
#' @title Helper functions for formatting validation failure report
#' @description \code{\link{exportRecordsTyped}} may have an \code{invalid}
#' attribute if validations fail. This data has some routines which
#' help locate the failing records.
#'
#' @param x The `invalid` object
#' @param ... additional arguments to `print`
#' 
#' #' @examples
#' \dontrun{
#' rcon <- redcapConnection(url=[YOUR_REDCAP_URL], token=[API_TOKEN])
#' 
#' rec <- exportRecordsTyped(rcon)
#' 
#' attr(rec, "invalid")
#' }
format.invalid <- function(x, ...)
{
  dt <- attr(x, "time")
  vr <- attr(x, "version")
  ft <- attr(x, "field_types")
  pt <- attr(x, "project")
  x <- split(x, x$field_name)
  paste0(
    "# Failed Validations from REDCap project '",  pt, "'\n\n",
    paste0(dt, "  \n"),
    paste0("Package redcapAPI version ", packageVersion("redcapAPI"), "  \n"),
    paste0("REDCap version ", vr, "  \n\n"),
    paste0(unlist(sapply(seq_along(x), function(i) {
      data <- x[[i]]
      c(paste0("* Field[",ft[i], "] '", names(x)[i], "' has ", nrow(data), " failure", ifelse(nrow(data) > 1, "s", "")),
        if(!is.na(data$record_id[1])) 
           paste0("  * Row ", data$row, ", Record Id '", data$record_id, "', Value '", data$value, "'") else
           paste0("  * Row ", data$row, ", Value '", data$value, "'")
      )
    })), collapse="\n")
  )
}

#' @rdname invalidSummary
#' @export
print.invalid <- function(x, ...)
{
  cat(format(x), ...)
}

#' @rdname invalidSummary
#' @export
summary.invalid <- function(x, ...)
{
  paste0("There are ", nrow(x), " validation failures over the following fields: '",
    paste0(unique(x$field_name), collapse="', '"),
    "'."
  , collapse="")
}
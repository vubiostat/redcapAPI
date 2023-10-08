#' @name invalidSummary
#' @title Helper functions for formatting validation failure report
#' 
#' @description [exportRecordsTyped()] may have an `invalid`
#' attribute if validations fail. This data has some routines which
#' help locate the failing records.
#'
#' @param x The `invalid` class object.
#' @param object The `invalid` class object.
#' @param ... additional arguments to `print`
#' 
#' @examples
#' \dontrun{
#' rcon <- redcapConnection(url=[YOUR_REDCAP_URL], token=[API_TOKEN])
#' 
#' rec <- exportRecordsTyped(rcon)
#' 
#' attr(rec, "invalid")
#' }
#' 
#' @export
format.invalid <- function(x, ...)
{
  dt <- attr(x, "time")
  vr <- attr(x, "version")
  pt <- attr(x, "project")
  
  if(is.null(vr)) vr <- "offline"
  if(is.null(pt)) pt <- "offline"
  
  x$link_to_redcap <- ifelse(is.na(x$link_to_form), 
                           "(link unavailable)", 
                           sprintf("[link](%s)", x$link_to_form))

  x <- split(x, x$field_name)
  
  paste0(
    "# Failed Validations from REDCap project '",  pt, "'\n\n",
    paste0(dt, "  \n"),
    paste0("Package redcapAPI version ", utils::packageVersion("redcapAPI"), "  \n"),
    paste0("REDCap version ", vr, "  \n\n"),
    paste0(unlist(sapply(seq_along(x), function(i) {
      data <- x[[i]]
      c(paste0("* Field[",data$field_type[1], "] '", names(x)[i], "' on form '", data$form_name[1], "' has ", nrow(data), " failure", ifelse(nrow(data) > 1, "s", "")),
        if("record_id" %in% names(data) && !is.na(data$record_id[1])) 
           paste0("  * Row ", data$row, ", Record Id '", data$record_id, "', Value '", data$value, "' ", data$link_to_redcap) else
           paste0("  * Row ", data$row, ", Value '", data$value, "'")
      )
    })), collapse="\n"),
    collapse=''
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
summary.invalid <- function(object, ...)
{
  paste0("There are ", nrow(object), " validation failures over the following fields: '",
    paste0(unique(object$field_name), collapse="', '"),
    "'."
  , collapse="")
}

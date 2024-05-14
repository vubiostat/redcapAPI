#' @name redcapError
#' @title Handle Errors from the REDCap API
#' 
#' @description Determine the proper way to handle errors returned from the API.
#'   Not all errors should be fatal.  See Details for more
#'   
#' @param x Object returned by [httr::POST()].
#' @inheritParams common-api-args
#' 
#' @details Maintaining consistent functionality for all types of REDCap projects 
#'   requires that errors be handled delicately.  It is not always desirable for an
#'   error from the API to terminate the program.  One example of such a case is when
#'   a user executes the `exportEvents` function for a classic project; 
#'   doing so returns an error from the API that events cannot be exported for
#'   classic projects.  In REDCap versions earlier than 6.5.0, there is no way to
#'   determine if a project is classic or longitudinal without attempting to export
#'   the events.  
#'   
#'   For this reason, it is often preferable to have these kinds of errors return 
#'   `NULL` so that the program does not crash if it does not need to (one such 
#'   instance where it does not need to crash is when `exportEvents` is called
#'   within `exportRecords`; the `events` argument is irrelevant to a 
#'   classic project and the error can safely be ignored.
#'   
#'   The other common type of error that does not need to be fatal is when a 
#'   `redcapAPI` method is sent to a REDCap instance that does not support the 
#'   method.  For example, the `exportVersion` method is not supported in 
#'   REDCap instances earlier than 6.0.0.  In these cases, we may prefer not to cast
#'   a hard error.
#'   
#'   These two types of errors may be handled in one of two ways.  When the 
#'   error handler is set to `"null"`, a `NULL` is returned.  When the 
#'   error handler is set to `"error"`, the error is returned.  The option 
#'   is set globally using `options(redcap_error_handler = "null")` and is
#'   set to `"null"` by default.
#'   
#' 

redcapError <- function(x, error_handling=getOption("redcap_error_handling"))
{
  error_message <- as.character(x)
  
  handle <- c("ERROR: The value of the parameter \"content\" is not valid",
              "ERROR: You cannot export arms for classic projects",
              "ERROR: You cannot export events for classic projects",
              "ERROR: You cannot export form/event mappings for classic projects")
  if (error_message %in% handle && error_handling == "null") {
    return(NULL)
  } else if (grepl("ERROR[:] The File Repository folder folder_id[=]\\d+ does not exist or else", error_message)){
    return(FILE_REPOSITORY_EMPTY_FRAME)
  } else if (grepl("Connection reset by peer", error_message) || 
             grepl("Timeout was reached", error_message)){
    stop(paste0(.RESET_BY_PEER, ": ", as.character(x)))
  } else{ 
    stop(paste0(x$status_code, ": ", as.character(x)))
  }
}

.RESET_BY_PEER <- "A network error has occurred. This can happen when too much data is requested causing a timeout (consider batching), or when REDCap is having trouble servicing requests. It may also be a misconfigured proxy or network routing congestion."

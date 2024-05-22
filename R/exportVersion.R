#' @name exportVersion
#'
#' @title Export the REDCap Version Number
#' @description These methods enable the user to export the REDCap instance
#'   version number.
#'
#' @inheritParams common-rcon-arg
#' @inheritParams common-dot-args
#' @inheritParams common-api-args
#'
#' @return 
#' Returns a character value giving the version number. 
#' 
#' IF this function is used in a version of REDCap that does not support 
#' the method (prior to version 6.0.0), the value "5.12.2" will be returned. 
#' This is done solely for the convenience of always returning a value
#' that can be compared against other versions.
#' 
#' @examples
#' \dontrun{
#' unlockREDCap(connections = c(rcon = "project_alias"), 
#'              url = "your_redcap_url", 
#'              keyring = "API_KEYs", 
#'              envir = globalenv())
#'              
#' # Export the version number
#' exportVersion(rcon)
#' }
#' 
#' @export


exportVersion <- function(rcon, ...){
  UseMethod("exportVersion")
}

#' @rdname exportVersion
#' @export

exportVersion.redcapApiConnection <- function(rcon, 
                                              ...)
{
   ##################################################################
  # Argument Validation 
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon,
                          classes = "redcapApiConnection",
                          add = coll)

  checkmate::reportAssertions(coll)
  
   ##################################################################
  # Make the API Body List
  
  body <- list(content='version')

   ##################################################################
  # Call the API
  response <- makeApiCall(rcon, body, ...)

  if (response$status_code != 200  &&
      is.null(redcapError(response)))
    return("5.12.2")

  as.character(response)
}

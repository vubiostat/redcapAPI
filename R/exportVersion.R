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
                                              ...,
                                              error_handling = getOption("redcap_error_handling"), 
                                              config         = list(), 
                                              api_param      = list()){
  
   ##################################################################
  # Argument Validation 
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon,
                          classes = "redcapApiConnection",
                          add = coll)
  
  error_handling <- checkmate::matchArg(x = error_handling,
                                        choices = c("null", "error"),
                                        .var.name = "error_handling",
                                        add = coll)
  
  checkmate::assert_list(x = config, 
                         names = "named", 
                         add = coll)
  
  checkmate::assert_list(x = api_param, 
                         names = "named", 
                         add = coll)
  
  checkmate::reportAssertions(coll)
  
   ##################################################################
  # Make the API Body List
  
  body <- list(token=rcon$token, 
               content='version')
  
  body <- body[lengths(body) > 0]
  
   ##################################################################
  # Call the API
  
  response <- makeApiCall(rcon, 
                          body = c(body, api_param), 
                          config = config)
  
  if (response$status_code != 200) 
  {
    handled <- redcapError(response, error_handling)
    #* If the export version API method is not supported by the REDCap instance,
    #* return "5.12.2".  For convenience, we will treat all pre 6.0.0 
    #* versions the same.  The only inefficiency this will generate is 
    #* in choosing when to run `syncUnderscoreCodings`.
    if (is.null(handled)) return("5.12.2")
  }

  as.character(response)
}

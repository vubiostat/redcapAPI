#' @name makeApiCall
#' @title Make REDCap API Calls
#' 
#' @description Constructs and executes API calls to the REDCap API. These
#'   are left deliberately abstract in order to be flexible enough to 
#'   support the `redcapAPI` functions, but also allow users to 
#'   execute calls for new REDCap features that are not yet implemented.
#'   
#' @inheritParams common-rcon-arg
#' @param body `list` List of parameters to be passed to [curl::]'s 
#'   `body` argument
#' @param url `character(1)` A url string to hit. Defaults to rcon$url.
#' @param success_status_codes `integerish` A vector of success codes to ignore
#'   for error handling. Defaults to c(200L).
#' @param ... This will capture `api_param` (if specified) which will modify the body of the 
#'   the specified body of the request. It also captures `config` which will get
#'   passed to curl::handle_setopt.
#' @details The intent of this function is to provide an approach to execute
#'   calls to the REDCap API that is both consistent and flexible. Importantly, 
#'   this provides a framework for making calls to the API using features that
#'   the R package does not yet support (redcapAPI will always lag behind when 
#'   REDCap adds new features). 
#'   
#'   The API call consists of two components: the "body" and the "config." 
#'   The body of the call contains all of the arguments being passed to the 
#'   API. When building body components, be sure to review the documentation. 
#'   options to the API that require an array need to be built using 
#'   `vectorToApiBodyList`; options that are not an array can be entered
#'   directly (see examples). 
#'   
#'   The config list is a list of parameter overrides that reflect the curl
#'   request object. The most commonly used elements of this list 
#'   is `options` or maybe `headers`. 
#'   
#'   Using the settings stored in the `redcapConnection` object, a response
#'   code of 408 (Request Timeout), 500 (Internal Server Error), 
#'   502 (Bad Gateway), 503 (Service Unavailable), or 504 (Gateway Timeout)
#'   will prompt reattempts at calling the API. See [redcapConnection()]
#'   for details. If the API reaches its attempt limit without resolving to 
#'   any other code, the last response is returned. If any other response
#'   code is returned at any point in the retry loop, the loop breaks and 
#'   returns that response.
#'   
#' @examples 
#' \dontrun{
#'   url <- "Enter your API URL here"
#'   token <- "Enter your API token here"
#'   
#'   rcon <- redcapConnection(url = url, 
#'                            token = token)
#'                            
#'   MetaData <- 
#'     makeApiCall(rcon = rcon,
#'                body = list(content = "metadata",
#'                            format = "csv",
#'                            returnFormat = "csv"))
#'   MetaData <- utils::read.csv(text = as.character(MetaData),
#'                               stringsAsFactors = FALSE,
#'                               na.strings = "")
#' 
#' 
#' 
#'   # Call to export Meta Data (Data Dictionary) for specific fields
#' 
#'   fields <- vectorToApiBodyList(vector = c("row_purpose", 
#'                                            "prereq_radio"),
#'                                 parameter_name = "fields")
#'   MetaData <-
#'     makeApiCall(rcon = rcon,
#'                 body = c(list(content = "metadata",
#'                               format = "csv",
#'                               returnFormat = "csv"),
#'                          fields))
#'   MetaData <- read.csv(text = as.character(MetaData),
#'                        stringsAsFactors = FALSE,
#'                        na.strings = "")
#' 
#' 
#' 
#'   # Basic call to export records
#' 
#'   Records <- makeApiCall(rcon = rcon,
#'                          body = list(content = "record",
#'                                      format = "csv",
#'                                      returnFormat = "csv",
#'                                      type = "flat"))
#' 
#'   Records <- read.csv(text = as.character(Records),
#'                       stringsAsFactors = FALSE,
#'                       na.strings = "")
#' 
#' 
#'   # Call to export records for a single form.
#'   # Note that even though we are interested in a single form, the
#'   # API requires an array, so we use vectorToApiBodyList
#' 
#'   export_form <- vectorToApiBodyList("branching_logic",
#'                                      parameter_name = "forms")
#'   Records <- makeApiCall(rcon = rcon,
#'                          body = c(list(content = "record",
#'                                        format = "csv",
#'                                        returnFormat = "csv",
#'                                        type = "flat"),
#'                                   export_form))
#'   Records <- read.csv(text = as.character(Records),
#'                       stringsAsFactors = FALSE,
#'                       na.strings = "")
#' 
#' 
#'   # Call to export records with a pipe delimiter.
#' 
#'   Records <- makeApiCall(rcon = rcon,
#'                          body = list(content = "record",
#'                                      format = "csv",
#'                                      returnFormat = "csv",
#'                                      type = "flat",
#'                                      csvDelimiter = "|"))
#'   Records <- read.csv(text = as.character(Records),
#'                       stringsAsFactors = FALSE,
#'                       na.strings = "",
#'                       sep = "|")
#' 
#' 
#'   # Call to export records created/modified after 25 Dec 2022 14:00.
#' 
#'   Records <- makeApiCall(rcon = rcon,
#'                          body = list(content = "record",
#'                                      format = "csv",
#'                                      returnFormat = "csv",
#'                                      type = "flat",
#'                                      dateRangeBegin = "2022-12-25 14:00:00"))
#' 
#'   Records <- read.csv(text = as.character(Records),
#'                       stringsAsFactors = FALSE,
#'                       na.strings = "")
#'                       
#'  
#' }
#' 
#' @export
makeApiCall <- function(rcon, 
                        body   = list(), 
                        url    = NULL,
                        success_status_codes = 200L,
                        ...)
{
  # Pull config, api_param from ...
  dots      <- list(...)
  
  api_param <- if("api_param" %in% names(dots)) dots$api_param else list()
  config    <- if("config"    %in% names(dots)) dots$config    else list()
  
  # Argument Validation ---------------------------------------------
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::assert_list(x = body, 
                         names = "named",
                         add = coll)

  checkmate::assert_character(x = url,
                              null.ok = TRUE,
                              len = 1,
                              add = coll)
  
  checkmate::assert_integerish(x = success_status_codes,
                               add = coll)
    
  checkmate::assert_list(x = config, 
                         names = "named", 
                         add = coll)
  
  checkmate::assert_list(x = api_param, 
                         names = "named", 
                         add = coll)

  checkmate::reportAssertions(coll)
  
  body <- utils::modifyList(body, list(token = rcon$token))
  body <- utils::modifyList(body, api_param)
  body <- body[lengths(body) > 0]

  config <- .curlMergeConfig(rcon$config, config)
  if(!is.null(url)) config$url <- url

  # Functional Code -------------------------------------------------
  
  for (i in seq_len(rcon$retries()))
  {
    response <-
      tryCatch(
      {
        .curlPost(body = body, config = config)
      },
      error=function(e)
      {
        if(grepl("Timeout was reached", e$message))
        {
          structure(
            list(
              status_code = 408L,
              content = charToRaw(e$message),
              headers = list('content-type' = "text/csv; charset=utf-8")
            ),
            class="response")
        } else
        {
          stop(e)
        }
      })
    
    if("options" %in% names(config)         &&
       "verbose" %in% names(config$options) &&
       is.logical(config$options$verbose)   &&
       config$options$verbose
      )
    {
      message(paste0(">>>\n", as.character(response), "<<<\n"))
    }
    
    response <- .makeApiCall_handleRedirect(rcon, body, response, ...)
    
    is_retry_eligible <- .makeApiCall_isRetryEligible(response)
    
    if (!is_retry_eligible) break
    
    # The attempt failed. Produce a message detailing the failure (when not quiet)
    if (!rcon$retry_quietly())
    {
      .makeApiCall_retryMessage(rcon = rcon, 
                                response = response, 
                                iteration = i)
    }
    
    # Wait the designated time until trying again.
    # when i = rcon$retries(), we've made all our attempts, we do not need to wait to exit the loop 
    if (i < rcon$retries()) 
      Sys.sleep(rcon$retry_interval()[i])
  }
  
  if(!response$status_code %in% success_status_codes)
    redcapError(response)
  
  response
}

####################################################################
# Unexported
.makeApiCall_handleRedirect <- function(rcon, body, response, ...)
{
  if(response$status_code %in% c(301L, 302L))
  {
    if(response$status_code == 301L)
    {
      warning(paste("Permanent 301 redirect", response$url, "to", response$headers$location))
    } else
    {
      message(paste("Temporary 302 redirect", response$url, "to", response$headers$location))
    }
    
    # Good for a single call
    makeApiCall(rcon, body, response$headers$location, ...)
  } else 
    response # The not redirected case
}

.makeApiCall_isRetryEligible <- function(response)
{
  # the return from this is a logical indicating if we are ready to break the loop.
  # we want to break the loop in cases where the response is anything that does
  # not justify a retry. 
  # It's somewhat silly to have this as a separate method, but it allows us
  # to test that we can hit the retry conditions based on the status code
  # without having to force one of these conditions onto the server. 
  # See tests for .makeApiCall_isRetryEligible in test/testthat/test-makeApiCall.R
  
  retry_eligible <- response$status_code %in% c(408, 500, 502, 503, 504)
  
  return(retry_eligible)
}

.makeApiCall_retryMessage <- function(rcon, 
                                      response, 
                                      iteration){
  msg_part1 <- sprintf("API attempt %s of %s failed. ", 
                       iteration, 
                       rcon$retries())
  msg_part2 <- 
    if (iteration < rcon$retries()){
      sprintf("Trying again in %s seconds. ", 
              rcon$retry_interval()[iteration])
    } else { # when i = retries, we are not actually going to try again. 
      ""
    }
  
  msg_part3 <- as.character(response)
  
  message(msg_part1, msg_part2, msg_part3)
}

# Helper function to convert responses to character strings without crashing.
#' @keywords internal
as.data.frame.response <- function(x, row.names=NULL, optional=FALSE, ...)
{
  # Setting defaults, necessary because cannot change S3 interface
  extra <- list(...)
  stringsAsFactors <- extra$stringsAsFactors
  if(is.null(stringsAsFactors)) stringsAsFactors <- FALSE
  na.strings <- extra$na.strings
  if(is.null(na.strings)) na.strings <- ""
  
  enc <- if(grepl("charset", x$headers[["content-type"]]))
    toupper(sub('.*charset=([^;]+).*', '\\1', x$headers[["content-type"]])) else
    'ISO-8859-1' # [Default if unspecified](https://www.w3.org/International/articles/http-charset/index)
  mapped <- iconv(readBin(x$content, character()),
                  enc, 'UTF-8', '\U25a1')
  if(grepl('\U25a1', mapped)) warning("Project contains invalid characters. Mapped to '\U25a1'.")

  # First check is very fast check to see if the first 10 bytes are empty space
  # Second check is followup to see if it's entirely empty space (verify)
  if(grepl("^\\s*$", substr(mapped, 1, 10)) &&
     nchar(trimws(mapped,'left')) == 0)
  {
    data.frame()
  }
  else
  {
    utils::read.csv(
      text             = mapped,
      stringsAsFactors = stringsAsFactors, 
      na.strings       = na.strings,
      ...)
  }
}

#' @name as.character.response
#' @title S3 method to turn curl response into character
#' 
#' @description Converts a raw curl response into a character string.
#' @export
#' @param x response from curl to render to character
#' @param ... If type='text/csv' this is passed to read.csv. If type='application/json'
#'            this is sent to jsonlite::fromJSON
as.character.response <- function(x, ...) {
  .curlContent(x, ...)
}

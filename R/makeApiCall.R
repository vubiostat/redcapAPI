#' @name makeApiCall
#' @title Make REDCap API Calls
#' 
#' @description Constructs and executes API calls to the REDCap API. These
#'   are left deliberately abstract in order to be flexible enough to 
#'   support the \code{redcapAPI} functions, but also allow users to 
#'   execute calls for new REDCap features that are not yet implemented.
#'   
#' @param rcon \code{redcapApiConnection} object.
#' @param body \code{list} List of parameters to be passed to \code{\link[httr]{POST}}'s 
#'   \code{body} argument
#' @param config \code{list} A list of options to be passed to \code{\link[httr]{POST}}.
#'   These will be appended to the \code{config} options included in the 
#'   \code{rcon} object.
#'   
#' @details The intent of this function is to provide an approach to execute
#'   calls to the REDCap API that is both consistent and flexible. Importantly, 
#'   this provides a framework for making calls to the API using features that
#'   the R package doesn't yet support (redcapAPI will always lag behind when 
#'   REDCap adds new features). 
#'   
#'   The API call consists of two components: the "body" and the "config." 
#'   The body of the call contains all of the arguments being passed to the 
#'   API. When building body components, be sure to review the documentation. 
#'   options to the API that require an array need to be built using 
#'   \code{vectorToApiBodyList}; options that are not an array can be entered
#'   directly (see examples). 
#'   
#'   The config list is a list of parameters to pass to \code{\link[httr]{POST}}. 
#'   Refer to documentation there for details.
#'   
#'   Using the settings stored in the \code{redcapConnection} object, a response
#'   code of 408 (Request Timeout), 500 (Internal Server Error), 
#'   502 (Bad Gateway), 503 (Service Unavailable), or 504 (Gateway Timeout)
#'   will prompt reattempts at calling the API. See \code{\link{redcapConnection}}
#'   for details. If the API reaches its attempt limit without resolving to 
#'   any other code, the last response is returned. If any other response
#'   code is returned at any point in the retry loop, the loop breaks and 
#'   returns that response.
#'   
#' @author Benjamin Nutter
#' 
#' @references
#' Please refer to your institution's API documentation.
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
#'   MetaData <- read.csv(text = as.character(MetaData),
#'                       stringsAsFactors = FALSE,
#'                        na.strings = "")
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
                        body = list(), 
                        config = list()){
  # Argument Validation ---------------------------------------------
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::assert_list(x = body, 
                         names = "named",
                         add = coll)
  
  checkmate::assert_list(x = config, 
                         names = "named",
                         add = coll)
  
  checkmate::reportAssertions(coll)
  
  # Functional Code -------------------------------------------------
  
  for (i in seq_len(rcon$retries())){
    response <-   
      httr::POST(url = rcon$url, 
                 body = c(list(token = rcon$token), 
                          body),
                 config = c(rcon$config, 
                            config))
    
    is_retry_eligible <- .makeApiCall_isRetryEligible(response = response)
    
    if (!is_retry_eligible) 
      break
    
    # The attempt failed. Produce a message detailing the failure (when not quiet)
    if (!rcon$retry_quietly()){
      .makeApiCall_retryMessage(rcon = rcon, 
                                response = response, 
                                iteration = i)
    }
    
    # Wait the designated time until trying again.
    # when i = rcon$retries(), we've made all our attempts, we don't need to wait to exit the loop 
    if (i < rcon$retries()) { 
      Sys.sleep(rcon$retry_interval()[i])
    }
  }
  
  response
}

####################################################################
# Unexported

.makeApiCall_isRetryEligible <- function(response){
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
    } else { # when i = retries, we aren't actually going to try again. 
      ""
    }
  
  msg_part3 <- as.character(response)
  
  message(msg_part1, msg_part2, msg_part3)
}

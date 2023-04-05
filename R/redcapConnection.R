#' @name redcapConnection
#' 
#' @title Connect to a REDCap Database
#' @description Creates an object of class \code{redcapApiConnection} for 
#' using the REDCap API 
#' 
#' @param url URL for a REDCap database API.  Check your institution's REDCap 
#'   documentation for this address.  Either \code{url} or \code{conn} must 
#'   be specified.
#' @param token REDCap API token
#' @param config A list to be passed to \code{httr::POST}.  This allows the 
#'   user to set additional configurations for the API calls, such as 
#'   certificates, ssl version, etc. For the majority of users, this does 
#'   not need to be altered.  See Details for more about this argument's 
#'   purpose and the \code{redcapAPI} wiki for specifics on its use.
#' @param retries \code{integerish(1)}. Sets the number of attempts to make to the
#'   API if a timeout error is encountered. Must be a positive value.
#' @param retry_interval \code{numeric}. Sets the intervals (in seconds) at 
#'   which retries are attempted. By default, set at a \code{2^r} where 
#'   \code{r} is the \code{r}th retry (ie, 2, 4, 8, 16, ...). For fixed 
#'   intervals, provide a single value. Values will be recycled to match
#'   the number of retries.
#' @param retry_quietly \code{logical(1)}. When \code{FALSE}, messages will 
#'   be shown giving the status of the API calls. Defaults to \code{TRUE}.
#' @param x \code{redcapConnection} object to be printed
#' @param ... arguments to pass to other methods
#'   
#' @details
#' \code{redcapConnection} objects will retrieve and cache various forms of 
#' project information. This can make metadata, arms, events, instruments, fieldnames, 
#' arm-event mappings, users, version, project information, and fileRepository available
#' directly from the \code{redcapConnection} object. Take note that 
#' the retrieval of these objects uses the default values of the respective
#' export functions (excepting the file repository, 
#' which uses \code{recursive = TRUE}). 
#' 
#' For each of these objects, there are four methods that can be called from 
#' the \code{redcapConnection} object: the get method (called via
#' \code{rcon$metadata()}, for example); the has method (\code{rcon$has_metadata}), 
#' which returns a logical indicating if the metadata has been cached; 
#' the flush method (\code{rcon$flush_metadata}), which removes the cached value; 
#' and the refresh method (\code{rcon$refresh_metadata}), which replaces the 
#' current value with a new call to the API. There is also a \code{flush_all}
#' and \code{refresh_all} method.
#' 
#' The \code{redcapConnection} object also stores the user preferences for 
#' handling repeated attempts to call the API. In the event of a timeout 
#' error or server unavailability, these settings allow a system pause before
#' attempting another API call. In the event all of the retries fail, the 
#' error message of the last attempt will be returned. These settings may 
#' be altered at any time using the methods \code{rcon$set_retries(r)}, 
#' \code{rcon$set_retry_interval(ri)}, and \code{rcon$set_retry_quietly(rq)}. 
#' The argument to these functions have the same requirements as the 
#' corresponding arguments to \code{redcapConnection}.
#' 
#' For convenience, you may consider using 
#' \code{options(redcap_api_url=[your URL here])} in your RProfile.
#' To obtain an API token for a project, do the following:\cr
#' Enter the 'User Rights' section of a project\cr
#' Select a user\cr
#' Check the box for 'API Data Export' or 'API Data Import,' as appropriate.  A full tutorial on 
#' configuring REDCap to use the API can be found at \url{https://github.com/vubiostat/redcapAPI/wiki}
#' 
#' Tokens are specific to a project, and a token must be created for each 
#' project for which you wish to use the API.
#' 
#' The \code{config} argument is passed to the \code{httr::POST} argument of 
#' the same name.  The most likely reason for using this argument is that the 
#' certificate files bundled in \code{httr} have fallen out of date.  
#' Hadley Wickham is pretty good about keeping those certificates up 
#' to date, so most of the time this problem can be resolved by updating 
#' \code{httr} to the most recent version.  If that doesn't work, a 
#' certificate file can be manually passed via the \code{config} argument.  
#' The \code{redcapAPI} wiki has a more detailed tutorial on how to 
#' find and pass an SSL certificate to the API call 
#' (\url{https://github.com/vubiostat/redcapAPI/wiki/Manually-Setting-an-SSL-Certificate-File}).
#' 
#' Additional Curl option can be set in the \code{config} argument.  See the documentation
#' for \code{httr::config} and \code{httr:httr_options} for more Curl options.
#' 
#' @author Jeffrey Horner
#' 
#' @examples
#' \dontrun{
#' rcon <- redcapConnection(url=[YOUR_REDCAP_URL], token=[API_TOKEN])
#' 
#' options(redcap_api_url=[YOUR_REDCAP_URL])
#' rcon <- redcapConnection(token=[API_TOKEN])
#' 
#' exportRecords(rcon)
#' 
#' # Get the complete metadata for the project
#' rcon$metadata()
#' 
#' # Get the fieldnames for a project
#' rcon$fieldnames()
#' 
#' # remove a cached value for fieldnames
#' rcon$flush_fieldnames()
#' rcon$has_fieldnames()
#' }
#' 
#' @export

redcapConnection <- function(url = getOption('redcap_api_url'),
                             token,
                             config = httr::config(), 
                             retries = 5, 
                             retry_interval = 2^(seq_len(retries)), 
                             retry_quietly = TRUE)
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_character(x = url, 
                              len = 1, 
                              add = coll)
  
  checkmate::assert_character(x = token, 
                              len = 1, 
                              add = coll)
  
  checkmate::assert_integerish(x = retries, 
                               len = 1, 
                               lower = 1, 
                               any.missing = FALSE, 
                               add = coll)
  
  checkmate::assert_numeric(x = retry_interval, 
                            lower = 0,
                            any.missing = FALSE, 
                            add = coll)
  
  checkmate::assert_logical(x = retry_quietly, 
                            len = 1, 
                            any.missing = FALSE, 
                            add = coll)
  
  checkmate::reportAssertions(coll)
  
  u <- url
  t <- token
  this_metadata <- NULL
  this_arm <- NULL
  this_event <- NULL
  this_fieldname <- NULL
  this_mapping <- NULL
  this_user <- NULL
  this_version <- NULL
  this_project <- NULL
  this_instrument <- NULL
  this_fileRepository <- NULL
  rtry <- retries
  rtry_int <- rep(retry_interval, 
                  length.out = rtry)
  rtry_q <- retry_quietly
  
  getter <- function(export){
    switch(export, 
           "metadata" = exportMetaData(rc), 
           "arm" = exportArms(rc), 
           "event" = exportEvents(rc),
           "fieldname" = exportFieldNames(rc), 
           "mapping" = exportMappings(rc), 
           "user" = exportUsers(rc), 
           "version" = exportVersion(rc), 
           "project" = exportProjectInformation(rc), 
           "instrument" = exportInstruments(rc), 
           "fileRepo" = exportFileRepositoryListing(rc, recursive = TRUE),
           NULL)
  }
  
  rc <- 
    list(
      url = u, 
      token = t, 
      config = config, 
      
      metadata = function(){ if (is.null(this_metadata)) this_metadata <<- getter("metadata"); this_metadata }, 
      has_metadata = function() !is.null(this_metadata),
      flush_metadata = function() this_metadata <<- NULL, 
      refresh_metadata = function() this_metadata <<- getter("metadata"), 
      
      arms = function(){ if (is.null(this_arm)) this_arm <<- getter("arm"); this_arm }, 
      has_arms = function() !is.null(this_arm), 
      flush_arms = function() this_arm <<- NULL, 
      refresh_arms = function() this_arm <<- getter("arm"), 
      
      events = function(){ if (is.null(this_event)) this_event <<- getter("event"); this_event}, 
      has_events = function() !is.null(this_event), 
      flush_events = function() this_event <<- NULL, 
      refresh_events = function() this_event <<- getter("event"), 
      
      fieldnames = function(){ if (is.null(this_fieldname)) this_fieldname <<- getter("fieldname"); this_fieldname }, 
      has_fieldnames = function() !is.null(this_fieldname), 
      flush_fieldnames = function() this_fieldname <<- NULL, 
      refresh_fieldnames = function() this_fieldname <<- getter("fieldname"), 
      
      mapping = function(){ if (is.null(this_mapping)) this_mapping <<- getter("mapping"); this_mapping }, 
      has_mapping = function() !is.null(this_mapping), 
      flush_mapping = function() this_mapping <<- NULL, 
      refresh_mapping = function() this_mapping <<- getter("mapping"), 
      
      users = function(){ if (is.null(this_user)) this_user <<- getter("user"); this_user }, 
      has_users = function() !is.null(this_user), 
      flush_users = function() this_user <<- NULL, 
      refresh_users = function() this_user <<- getter("user"), 
      
      version = function(){ if (is.null(this_version)) this_version <<- getter("version"); this_version }, 
      has_version = function() !is.null(this_version), 
      flush_version = function() this_version <<- NULL, 
      refresh_version = function() this_version <<- getter("version"), 
      
      projectInformation = function(){ if (is.null(this_project)) this_project <<- getter("project"); this_project }, 
      has_projectInformation = function() !is.null(this_project), 
      flush_projectInformation = function() this_project <<- NULL, 
      refresh_projectInformation = function() this_project <<- getter("project"), 
      push_projectInformation = function(push) this_project <<- push, 
      
      instruments = function(){ if (is.null(this_instrument)) this_instrument <<- getter("instrument"); this_instrument },
      has_instruments = function() !is.null(this_instrument), 
      flush_instruments = function() this_instrument <<- NULL, 
      refresh_instruments = function() this_instrument <<- getter("instrument"), 
      
      fileRepository = function(){ if (is.null(this_fileRepository)) this_fileRepository <<- getter("fileRepo"); this_fileRepository },
      has_fileRepository = function() !is.null(this_fileRepository),
      flush_fileRepository = function() this_fileRepository <<- NULL,
      refresh_fileRepository = function() this_fileRepository <<- getter("fileRepo"),
      
      flush_all = function(){ 
        this_metadata <<- this_arm <<- this_event <<- this_fieldname <<- 
          this_mapping <<- this_user <<- this_version <<- this_project <<- 
          this_instrument <<- this_fileRepository <<- NULL}, 
      
      refresh_all = function(){
        this_metadata <<- getter("metadata")
        this_arm <<- getter("arm")
        this_event <<- getter("event")
        this_fieldname <<- getter("fieldname")
        this_mapping <<- getter("mapping")
        this_user <<- getter("user")
        this_version <<- getter("version")
        this_project <<- getter("project")
        this_instrument <<- getter("instrument")
        this_fileRepository <<- getter("fileRepo")
      },
      
      retries = function() rtry, 
      set_retries = function(r){
        checkmate::assert_integerish(x = r, 
                                     len = 1, 
                                     lower = 1,
                                     any.missing = FALSE) 
        rtry <<- r
      },
      
      retry_interval = function() rtry_int, 
      set_retry_interval = function(ri){
        checkmate::assert_numeric(x = ri, 
                                  lower = 0,
                                  any.missing = FALSE)
        rtry_int <<- rep(ri, length.out = rtry)
      }, 
      
      retry_quietly = function() rtry_q, 
      set_retry_quietly = function(rq){
        checkmate::assert_logical(x = rq, 
                                  len = 1, 
                                  any.missing = FALSE)
        rtry_q <<- rq
      }
    )
  class(rc) <- "redcapApiConnection"
  rc
}

#' @rdname redcapConnection
#' @export

print.redcapApiConnection <- function(x, ...){
  is_cached <- function(l) if (l) "Cached" else "Not Cached" 
  output <- 
    c("REDCap API Connection Object:", 
      sprintf("Meta Data   : %s", is_cached(x$has_metadata())), 
      sprintf("Arms        : %s", is_cached(x$has_arms())), 
      sprintf("Events      : %s", is_cached(x$has_events())),
      sprintf("Instruments : %s", is_cached(x$has_instruments())),
      sprintf("Field Names : %s", is_cached(x$has_fieldnames())), 
      sprintf("Mapping     : %s", is_cached(x$has_mapping())),
      sprintf("Users       : %s", is_cached(x$has_users())), 
      sprintf("Version     : %s", is_cached(x$has_version())), 
      sprintf("Project Info: %s", is_cached(x$has_projectInformation())), 
      sprintf("File Repo   : %s", is_cached(x$has_fileRepository())))
  cat(output, sep = "\n")
}


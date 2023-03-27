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
#' @param x \code{redcapConnection} object to be printed
#' @param ... arguments to pass to other methods
#'   
#' @details
#' \code{redcapConnection} objects will retrieve and cache various forms of 
#' project information. This can make metadata, arms, events, fieldnames, 
#' arm-event mappings, users, version, and project information available
#' directly from the \code{redcapConnection} object. Take note that 
#' the retrieval of these objects uses the default values of the respective
#' export functions. 
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
                             config = httr::config())
{
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_character(x = url, 
                              len = 1, 
                              add = coll)
  
  checkmate::assert_character(x = token, 
                              len = 1, 
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
      sprintf("Field Names : %s", is_cached(x$has_fieldnames())), 
      sprintf("Mapping     : %s", is_cached(x$has_mapping())),
      sprintf("Users       : %s", is_cached(x$has_users())), 
      sprintf("Version     : %s", is_cached(x$has_version())), 
      sprintf("Project Info: %s", is_cached(x$has_projectInformation())))
  cat(output, sep = "\n")
}


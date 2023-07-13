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
#' project information. This can make metadata, arms, dags, events, instruments, fieldnames, 
#' arm-event mappings, users, version, project information, fileRepository,
#' and repeating instruments available
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
  this_repeat <- NULL
  this_dag <- NULL
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
           "repeat" = exportRepeatingInstrumentsEvents(rc),
           "dags" = exportDags(rc),
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
      
      instruments = function(){ if (is.null(this_instrument)) this_instrument <<- getter("instrument"); this_instrument },
      has_instruments = function() !is.null(this_instrument), 
      flush_instruments = function() this_instrument <<- NULL, 
      refresh_instruments = function() this_instrument <<- getter("instrument"), 
      
      fileRepository = function(){ if (is.null(this_fileRepository)) this_fileRepository <<- getter("fileRepo"); this_fileRepository },
      has_fileRepository = function() !is.null(this_fileRepository),
      flush_fileRepository = function() this_fileRepository <<- NULL,
      refresh_fileRepository = function() this_fileRepository <<- getter("fileRepo"),
      
      repeatInstrumentEvent = function(){ if (is.null(this_repeat)) this_repeat <<- getter("repeat"); this_repeat }, 
      has_repeatInstrumentEvent = function() !is.null(this_repeat),
      flush_repeatInstrumentEvent = function() this_repeat <<- NULL,
      refresh_repeatInstrumentEvent = function() this_repeat <<- getter("repeat"),
      
      dags = function() {if (is.null(this_dag)) this_dag <<- getter("dags"); this_dag },
      has_dags = function() !is.null(this_dag), 
      flush_dags = function() this_dag <<- NULL, 
      refresh_dags = function() this_dag <<- getter("dags"),
      
      flush_all = function(){ 
        this_metadata <<- this_arm <<- this_event <<- this_fieldname <<- 
          this_mapping <<- this_user <<- this_version <<- this_project <<- 
          this_instrument <<- this_fileRepository <<- this_repeat <<- 
          this_dag <<- NULL}, 
      
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
        this_repeat <<- getter("repeat")
        this_dag <<- getter("dag")
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
  class(rc) <- c("redcapApiConnection", "redcapConnection")
  rc
}

#' @rdname redcapConnection
#' @export

print.redcapApiConnection <- function(x, ...){
  is_cached <- function(l) if (l) "Cached" else "Not Cached" 

  output <- 
    c("REDCap API Connection Object", 
      sprintf("Meta Data   : %s", is_cached(x$has_metadata())), 
      sprintf("Arms        : %s", is_cached(x$has_arms())), 
      sprintf("DAGs        : %s", is_cached(x$has_dags())),
      sprintf("Events      : %s", is_cached(x$has_events())),
      sprintf("Instruments : %s", is_cached(x$has_instruments())),
      sprintf("Field Names : %s", is_cached(x$has_fieldnames())), 
      sprintf("Mapping     : %s", is_cached(x$has_mapping())),
      sprintf("Repeat Inst.: %s", is_cached(x$has_repeatInstrumentEvent())),
      sprintf("Users       : %s", is_cached(x$has_users())), 
      sprintf("Version     : %s", is_cached(x$has_version())), 
      sprintf("Project Info: %s", is_cached(x$has_projectInformation())), 
      sprintf("File Repo   : %s", is_cached(x$has_fileRepository())))
  cat(output, sep = "\n")
}

#' @rdname redcapConnection
#' @param meta_data Either a \code{character} giving the file from which the 
#'   metadata can be read, or a \code{data.frame}.
#' @param arms Either a \code{character} giving the file from which the 
#'   arms can be read, or a \code{data.frame}.
#' @param events Either a \code{character} giving the file from which the 
#'   events can be read, or a \code{data.frame}.
#' @param instruments Either a \code{character} giving the file from which the 
#'   instruments can be read, or a \code{data.frame}.
#' @param field_names Either a \code{character} giving the file from which the 
#'   field names can be read, or a \code{data.frame}.
#' @param mapping Either a \code{character} giving the file from which the 
#'   Event Instrument mappings can be read, or a \code{data.frame}.
#' @param users Either a \code{character} giving the file from which the 
#'   User settings can be read, or a \code{data.frame}.
#' @param version Either a \code{character} giving the file from which the 
#'   version can be read, or a \code{data.frame}.
#' @param project_info Either a \code{character} giving the file from which the 
#'   Project Information can be read, or a \code{data.frame}.
#' @param file_repo Either a \code{character} giving the file from which the 
#'   File Repository Listing can be read, or a \code{data.frame}.
#' @param repeat_instrument Either a \code{character} giving the file from which the 
#'   Repeating Instruments and Events settings can be read, or a \code{data.frame}.
#'   Note: The REDCap GUI doesn't offer a download file of these settings 
#'   (at the time of this writing).
#' @param records Either a \code{character} giving the file from which the 
#'   Records can be read, or a \code{data.frame}. This should be the raw 
#'   data as downloaded from the API, for instance. Using labelled or formatted
#'   data is likely to result in errors when passed to other functions. 
#' @param dags Either a \code{character} giving the file from which the 
#'   Data Access Groups can be read, or a \code{data.frame}.
#' @export

offlineConnection <- function(meta_data = NULL, 
                              arms = NULL, 
                              events = NULL, 
                              instruments = NULL, 
                              field_names = NULL, 
                              mapping = NULL, 
                              users = NULL, 
                              version = NULL, 
                              project_info = NULL, 
                              file_repo = NULL, 
                              repeat_instrument = NULL,
                              records = NULL, 
                              dags = NULL){
  ###################################################################
  # Argument Validation 
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert(
    checkmate::check_character(x = meta_data, 
                               len = 1, 
                               null.ok = TRUE), 
    checkmate::check_data_frame(x = meta_data, 
                                null.ok = TRUE), 
    combine = "or",
    .var.name = "meta_data", 
    add = coll
  )
  
  checkmate::assert(
    checkmate::check_character(x = arms, 
                               len = 1, 
                               null.ok = TRUE), 
    checkmate::check_data_frame(x = arms, 
                                null.ok = TRUE), 
    .var.name = "arms", 
    add = coll
  )
  
  checkmate::assert(
    checkmate::check_character(x = events, 
                               len = 1, 
                               null.ok = TRUE), 
    checkmate::check_data_frame(x = events, 
                                null.ok = TRUE), 
    .var.name = "events", 
    add = coll
  )
  
  checkmate::assert(
    checkmate::check_character(x = instruments, 
                               len = 1, 
                               null.ok = TRUE), 
    checkmate::check_data_frame(x = instruments, 
                                null.ok = TRUE), 
    .var.name = "instruments", 
    add = coll
  )
  
  checkmate::assert(
    checkmate::check_character(x = field_names, 
                               len = 1, 
                               null.ok = TRUE), 
    checkmate::check_data_frame(x = field_names, 
                                null.ok = TRUE), 
    .var.name = "field_names", 
    add = coll
  )
  
  checkmate::assert(
    checkmate::check_character(x = mapping, 
                               len = 1, 
                               null.ok = TRUE), 
    checkmate::check_data_frame(x = mapping, 
                                null.ok = TRUE), 
    .var.name = "mapping", 
    add = coll
  )
  
  checkmate::assert(
    checkmate::check_character(x = users, 
                               len = 1, 
                               null.ok = TRUE), 
    checkmate::check_data_frame(x = users, 
                                null.ok = TRUE), 
    .var.name = "users", 
    add = coll
  )
  
  checkmate::assert(
    checkmate::check_character(x = version, 
                               len = 1, 
                               null.ok = TRUE), 
    checkmate::check_data_frame(x = version, 
                                null.ok = TRUE), 
    .var.name = "version", 
    add = coll
  )
  
  checkmate::assert(
    checkmate::check_character(x = project_info, 
                               len = 1, 
                               null.ok = TRUE), 
    checkmate::check_data_frame(x = project_info, 
                                null.ok = TRUE), 
    .var.name = "project_info", 
    add = coll
  )
  
  checkmate::assert(
    checkmate::check_character(x = file_repo, 
                               len = 1, 
                               null.ok = TRUE), 
    checkmate::check_data_frame(x = file_repo, 
                                null.ok = TRUE), 
    .var.name = "file_repo", 
    add = coll
  )
  
  checkmate::assert(
    checkmate::check_character(x = repeat_instrument, 
                               len = 1, 
                               null.ok = TRUE), 
    checkmate::check_data_frame(x = repeat_instrument, 
                                null.ok = TRUE), 
    .var.name = "repeat_instrument", 
    add = coll
  )
  
  checkmate::assert(
    checkmate::check_character(x = records, 
                               len = 1, 
                               null.ok = TRUE), 
    checkmate::check_data_frame(x = records, 
                                null.ok = TRUE), 
    .var.name = "records", 
    add = coll
  )
  
  checkmate::assert(
    checkmate::check_character(x = dags, 
                               len = 1, 
                               null.ok = TRUE), 
    checkmate::check_data_frame(x = dags, 
                                null.ok = TRUE), 
    .var.name = "dags", 
    add = coll
  )
  
  checkmate::reportAssertions(coll)
  
  ###################################################################
  # Argument Validation - Part Two
  
  if (is.character(meta_data)){
    checkmate::assert_file_exists(x = meta_data, 
                                  add = coll)
  }
  
  if (is.character(arms)){
    checkmate::assert_file_exists(x = arms, 
                                  add = coll)
  }
  
  if (is.character(events)){
    checkmate::assert_file_exists(x = events, 
                                  add = coll)
  }
  
  if (is.character(instruments)){
    checkmate::assert_file_exists(x = instruments, 
                                  add = coll)
  }
  
  if (is.character(field_names)){
    checkmate::assert_file_exists(x = field_names, 
                                  add = coll)
  }
  
  if (is.character(mapping)){
    checkmate::assert_file_exists(x = mapping, 
                                  add = coll)
  }
  
  if (is.character(users)){
    checkmate::assert_file_exists(x = users, 
                                  add = coll)
  }
  
  if (is.character(project_info)){
    checkmate::assert_file_exists(x = project_info, 
                                  add = coll)
  }
  
  if (is.character(file_repo)){
    checkmate::assert_file_exists(x = file_repo, 
                                  add = coll)
  }
  
  if (is.character(repeat_instrument)){
    checkmate::assert_file_exists(x = repeat_instrument, 
                                  add = coll)
  }
  
  if (is.character(records)){
    checkmate::assert_file_exists(x = records, 
                                  add = coll)
  }
  
  if (is.character(dags)){
    checkmate::assert_file_exists(x = dags, 
                                  add = coll)
  }
  
  checkmate::reportAssertions(coll)
  
  ###################################################################
  # Read files
  this_metadata <- 
    validateRedcapData(data = .offlineConnection_readMetaData(meta_data), 
                       redcap_data = REDCAP_METADATA_STRUCTURE)
  this_arm <- 
    validateRedcapData(data = .offlineConnection_readFile(arms), 
                       redcap_data = REDCAP_ARMS_STRUCTURE)
  this_event <- 
    validateRedcapData(data = .offlineConnection_readFile(events), 
                       redcap_data = REDCAP_EVENT_STRUCTURE)
  this_fieldname <- 
    if (is.null(field_names) & !is.null(this_metadata)){
      .fieldNamesFromMetaData(this_metadata)
    } else {
      validateRedcapData(data = .offlineConnection_readFile(field_names), 
                         redcap_data = REDCAP_FIELDNAME_STRUCTURE)
    }
  this_mapping <- 
    validateRedcapData(data = .offlineConnection_readFile(mapping), 
                       redcap_data = REDCAP_INSTRUMENT_MAPPING_STRUCTURE)
  this_user <- 
    validateRedcapData(data = .offlineConnection_readFile(users), 
                       redcap_data = REDCAP_USER_STRUCTURE)
  this_version <- version
  this_project <- 
    validateRedcapData(data = .offlineConnection_readFile(project_info), 
                       redcap_data = REDCAP_PROJECT_INFORMATION_STRUCTURE)
  this_fileRepository <- .offlineConnection_readFile(file_repo)
  
  this_repeat <- .offlineConnection_readFile(repeat_instrument)
  
  this_instrument <- 
    if (is.null(instruments) & !is.null(this_metadata)){
      data.frame(instrument_name = unique(this_metadata$form_name), 
                 instrument_label = unique(this_metadata$form_name), 
                 stringsAsFactors = FALSE)
    } else {
      validateRedcapData(data = .offlineConnection_readFile(instruments), 
                         redcap_data = REDCAP_INSTRUMENT_STRUCTURE)
    }
  
  this_dag <- 
    validateRedcapData(data = .offlineConnection_readFile(dags), 
                       redcap_data = REDCAP_DAG_STRUCTURE)
  
  this_record <- .offlineConnection_readFile(records)
  
  rc <- 
    list(
      url = NULL, 
      token = NULL, 
      config = NULL, 
      
      metadata = function(){ this_metadata }, 
      has_metadata = function() !is.null(this_metadata),
      flush_metadata = function() this_metadata <<- NULL, 
      refresh_metadata = function(x) {this_metadata <<- validateRedcapData(data = .offlineConnection_readFile(x), 
                                                                           redcap_data = REDCAP_METADATA_STRUCTURE)}, 
      
      arms = function(){ this_arm }, 
      has_arms = function() !is.null(this_arm), 
      flush_arms = function() this_arm <<- NULL, 
      refresh_arms = function(x) {this_arm <<- validateRedcapData(data = .offlineConnection_readFile(x), 
                                                                  redcap_data = REDCAP_ARMS_STRUCTURE)}, 
      
      events = function(){ this_event}, 
      has_events = function() !is.null(this_event), 
      flush_events = function() this_event <<- NULL, 
      refresh_events = function(x) {this_event <<- validateRedcapData(data = .offlineConnection_readFile(x), 
                                                                      redcap_data = REDCAP_EVENT_STRUCTURE)}, 
      
      fieldnames = function(){ this_fieldname }, 
      has_fieldnames = function() !is.null(this_fieldname), 
      flush_fieldnames = function() this_fieldname <<- NULL, 
      refresh_fieldnames = function(x = NULL) {
        this_fieldname <<- 
          if (is.null(x) & !is.null(this_metadata)){
            .fieldNamesFromMetaData(this_metadata)
          } else {
            validateRedcapData(data = .offlineConnection_readFile(x), 
                               redcap_data = REDCAP_FIELDNAME_STRUCTURE)
          }
      }, 
      
      mapping = function(){ this_mapping }, 
      has_mapping = function() !is.null(this_mapping), 
      flush_mapping = function() this_mapping <<- NULL, 
      refresh_mapping = function(x) { this_mapping <<- validateRedcapData(data = .offlineConnection_readFile(x), 
                                                                          redcap_data = REDCAP_INSTRUMENT_MAPPING_STRUCTURE)}, 
      
      users = function(){ this_user }, 
      has_users = function() !is.null(this_user), 
      flush_users = function() this_user <<- NULL, 
      refresh_users = function(x) {this_user <<- validateRedcapData(data = .offlineConnection_readFile(x), 
                                                                    redcap_data = REDCAP_USER_STRUCTURE)}, 
      
      version = function(){ this_version }, 
      has_version = function() !is.null(this_version), 
      flush_version = function() this_version <<- NULL, 
      refresh_version = function(x) {this_version <<- x}, 
      
      projectInformation = function(){ this_project }, 
      has_projectInformation = function() !is.null(this_project), 
      flush_projectInformation = function() this_project <<- NULL, 
      refresh_projectInformation = function(x) {this_project <<- validateRedcapData(data = .offlineConnection_readFile(x), 
                                                                                    redcap_data = REDCAP_PROJECT_INFORMATION_STRUCTURE)}, 
      
      instruments = function(){ this_instrument },
      has_instruments = function() !is.null(this_instrument), 
      flush_instruments = function() this_instrument <<- NULL, 
      refresh_instruments = function(x) {
        this_instrument <<- 
          if (is.null(x) & !is.null(this_metadata)){
            data.frame(instrument_name = unique(this_metadata$form_name), 
                       instrument_label = unique(this_metadata$form_name), 
                       stringsAsFactors = FALSE)
          } else {
            validateRedcapData(data = .offlineConnection_readFile(x), 
                               redcap_data = REDCAP_INSTRUMENT_STRUCTURE)
          }
      }, 
      
      fileRepository = function(){ this_fileRepository },
      has_fileRepository = function() !is.null(this_fileRepository),
      flush_fileRepository = function() this_fileRepository <<- NULL,
      refresh_fileRepository = function(x) {this_fileRepository <<- .offlineConnection_readFile(x)},
      
      repeatInstrumentEvent = function(){ this_repeat }, 
      has_repeatInstrumentEvent = function() !is.null(this_repeat), 
      flush_repeatInstrumentEvent = function() this_project <<- NULL, 
      refresh_repeatInstrumentEvent = function(x) {this_project <<- validateRedcapData(data = .offlineConnection_readFile(x), 
                                                                                       redcap_data = REDCAP_REPEAT_INSTRUMENT_STRUCTURE)},
      
      dags = function(){ this_dag }, 
      has_dags = function() !is.null(this_dag), 
      flush_dags = function() this_dag <<- NULL, 
      refresh_dags = function(x) {this_dag <<- validateRedcapData(data = .offlineConnection_readFile(dags), 
                                                                  redcap_data = REDCAP_DAG_STRUCTURE)},
      
      records = function(){ this_record },
      has_records = function() !is.null(this_record),
      flush_records = function() this_record <<- NULL,
      refresh_records = function(x) {this_record <<- .offlineConnection_readFile(records)},
      
      flush_all = function(){ 
        this_metadata <<- this_arm <<- this_event <<- this_fieldname <<- 
          this_mapping <<- this_user <<- this_version <<- this_project <<- 
          this_instrument <<- this_fileRepository <<- this_repeat <<- NULL}, 
      
      refresh_all = function(){} # provided only to match the redcapApiConnection. Has no effect
    )
  class(rc) <- c("redcapOfflineConnection", "redcapConnection")
  rc
}

#' @rdname redcapConnection
#' @export

print.redcapOfflineConnection <- function(x, ...){
    is_cached <- function(l) if (l) "Cached" else "Not Cached" 
    
    output <- 
      c("REDCap Offline Connection Object", 
        sprintf("Records     : %s", is_cached(x$has_records())),
        sprintf("Meta Data   : %s", is_cached(x$has_metadata())), 
        sprintf("Arms        : %s", is_cached(x$has_arms())), 
        sprintf("Events      : %s", is_cached(x$has_events())),
        sprintf("Instruments : %s", is_cached(x$has_instruments())),
        sprintf("Field Names : %s", is_cached(x$has_fieldnames())), 
        sprintf("Mapping     : %s", is_cached(x$has_mapping())),
        sprintf("Repeat Inst.: %s", is_cached(x$has_repeatInstrumentEvent())),
        sprintf("Users       : %s", is_cached(x$has_users())),
        sprintf("DAGs        : %s", is_cached(x$has_dags())),
        sprintf("Version     : %s", is_cached(x$has_version())), 
        sprintf("Project Info: %s", is_cached(x$has_projectInformation())), 
        sprintf("File Repo   : %s", is_cached(x$has_fileRepository())))
    cat(output, sep = "\n")
}

#####################################################################
# Unexported

.offlineConnection_readFile <- function(file){
  if (is.character(file)){
    read.csv(file, 
             na.strings = "", 
             stringsAsFactors = FALSE, 
             colClasses = "character")
  } else {
    file
  }
}

.fieldNamesFromMetaData <- function(meta_data){
  FieldNameFrame <- 
    mapply(
      function(field_name, field_type, choices){
        if (field_type == "checkbox"){
          mapping <- fieldChoiceMapping(choices, field_name)
          data.frame(original_field_name = rep(field_name, nrow(mapping)), 
                     choice_value = mapping[, 1], 
                     export_field_name = sprintf("%s___%s", 
                                                 field_name, 
                                                 tolower(mapping[, 1])), 
                     stringsAsFactors = FALSE)
        } else {
          data.frame(original_field_name = field_name, 
                     choice_value = NA_character_, 
                     export_field_name = field_name, 
                     stringsAsFactors = FALSE)
        }
      }, 
      field_name = meta_data$field_name, 
      field_type = meta_data$field_type, 
      choices = meta_data$select_choices_or_calculations, 
      SIMPLIFY = FALSE)
  
  forms <- unique(meta_data$form_name)
  forms <- sprintf("%s_complete", forms)
  FormFieldName <- data.frame(original_field_name = forms, 
                              choice_value = NA_character_, 
                              export_field_name = forms, 
                              stringsAsFactors = FALSE)
  
  FieldNames <- do.call("rbind", FieldNameFrame)
  FieldNames <- rbind(FieldNames, FormFieldName)
  rownames(FieldNames) <- NULL
  FieldNames
}

.offlineConnection_readMetaData <- function(file){
  if (is.character(file)){
    MetaData <- read.csv(file, 
                         na.strings = "", 
                         stringsAsFactors = FALSE)
    
    names(MetaData) <- 
      ifelse(names(MetaData) %in% names(REDCAP_METADATA_API_UI_MAPPING), 
             REDCAP_METADATA_API_UI_MAPPING[names(MetaData)], 
             names(MetaData))
    
    return(MetaData)
  } else {
    file
  }
}

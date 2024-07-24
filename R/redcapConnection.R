#' @name redcapConnection
#' @title Connect to a REDCap Database
#' 
#' @description These methods enable the user to create a connection object
#'   used to access the database. 
#' 
#' @param url `character(1)`. URL for the user's REDCap database API.  
#' @param token `character(1)` REDCap API token
#' @param config A list to be passed to [curl::handle_setopt].  This allows the 
#'   user to set additional configurations for the API calls, such as 
#'   certificates, SSL version, etc. For the majority of users, this does 
#'   not need to be altered.
#' @param retries `integerish(1)`. Sets the number of attempts to make to the
#'   API if a timeout error is encountered. Must be a positive value.
#' @param retry_interval `numeric`. Sets the intervals (in seconds) at 
#'   which retries are attempted. By default, set at a `2^r` where 
#'   `r` is the `r`th retry (ie, 2, 4, 8, 16, ...). For fixed 
#'   intervals, provide a single value. Values will be recycled to match
#'   the number of retries.
#' @param retry_quietly `logical(1)`. When `FALSE`, messages will 
#'   be shown giving the status of the API calls. Defaults to `TRUE`.
#' @param x `redcapConnection` object to be printed
#' @param ... arguments to pass to other methods
#'   
#' @details
#' `redcapConnection` objects will retrieve and cache various forms of 
#' project information. This can make metadata, arms, events, etc. available 
#' directly from the `redcapConnection` object. The retrieval of these objects
#' uses the default values of the respective export functions (excepting the 
#' file repository, which uses `recursive = TRUE`). 
#' 
#' For each of these objects, there are four methods that can be called from 
#' the `redcapConnection` object: 
#' 
#' | Function type | Purpose | Example | 
#' |-----------------------|-------------------------------|----------------|
#' | `[info_type]`         | Returns the information from the connection object | `rcon$metadata()` |
#' | `has_[info_type]`     | Returns a boolean indicating if the information is cached | `rcon$has_metadata()` |
#' | `flush_[info_type]`   | Purges the information from the connection object | `rcon$flush_metadata()` | 
#' | `refresh_[info_type]` | Replaces the information with a new call to the API | `rcon$refresh_metadata()` |
#' 
#' Information is cached for 
#' 
#' * `metadata`
#' * `arms`
#' * `events`
#' * `instruments`
#' * `fieldnames`
#' * `mapping` (field-event mappings)
#' * `repeatInstrumentEvent`
#' * `users`
#' * `user_roles`
#' * `user_role_assignment`
#' * `dags`
#' * `dag_assignment`
#' * `projectInformation`
#' * `version`
#' * `fileRepository`
#' * `externalCoding`
#' 
#' There is also a `flush_all` and `refresh_all` method that will purge
#' the entire cache and refresh the entire cache, respectively.
#' 
#' The `externalCoding` elements relate to the code-label mappings of text fields 
#' with the external validation types (such as `sql` fields or text fields 
#' with BioPortal Ontology modules enabled). 
#' 
#' ## Specific to API Connections
#' 
#' The `redcapApiConnection` object also stores the user preferences for 
#' handling repeated attempts to call the API. In the event of a timeout 
#' error or server unavailability, these settings allow a system pause before
#' attempting another API call. In the event all of the retries fail, the 
#' error message of the last attempt will be returned. These settings may 
#' be altered at any time using the methods `rcon$set_retries(r)`, 
#' `rcon$set_retry_interval(ri)`, and `rcon$set_retry_quietly(rq)`. 
#' The argument to these functions have the same requirements as the 
#' corresponding arguments to `redcapConnection`.
#' 
#' Tokens are specific to a project, and a token must be created for each 
#' project for which the user wishes to use the API.
#' 
#' Additional Curl option can be set in the `config` argument.  See the documentation
#' for [curl::handle_setopt] for more curl options.
#' 
#' ## Specific to Offline Connections
#' 
#' "Offline connections" are a tool designed to provide the users without 
#' API privileges with at least a subset of the functionality available to 
#' API users. The offline connections are typically constructed from the 
#' comma separated value (CSV) files downloaded from the REDCap user 
#' interface. Alternatively, data frames may be provided with the 
#' necessary data.
#' 
#' Not all of the components of an offline connection are needed for most 
#' operations. Rather, the object was built to accept the same components
#' available to the `redcapApiConnection` in order to provide a consistent
#' interface and simplify future development.
#' 
#' The meta data will be required for nearly all operations. For 
#' validating and casting data, the `records` data must be provided, and 
#' works best if the data are the raw, unlabeled data downloaded from the
#' REDCap user interface.
#' 
#' Other components that may prove useful when casting records are the 
#' url, version, events (if the project is longitudinal), and a subset 
#' of the project information. The user is encouraged to review the 
#' vignette for working with offline connections for more details.
#' 
#' With offline connections, the refresh methods have an important difference.
#' The user may pass the refresh method a file path or data frame which 
#' will be used to replace the existing component. See examples. 
#' 
#' @seealso 
#' For establishing connections using secure token storage. \cr
#' [unlockREDCap()] \cr
#' `vignette("redcapAPI-getting-started-connecting", package = "redcapAPI")`\cr
#' 
#' For working with offline connections.
#' `vignette("redcapAPI-offline-connection", package = "redcapAPI")`\cr
#' \cr
#' To prepare data for an offline user, see [preserveProject()] and 
#' [readPreservedProject()].
#' 
#' 
#' @examples
#' \dontrun{
#' rcon <- redcapConnection(url = [YOUR_REDCAP_URL], 
#'                          token = [API_TOKEN])
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
#' 
#' 
#' # Using offline connections
#' 
#' meta_data_file <- "path/to/meta_data_file.csv"
#' records_file <- "path/to/records_file.csv"
#' events_file <- "path/to/events_file.csv"
#' 
#' ProjectInfo <- data.frame(project_id = 12345, 
#'                           is_longitudinal = 1)
#' 
#' off_conn <- offlineConnection(meta_data = meta_data_file, 
#'                               records = records_file,
#'                               project_info = ProjectInfo, 
#'                               version = [YOUR_REDCAP_VERSION_NUMBER], 
#'                               url = [YOUR_REDCAP_URL])
#'                               
#' off_conn$metadata()
#' off_conn$records()
#' off_conn$projectInformation()
#' off_conn$version()
#' 
#' # Add or replace the data in the events component.
#' off_conn$refresh_events(events_file)
#' off_conn$events()
#' }
#' 
#' @export

redcapConnection <- function(url = getOption('redcap_api_url'),
                             token,
                             config = NULL,
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
  
  checkmate::assert_list(x = config,
                         names = 'named',
                         add = coll,
                         null.ok=TRUE)
  
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
  
  config <- if(is.null(config)) .curlConfig(url, token) else
                                .curlMergeConfig(.curlConfig(url, token), config)
  
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
  this_dag_assign <- NULL
  this_user_role <- NULL
  this_user_role_assign <- NULL
  this_ec <- NULL #external coding
  
  rtry <- retries
  rtry_int <- rep(retry_interval, 
                  length.out = rtry)
  rtry_q <- retry_quietly
  
  getter <- function(export, ...){
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
           "dagAssign" = exportUserDagAssignments(rc),
           "userRole" = exportUserRoles(rc),
           "userRoleAssign" = exportUserRoleAssignments(rc),
           "externalCoding" = exportExternalCoding(rc, ...),
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
      
      user_roles = function(){ if (is.null(this_user_role)) this_user_role <<- getter("userRole"); this_user_role },
      has_user_roles = function() !is.null(this_user_role),
      flush_user_roles = function() this_user_role <<- NULL, 
      refresh_user_roles = function() this_user_role <<- getter("userRole"),
      
      user_role_assignment = function(){ if (is.null(this_user_role_assign)) this_user_role_assign <<- getter("userRoleAssign"); this_user_role_assign },
      has_user_role_assignment = function() !is.null(this_user_role_assign),
      flush_user_role_assignment = function() this_user_role_assign <<- NULL, 
      refresh_user_role_assignment = function() this_user_role_assign <<- getter("userRoleAssign"), 
      
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
      
      dag_assignment = function() {if (is.null(this_dag_assign)) this_dag_assign <<- getter("dagAssign"); this_dag_assign },
      has_dag_assignment = function() !is.null(this_dag_assign), 
      flush_dag_assignment = function() this_dag_assign <<- NULL, 
      refresh_dag_assignment = function() this_dag_assign <<- getter("dagAssign"),
      
      externalCoding = function(...) {if (is.null(this_ec)) this_ec <<- getter("externalCoding", ...); this_ec}, 
      has_externalCoding = function() !is.null(this_ec), 
      flush_externalCoding = function() this_ec <<- NULL, 
      refresh_externalCoding = function(...) this_ec <<- getter("externalCoding", ...),
      
      flush_all = function(){ 
        this_metadata <<- 
          this_arm <<- this_event <<- 
          this_instrument <<- this_fieldname <<- this_mapping <<-
          this_repeat <<- 
          this_user <<- this_user_role <<- this_user_role_assign <<-
          this_dag <<- this_dag_assign <<-
          this_project <<- this_version <<-
          this_fileRepository <<- 
          this_ec <<-
          NULL}, 
      
      refresh_all = function(){
        this_metadata <<- getter("metadata")
        this_arm <<- getter("arm")
        this_event <<- getter("event")
        this_instrument <<- getter("instrument")
        this_fieldname <<- getter("fieldname")
        this_mapping <<- getter("mapping")
        this_repeat <<- getter("repeat")
        this_user_role <<- getter("userRole")
        this_user_role_assign <<- getter("userRoleAssign")
        this_dag <<- getter("dag")
        this_dag_assign <<- getter("dagAssign")
        this_project <<- getter("project")
        this_version <<- getter("version")
        this_fileRepository <<- getter("fileRepo")
        this_ec <<- getter("externalCoding")
        
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
      sprintf("Meta Data            : %s", is_cached(x$has_metadata())), 
      sprintf("Arms                 : %s", is_cached(x$has_arms())),
      sprintf("Events               : %s", is_cached(x$has_events())), 
      sprintf("Instruments          : %s", is_cached(x$has_instruments())),
      sprintf("Field Names          : %s", is_cached(x$has_fieldnames())), 
      sprintf("Mapping              : %s", is_cached(x$has_mapping())),
      sprintf("Repeat Inst.         : %s", is_cached(x$has_repeatInstrumentEvent())),
      sprintf("Users                : %s", is_cached(x$has_users())), 
      sprintf("User Roles           : %s", is_cached(x$has_user_roles())),
      sprintf("User-Role Assignment : %s", is_cached(x$has_user_role_assignment())),
      sprintf("DAGs                 : %s", is_cached(x$has_dags())),
      sprintf("DAG Assignment       : %s", is_cached(x$has_dag_assignment())),
      sprintf("Project Info         : %s", is_cached(x$has_projectInformation())),
      sprintf("Version              : %s", is_cached(x$has_version())),  
      sprintf("File Repo            : %s", is_cached(x$has_fileRepository())), 
      sprintf("External Coding      : %s", is_cached(x$has_externalCoding())))
  cat(output, sep = "\n")
}

#' @rdname redcapConnection
#' @param meta_data Either a `character` giving the file from which the 
#'   metadata can be read, or a `data.frame`.
#' @param arms Either a `character` giving the file from which the 
#'   arms can be read, or a `data.frame`.
#' @param events Either a `character` giving the file from which the 
#'   events can be read, or a `data.frame`.
#' @param instruments Either a `character` giving the file from which the 
#'   instruments can be read, or a `data.frame`.
#' @param field_names Either a `character` giving the file from which the 
#'   field names can be read, or a `data.frame`.
#' @param mapping Either a `character` giving the file from which the 
#'   Event Instrument mappings can be read, or a `data.frame`.
#' @param repeat_instrument Either a `character` giving the file from which the 
#'   Repeating Instruments and Events settings can be read, or a `data.frame`.
#'   Note: The REDCap GUI does not offer a download file of these settings 
#'   (at the time of this writing).
#' @param users Either a `character` giving the file from which the 
#'   User settings can be read, or a `data.frame`.
#' @param user_roles Either a `character` giving the file from which the
#'   User Roles can be read, or a `data.frame`.
#' @param user_role_assignment Either a `character` giving the file from which the
#'   User-Role Assignments can be read, or a `data.frame`. 
#' @param dags Either a `character` giving the file from which the 
#'   Data Access Groups can be read, or a `data.frame`.
#' @param dag_assignment Either a `character` giving the file from which the
#'   Data Access Group Assignments can be read, or a `data.frame`.
#' @param project_info Either a `character` giving the file from which the 
#'   Project Information can be read, or a `data.frame`. See Details.
#' @param version `character(1)` giving the instance's REDCap version number.
#' @param file_repo Either a `character` giving the file from which the 
#'   File Repository Listing can be read, or a `data.frame`.
#' @param records Either a `character` giving the file from which the 
#'   Records can be read, or a `data.frame`. This should be the raw 
#'   data as downloaded from the API, for instance. Using labeled or formatted
#'   data is likely to result in errors when passed to other functions.
#' @param external_coding Named `list` of named `character` vectors or a 
#'   `character` giving the file from which the external coding may 
#'   be read. The list is generally obtained from the API using 
#'   [exportExternalCoding()]. The name of the list element should be 
#'   a field name in the data that is of type `bioportal` or `sql`. 
#'   The named vectors are code-label pairings where the value of the 
#'   vector is the code and the name is the label. If passing a file name, 
#'   it should be a file with the list saved via `dput`. 
#'   
#' @export

offlineConnection <- function(meta_data = NULL, 
                              arms = NULL, 
                              events = NULL, 
                              instruments = NULL, 
                              field_names = NULL, 
                              mapping = NULL, 
                              repeat_instrument = NULL,
                              users = NULL, 
                              user_roles = NULL, 
                              user_role_assignment = NULL,
                              dags = NULL, 
                              dag_assignment = NULL,
                              project_info = NULL, 
                              version = "14.4.0", 
                              file_repo = NULL,
                              records = NULL, 
                              url = NULL, 
                              external_coding = list()){
  ###################################################################
  # Argument Validation                                          ####
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
    checkmate::check_character(x = repeat_instrument, 
                               len = 1, 
                               null.ok = TRUE), 
    checkmate::check_data_frame(x = repeat_instrument, 
                                null.ok = TRUE), 
    .var.name = "repeat_instrument", 
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
    checkmate::check_character(x = user_roles, 
                               len = 1, 
                               null.ok = TRUE), 
    checkmate::check_data_frame(x = user_roles, 
                                null.ok = TRUE), 
    .var.name = "user_roles", 
    add = coll
  )

  checkmate::assert(
    checkmate::check_character(x = user_role_assignment, 
                               len = 1, 
                               null.ok = TRUE), 
    checkmate::check_data_frame(x = user_role_assignment, 
                                null.ok = TRUE), 
    .var.name = "user_role_assignment", 
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
  
  checkmate::assert(
    checkmate::check_character(x = dag_assignment, 
                               len = 1, 
                               null.ok = TRUE), 
    checkmate::check_data_frame(x = dag_assignment, 
                                null.ok = TRUE), 
    .var.name = "dag_assignment", 
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
  
  checkmate::assert_character(x = version, 
                              len = 1, 
                              null.ok = TRUE, 
                              add = coll)
  
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
    checkmate::check_character(x = records, 
                               len = 1, 
                               null.ok = TRUE), 
    checkmate::check_data_frame(x = records, 
                                null.ok = TRUE), 
    .var.name = "records", 
    add = coll
  )
  
  checkmate::assert_character(x = url, 
                              len = 1, 
                              null.ok = TRUE,
                              add = coll)
  
  checkmate::assert(
    checkmate::check_list(x = external_coding,
                          types = "character",
                          names = "named", 
                          null.ok = TRUE), 
    checkmate::check_character(x = external_coding, 
                               len = 1, 
                               null.ok = TRUE), 
    .var.name = "external_coding", 
    add = coll
  )
  
  checkmate::reportAssertions(coll)
  
  if (is.list(external_coding) && length(external_coding) > 0){
    for (i in seq_along(external_coding)){
      checkmate::assert_character(x = external_coding[[i]], 
                                  names = "named", 
                                  add = coll)
    }
  }
  
  checkmate::reportAssertions(coll)
  
  ###################################################################
  # Argument Validation - Part Two                               ####
  
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
  
  if (is.character(repeat_instrument)){
    checkmate::assert_file_exists(x = repeat_instrument, 
                                  add = coll)
  }
  
  if (is.character(users)){
    checkmate::assert_file_exists(x = users, 
                                  add = coll)
  }
  
  if (is.character(user_roles)){
    checkmate::assert_file_exists(x = user_roles, 
                                  add = coll)
  }

  if (is.character(user_role_assignment)){
    checkmate::assert_file_exists(x = user_role_assignment, 
                                  add = coll)
  }
  
  if (is.character(dags)){
    checkmate::assert_file_exists(x = dags, 
                                  add = coll)
  }
  
  if (is.character(dag_assignment)){
    checkmate::assert_file_exists(x = dag_assignment, 
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
  
  if (is.character(external_coding)){
    checkmate::assert_file_exists(x = external_coding, 
                                  add = coll)
  }
  
  checkmate::reportAssertions(coll)
  
  ###################################################################
  # Read files                                                   ####
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

  this_repeat <- .offlineConnection_readFile(repeat_instrument)
  
  this_user <- 
    validateRedcapData(data = .offlineConnection_readFile(users), 
                       redcap_data = redcapUserStructure(version))
  this_user_roles <- 
    validateRedcapData(data = .offlineConnection_readFile(user_roles), 
                       redcap_data = redcapUserRoleStructure(version))
  this_user_role_assignment <- 
    validateRedcapData(data = .offlineConnection_readFile(user_role_assignment), 
                       redcap_data = REDCAP_USER_ROLE_ASSIGNMENT_STRUCTURE)
  this_dags <- 
    validateRedcapData(data = .offlineConnection_readFile(dags),
                       redcap_data = REDCAP_DAG_STRUCTURE)
  this_dag_assignment <- 
    validateRedcapData(data = .offlineConnection_readFile(dag_assignment), 
                       redcap_data = REDCAP_DAG_ASSIGNMENT_STRUCTURE)
  this_project <- 
    validateRedcapData(data = .offlineConnection_readFile(project_info), 
                       redcap_data = REDCAP_PROJECT_INFORMATION_STRUCTURE)
  this_version <- version

  this_fileRepository <- .offlineConnection_readFile(file_repo)
  
  this_ec <- 
    if (is.list(external_coding)){
      external_coding
    } else {
      eval(parse(file = external_coding))
    }

  
  this_instrument <- 
    if (is.null(instruments) & !is.null(this_metadata)){
      data.frame(instrument_name = unique(this_metadata$form_name), 
                 instrument_label = unique(this_metadata$form_name), 
                 stringsAsFactors = FALSE)
    } else {
      validateRedcapData(data = .offlineConnection_readFile(instruments), 
                         redcap_data = REDCAP_INSTRUMENT_STRUCTURE)
    }
  
  this_record <- .offlineConnection_readFile(records)
  
  ###################################################################
  # Redcap Connection object                                     ####
  rc <- 
    list(
      url = url, 
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
      
      repeatInstrumentEvent = function(){ this_repeat }, 
      has_repeatInstrumentEvent = function() !is.null(this_repeat), 
      flush_repeatInstrumentEvent = function() this_project <<- NULL, 
      refresh_repeatInstrumentEvent = function(x) {this_project <<- validateRedcapData(data = .offlineConnection_readFile(x), 
                                                                                       redcap_data = REDCAP_REPEAT_INSTRUMENT_STRUCTURE)},
                                                                                       
      users = function(){ this_user }, 
      has_users = function() !is.null(this_user), 
      flush_users = function() this_user <<- NULL, 
      refresh_users = function(x) {this_user <<- validateRedcapData(data = .offlineConnection_readFile(x), 
                                                                    redcap_data = redcapUserStructure(version))}, 
      
      user_roles = function(){ this_user_roles }, 
      has_user_roles = function() !is.null(this_user_roles), 
      flush_user_roles = function() this_user_roles <<- NULL, 
      refresh_user_roles = function(x) {this_user_roles <<- validateRedcapData(data = .offlineConnection_readFile(x), 
                                                                               redcap_data = redcapUserRoleStructure(version))}, 
      
      users_role_assignment = function(){ this_user_role_assignment }, 
      has_user_role_assignment = function() !is.null(this_user_role_assignment), 
      flush_user_role_assignment = function() this_user_role_assignment <<- NULL, 
      refresh_user_role_assignment = function(x) {this_user_role_assignment <<- validateRedcapData(data = .offlineConnection_readFile(x), 
                                                                                                   redcap_data = REDCAP_USER_ROLE_ASSIGNMENT_STRUCTURE)}, 
      
      dags = function(){ this_dags }, 
      has_dags = function() !is.null(this_dags), 
      flush_dags = function() this_dags <<- NULL, 
      refresh_dags = function(x) {this_dags <<- validateRedcapData(data = .offlineConnection_readFile(x), 
                                                                  redcap_data = REDCAP_DAG_STRUCTURE)},
      
      dag_assignment = function(){ this_dag_assignment }, 
      has_dag_assignment = function() !is.null(this_dag_assignment), 
      flush_dag_assignment = function() this_dag_assignment <<- NULL, 
      refresh_dag_assignment = function(x) {this_dag_assignment <<- validateRedcapData(data = .offlineConnection_readFile(x), 
                                                                                       redcap_data = REDCAP_DAG_ASSIGNMENT_STRUCTURE)},
      
      projectInformation = function(){ this_project }, 
      has_projectInformation = function() !is.null(this_project), 
      flush_projectInformation = function() this_project <<- NULL, 
      refresh_projectInformation = function(x) {this_project <<- validateRedcapData(data = .offlineConnection_readFile(x), 
                                                                                    redcap_data = REDCAP_PROJECT_INFORMATION_STRUCTURE)}, 
      
      version = function(){ this_version }, 
      has_version = function() !is.null(this_version), 
      flush_version = function() this_version <<- NULL, 
      refresh_version = function(x) {this_version <<- x}, 
      
      fileRepository = function(){ this_fileRepository },
      has_fileRepository = function() !is.null(this_fileRepository),
      flush_fileRepository = function() this_fileRepository <<- NULL,
      refresh_fileRepository = function(x) {this_fileRepository <<- .offlineConnection_readFile(x)},
      
      records = function(){ this_record },
      has_records = function() !is.null(this_record),
      flush_records = function() this_record <<- NULL,
      refresh_records = function(x) {this_record <<- .offlineConnection_readFile(records)},
      
      externalCoding = function(...){ this_ec }, 
      has_externalCoding = function() !is.null(this_ec), 
      flush_externalCoding = function() this_ec <<- NULL,
      refresh_externalCoding = function(x, ...) {this_ec <<- x},
      
      flush_all = function(){ 
        this_metadata <<- this_arm <<- this_event <<- this_fieldname <<- 
          this_mapping <<- this_user <<- this_version <<- this_project <<- 
          this_instrument <<- this_fileRepository <<- this_repeat <<- 
          this_ec <<- NULL}, 
      
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
        sprintf("Records               : %s", is_cached(x$has_records())),
        sprintf("Meta Data             : %s", is_cached(x$has_metadata())), 
        sprintf("Arms                  : %s", is_cached(x$has_arms())), 
        sprintf("Events                : %s", is_cached(x$has_events())),
        sprintf("Instruments           : %s", is_cached(x$has_instruments())),
        sprintf("Field Names           : %s", is_cached(x$has_fieldnames())), 
        sprintf("Mapping               : %s", is_cached(x$has_mapping())),
        sprintf("Repeat Inst.          : %s", is_cached(x$has_repeatInstrumentEvent())),
        sprintf("Users                 : %s", is_cached(x$has_users())),
        sprintf("User Roles            : %s", is_cached(x$has_user_roles())),
        sprintf("Users Role Assignment : %s", is_cached(x$has_user_role_assignment())),
        sprintf("DAGs                  : %s", is_cached(x$has_dags())),
        sprintf("DAG Assigment         : %s", is_cached(x$has_dag_assignment())),
        sprintf("Project Info          : %s", is_cached(x$has_projectInformation())), 
        sprintf("Version               : %s", is_cached(x$has_version())), 
        sprintf("File Repo             : %s", is_cached(x$has_fileRepository())), 
        sprintf("External Coding       : %s", is_cached(x$has_externalCoding())))
    cat(output, sep = "\n")
}

#####################################################################
# Unexported

.offlineConnection_readFile <- function(file){
  if (is.character(file)){
    utils::read.csv(file, 
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
    MetaData <- utils::read.csv(file, 
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

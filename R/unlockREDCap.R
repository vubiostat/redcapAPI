# Copyright (C) 2021-2023 Vanderbilt University Medical Center,
# Shawn Garbett, Cole Beck, Hui Wu, Benjamin Nutter, Savannah Obregon
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

  ###########################################################################
 ## Connection function
##
.connectAndCheck <- function(key, url, ...)
{
  tryCatch(
    { 
      conn <- redcapConnection(key, url=url, ...)
      conn$metadata() # Test connection by reading metadata into cache
      conn
    },
    error = function(e)
    {
      if(grepl("Could not resolve host",     e) ||
         grepl("Couldn't connect to server", e))
        stop("Unable to connect to url '",url,"'. ", e$message)
        
      if(grepl("403", e)) return(NULL)
      
      stop(e)
    }
  )
}

  #############################################################################
 ## unlock via YAML override if it exists
##
.unlockYamlOverride <- function(connections, url, ...)
{
  config_file <- file.path("..", paste0(basename(getwd()),".yml"))
  
  if(!file.exists(config_file)) return(list())
  
  config <- read_yaml(config_file)
  if(is.null(config$redcapAPI)) stop(paste0("Config file '",config_file,"' does not contain required 'redcapAPI' entry"))
  config <- config$redcapAPI
  if(is.null(config$keys))      stop(paste0("Config file '",config_file,"' does not contain required 'keys' entry under the 'redcapAPI' entry"))
  keys   <- config$keys
  if(!is.null(config$args$url))  url <- config$args$url # Override from yml if available
  config$args$url <- NULL
  args   <- c(config$args, url = url, list(...))
    
  dest <- lapply(seq_along(connections), function(i) 
  {
    args$key  <- keys[connections[i]]
    
    if(is.null(args$key)) stop(paste0("Config file '", config_file, "'does not have API_KEY for '",connections[i],"' under keys specified."))
    
    do.call(.connectAndCheck, args)
  })
  names(dest) <- if(is.null(names(connections))) connections else names(connections)
  
  return(dest)
}
 
  #############################################################################
 ## unlock keyring
##
.unlockKeyring <- function(keyring, passwordFUN)
{
  state <- keyring::keyring_list()
  state <- state[state$keyring==keyring,]
  
  # If so, does it exist?
  if(nrow(state) == 1) # Exists => UNLOCK
  {
    locked <- state$locked
    # Is it locked
    while(locked)
    {
      msg      <- paste0("Please enter password to unlock API keyring '",keyring, "'.")
      password <- Sys.getenv("REDCAPAPI_PW")
      stored   <- !is.null(password) && password != ''
      if(!stored) password <- passwordFUN(msg)
      if(is.null(password) || password == '') stop(paste0("User aborted API keyring unlock '",keyring, "'."))
      
      tryCatch(
        {
          keyring::keyring_unlock(keyring, password)
          Sys.setenv(REDCAPAPI_PW=password)
          # Hacked work around for RStudio starting new session for everything
          if(requireNamespace("rstudioapi", quietly = TRUE) &&
            rstudioapi::isAvailable(child_ok=TRUE))
            rstudioapi::sendToConsole(paste0("Sys.setenv(REDCAPAPI_PW='", password, "')"), execute = TRUE, echo=FALSE)
          locked <- FALSE
        },
        error = function(e)
        {
          if(stored) 
          {
            Sys.unsetenv("REDCAPAPI_PW")
            stored <- FALSE
          } else
          {
            msg <-  paste0("Provided assword failed to unlock. Please enter password to unlock API keyring '",keyring, "'.")
          }
        }
      )
    }
  } else # Keyring does not exist => Create
  {
    password <- passwordFUN(paste0("Creating keyring. Enter NEW password for the keyring '",
                                   keyring, "'."))
    if(is.null(password) || password == '') stop(paste0("User cancelled creation of keyring '", keyring, "'."))

    keyring::keyring_create(keyring, password)
    Sys.setenv(REDCAPAPI_PW=password)
    if(requireNamespace("rstudioapi", quietly = TRUE) &&
       rstudioapi::isAvailable(child_ok=TRUE))
      rstudioapi::sendToConsole(paste0("Sys.setenv(REDCAPAPI_PW='", password, "')"), execute = TRUE, echo=FALSE)
    
  }
}

  #############################################################################
 ## Find the best password function
## If rstudioapi is loaded and rstudio is running, then use that.
## getOption('askpass') returns a function that doesn't work on MAC 
## when knitting from RStudio, ugh.
.default_pass <- function()
{
  if(grepl('mac', tolower(utils::osVersion))        &&
     requireNamespace("rstudioapi", quietly = TRUE) &&
     rstudioapi::isAvailable(child_ok=TRUE))
  {
    rstudioapi::askForPassword
  } else getPass::getPass
}

#' Open REDCap connections using cryptolocker for storage of API_KEYs.
#'
#' Opens a set of connections to REDcap from API_KEYs stored in an encrypted keyring.
#' If the keyring does not exist, it will ask for password to this keyring to use on
#' later requests. Next it
#' will ask for the API_KEYs specified in `connections`. If an API_KEY doesn't
#' work, it will request again. On later executions it will use an open keyring
#' to retrieve all API_KEYs or for a password if the keyring is currently
#' locked.
#' 
#' If one forgets the password to this keyring, or wishes to start over:
#' `keyring::keyring_delete("<NAME_OF_KEY_RING_HERE>")`
#' 
#' Consistent behavior requires `options(keyring_backend=keyring::backend_file)` to
#' be set. It is recommended to place this in `~/.Rprofile`.
#' 
#' For production servers where the password must be stored in a readable
#' plain text file, it will search for `../<basename>.yml`. DO NOT USE
#' this unless one is a sysadmin, as this defeats the security and purpose of
#' a local encrypted file. The expected structure of this yaml file is
#' as follows:
#' 
#' \preformatted{
#' other-config-stuff1: blah blah
#' redcapAPI:
#'   keys:
#'     intake: THIS_IS_THE_INTAKE_DATABASE_APIKEY
#'     details: THIS_IS_THE_DETAILS_DATABASE_APIKEY
#' other-config-stuff2: blah blah
#' other-config-stuff3: blah blah
#' }
#' 
#' IMPORTANT: Make sure that R is set to NEVER save workspace to .RData
#' as this *is* writing the API_KEY to a local file in clear text because
#' connection objects contain the unlocked key in memory. Tips
#' are provided in `vignette("redcapAPI-best-practices")`. 
#'
#' @param connections character vector. A list of strings that define the
#'          connections with associated API_KEYs to load into environment. Each
#'          name should correspond to a REDCap project for traceability, but 
#'          it can be named anything one desires.
#'          The name in the returned list is this name. 
#' @param envir environment. The target environment for the connections. Defaults to NULL
#'          which returns the keys as a list. Use \code{\link{globalenv}} to assign in the
#'          global environment. Will accept a number such a '1' for global as well.
#' @param keyring character. Potential keyring, not used by default.
#' @param url character. The url of one's institutional REDCap server api. 
#' @param passwordFUN function. Function to get the password for the keyring. Usually defaults `getPass::getPass`. 
#'          On MacOS it will use rstudioapi::askForPassword if available. 
#' @param \dots Additional arguments passed to \code{\link{redcapConnection}}.
#' @return If \code{envir} is NULL returns a list of opened connections. Otherwise
#'         connections are assigned into the specified \code{envir}.
#' @importFrom getPass getPass
#' @importFrom yaml read_yaml
#' @importFrom keyring key_get
#' @importFrom keyring key_list
#' @importFrom keyring key_set_with_value
#' @importFrom keyring keyring_create
#' @importFrom keyring keyring_list
#' @importFrom keyring keyring_unlock
#' @importFrom keyring keyring_is_locked
#'
#' @examples
#' \dontrun{
#' options(keyring_backend=keyring::backend_file) # Put in .Rprofile
#' 
#' unlockREDCap(c(test_conn    = 'TestRedcapAPI',
#'                sandbox_conn = 'SandboxAPI'),
#'              keyring      = '<NAME_OF_KEY_RING_HERE>',
#'              envir        = globalenv(),
#'              url          = 'https://<INSTITUTIONS_REDCAP_URL>/api/') 
#' @export
unlockREDCap    <- function(connections,
                            url,
                            keyring,
                            envir       = NULL,
                            passwordFUN = .default_pass(),
                            ...)
{
   ###########################################################################
  # Check parameters passed to function
  coll <- checkmate::makeAssertCollection()
  
  if(is.numeric(envir)) envir <- as.environment(envir)
  
  checkmate::assert_character(x = url,          null.ok = FALSE, add = coll)
  checkmate::assert_character(x = keyring,      null.ok = FALSE, add = coll)
  checkmate::assert_character(x = connections,  null.ok = FALSE, add = coll)
  checkmate::assert_function( x = passwordFUN,  null.ok = FALSE, add = coll)
  checkmate::assert_class(    x = envir,        null.ok = TRUE,  add = coll, classes="environment")
  checkmate::reportAssertions(coll)

  # Use YAML config if it exists
  dest <- .unlockYamlOverride(connections, url, varnames, ...)
  if(length(dest) > 0) 
    return(if(is.null(envir)) dest else list2env(dest, envir=envir))
  
  .unlockKeyring(keyring, passwordFUN)
  
  # Open Connections
  dest <- lapply(seq_along(connections), function(i)
  {
    stored <- connections[i] %in% keyring::key_list("redcapAPI", keyring)[,2]
    
    api_key <- if(stored)
    {
      keyring::key_get("redcapAPI", connections[i], keyring)
    } else 
    {
      passwordFUN(paste0("Please enter REDCap API_KEY for '", connections[i], "'."))
    }
    
    if(is.null(api_key) || api_key == '') stop(paste("No API_KEY entered for", connections[i]))
    
    conn <- NULL
    while(is.null(conn))
    {
      conn <- .connectAndCheck(api_key, url, ...)
      if(is.null(conn))
      {
        keyring::key_delete("redcapAPI", connections[i], keyring)
        api_key <- passwordFUN(paste0(
          "Invalid API_KEY for '", connections[i],"'. '",
                           keyring,
                            "' Possible causes include: mistyped, renewed, or revoked.",
                            " Please enter a new key or cancel to abort."))
        if(is.null(api_key) || api_key == '') stop("unlockREDCap aborted")
      } else
      {
        if(!stored) keyring::key_set_with_value("redcapAPI",
                                                username=connections[i],
                                                password=api_key,
                                                keyring=keyring)
      }
    }
    conn
  })
  names(dest) <- if(is.null(names(connections))) connections else names(connections)
  
  if(is.null(envir)) dest else list2env(dest, envir=envir)
}


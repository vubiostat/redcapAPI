# create REDCap connections from cryptolocker of API_KEYs
#
# Copyright (C) 2021-2023 Vanderbilt University Medical Center,
# Shawn Garbett, Cole Beck, Hui Wu
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


 #############################################################################
## Check if key is in package environment, aka memory
.key_saved <- function(envir, key)
{
  exists(key, envir=envir, inherits=FALSE) &&
    !is.null(envir[[key]])                   &&
    !is.na(envir[[key]])                     &&
    !envir[[key]]==''
}

#' Create a set of connections to redcap in the current (or specified 
#' environment) from API_KEYs stored in a crypto locker. On the first
#' execution it will ask to set the password for this locker. Next it
#' will ask for the API_KEYs specified. It will request the user enter
#' each key using getPass and store it in memory. If an API_KEY doesn't
#' work, it will automatically delete it from the
#' crypto locker and ask again on next execution.
#' 
#' If one forgets the password, or wishes to start over: `keyring::keyring_delete("keyring")`
#' 
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
#'   args:
#'     url: https://redcap.vanderbilt.edu/api/
#'   keys:
#'     intake: THIS_IS_THE_INTAKE_DATABASE_APIKEY
#'     details: THIS_IS_THE_DETAILS_DATABASE_APIKEY
#' other-config-stuff2: blah blah
#' other-config-stuff3: blah blah
#' }
#' 
#' IMPORTANT: Make sure that R is set to NEVER save workspace to .RData
#' as this *is* writing the API_KEY to a local
#' file in clear text.
#'
#' @param connections character vector. A list of strings that define the
#'          connections with associated API_KEYs to load into environment. Each
#'          name should correspond to a REDCap project for traceability, but 
#'          it can be anything. The variable name in the environment is this
#'          name, or if a named vector the name associated.
#' @param envir environment. The target environment for the connections. Defaults to NULL
#'          which returns the keys as a list. Use \code{\link{globalenv}} to assign the
#'          global environment.
#' @param keyring character. Potential keyring, not used by default.
#' @param url character. The url of the REDCap server's api. 
#' @param passwordFUN function. Function to get the password for the keyring. Defaults to getPass::getPass().
#' @param \dots Additional arguments passed to \code{\link{redcapConnection}}.
#' @return If \code{envir} is NULL returns a list of opened connections. Otherwise
#'         returns NULL and connections are assigned into the specified \code{envir}.
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
#'   # Cuts down on password requests on MAC
#' options(keyring_backend=keyring::backend_file)
#' 
#' unlockREDCap(c(test_conn    = 'TestRedcapAPI',
#'                sandbox_conn = 'SandboxAPI'),
#'              keyring      = 'MyKeyring',
#'              envir        = globalenv(),
#'              url          = 'https://<REDCAP_URL>/api/') 
#' }
#'
#' @export
unlockREDCap    <- function(connections,
                            url,
                            keyring,
                            envir       = NULL,
                            passwordFUN = getPass::getPass,
                            ...)
{
   ###########################################################################
  # Check parameters passed to function
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_character(x = url,          null.ok = FALSE, add = coll)
  checkmate::assert_character(x = keyring,      null.ok = FALSE, add = coll)
  checkmate::assert_character(x = connections,  null.ok = FALSE, add = coll)
  checkmate::assert_function( x = passwordFUN,  null.ok = FALSE, add = coll)
  checkmate::assert_class(    x = envir,        null.ok = TRUE,  add = coll, classes="environment")
  checkmate::reportAssertions(coll)
  
   ###########################################################################
  # Connection function
  FUN <- function(key, url, ...)
  {
    conn <- redcapConnection(key, url=url, ...)
    conn$metadata() # Test connection by reading metadata into cache
    conn
  }
    
  # Use the global environment for variable storage unless one was specified
  dest <- if(is.null(envir)) list() else envir
  
  varnames <- if(is.null(names(connections))) connections else names(connections)
  
  # If the variable exists, clear from memory
  if(is.environment(dest))
    for(i in seq_along(connections))
    {
      if(exists(varnames[i], envir=dest, inherits=FALSE)) rm(list=varnames[i], envir=dest, inherits=FALSE)
    }
  
  # Use config if it exists
  config_file <- file.path("..", paste0(basename(getwd()),".yml"))

  if(file.exists(config_file))
  {
    config <- read_yaml(config_file)
    
    config <- config$redcapAPI
    keys   <- config$keys
    args   <- c(config$args, list(...))
    
    
    for(i in seq_along(connections))
    {
      args$key  <- keys[[connections[i]]]
      args$form <- NULL

      data <-  do.call(FUN, args)
      if(is.environment(dest))
      {
        base::assign(varnames[i], data, envir=dest)
      } else {
        dest[[varnames[i]]] <- data
      }
    }

    return(if(is.environment(dest)) invisible() else dest)
  }
  
  # Create an environment to house API_KEYS locally
  if(!exists("apiKeyStore", inherits=FALSE)) apiKeyStore <- new.env()
  
  state <- keyring::keyring_list()
  state <- state[state$keyring==keyring,]
  
  # If so, does it exist?
  if(nrow(state) == 1)
  {
    # Is it locked
    if(state$locked)
    {
      password <- passwordFUN(paste0("Please enter password to unlock API keyring ",keyring, " "))
      keyring::keyring_unlock(keyring, password)
    }
  } else # Keyring does not exist
  {
    password <- passwordFUN(paste0("Creating keyring. Enter password for the API keyring ",
                                     keyring, " "))
    # Create keyring if it doesn't exist
    keyring::keyring_create(keyring, password)
  }

  # For each dataset requested
  for(i in seq_along(connections))
  {
    # If the API_KEY doesn't exist go look for it
    
    # Does it exist in a secret keyring, use that
    if(!.key_saved(apiKeyStore, connections[i]))
    {
      if(!is.null(keyring) &&
         keyring %in% (keyring::keyring_list()[,1]) &&
         connections[i] %in% keyring::key_list("redcapAPI", keyring)[,2])
      {
        apiKeyStore[[connections[i]]] <- keyring::key_get("redcapAPI", connections[i], keyring)
      }
    }
    # Check again if it's set properly
    if(!.key_saved(apiKeyStore, connections[i]))
    {
      key <- passwordFUN(paste("Please enter RedCap API_KEY for", connections[i]))
      
      if(is.null(key) || key == '') stop(paste("No Key Entered for", connections[i]))
      
      apiKeyStore[[connections[i]]] <- key
      if(!is.null(keyring))
      {
        keyring::key_set_with_value("redcapAPI", username=connections[[i]], password=apiKeyStore[[connections[i]]], keyring=keyring)
      }
    }
    
    withCallingHandlers(
      { 
        data <- FUN(apiKeyStore[[connections[i]]], url, ...)
        if(is.environment(dest))
        {
          base::assign(varnames[i], data, envir=dest)
        } else {
          dest[[varnames[i]]] <- data
        }
      },
      error = function(e)
      {
        if(grepl("Could not resolve host", e) ||
           grepl("403", e))
        {
          apiKeyStore[[connections[i]]] <- NULL
          if(is.environment(dest) && exists(varnames[i], envir=dest, inherits=FALSE))
            rm(list=varnames[i], envir=dest, inherits=FALSE)
          keyring::key_delete("redcapAPI", connections[i], keyring)
        }
        stop(e)
      }
    )
  }
  
  return(if(is.environment(dest)) invisible() else dest)
}


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
##
## Helper Functions
##
## Check if key is in package environment, aka memory
key_saved <- function(envir, key)
{
  exists(key, envir=envir, inherits=FALSE) &&
    !is.null(envir[[key]])                   &&
    !is.na(envir[[key]])                     &&
    !envir[[key]]==''
}


#' Create a set of connections to redcap in the current (or specified 
#' environment) from API_KEYs stored in a crypto locker.
#'
#' The first thing it does is check for a yaml config file of
#' the same name as the current directory with a .yml extension
#' one level above. This is intended for production environments
#' where the API_KEY must be stored in a file. If this yaml exists, then it expects this file
#' to contain `apiUrl` and `apiKeys`. `apiUrl` should be a
#' string with the URL of the REDCap instance. `apiKeys` should
#' be a list of variable name keys with values that are their
#' actual REDCap API_KEY.
#' 
#' Next it will use an api environment in memory to keep api_keys.
#' If one is knitting with parameters, it will request and store these
#' keys in memory. Otherwise it will request the user enter
#' each key using getPass and store it in memory.
#' 
#' IMPORTANT: Make sure that R is set to NEVER save workspace to .RData
#' as this is the equivalent of writing the API_KEY to a local
#' file in clear text.
#'
#' @param connections character vector. A list of strings that define the
#'          connections with associated API_KEYs to load into environment. Each
#'          name should correspond to a REDCap project for traceability, but 
#'          it can be anything. The variable name in the environment is this
#'          name, or if a named vector the name associated.
#' @param envir environment. The target environment for the data. Defaults to .Global
#' @param keyring character. Potential keyring, not used by default.
#' @param config string. Defaults to 'auto'. If set to NULL no configuration file is searched for. If set to anything
#'              but 'auto', that will be the config file override that is used if it exists instead of
#'              searching for the ../<basename>.yml.
#' @param url character. The url of the REDCap server's api. 
#' @param passwordFUN function. Function to get the password for the keyring. Defaults to getPass::getPass().
#' @param \dots Additional arguments passed to \link{\code{redcapConnection}}.
#' @return Nothing
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
#'              sandbox_conn = 'SandboxAPI'),
#'             keyring      = 'MyKeyring',
#'             url          = 'https://<REDCAP_URL>/api/') 
#' }
#'
#' @export
unlockREDCap    <- function(connections,
                            url,
                            keyring,
                            envir     = NULL,
                            config    = 'auto',
                            passwordFUN = getPass::getPass,
                            ...)
{
  FUN <- function(key, ...)
  {
    conn <- redcapConnection(key, url=url, ...)
    conn$metadata() # Test connection by reading metadata into cache
    conn
  }
    
  # Use the global environment for variable storage unless one was specified
  dest <- if(is.null(envir)) globalenv() else envir
  
  varnames <- if(is.null(names(connections))) connections else names(connections)
  
  # If the variable exists, clear from memory
  for(i in seq_along(connections))
  {
    if(exists(varnames[i], envir=dest, inherits=FALSE)) rm(list=varnames[i], envir=dest)
  }
  
  # Use config if it exists
  config_file <- if(config == 'auto')
  {
    file.path("..", paste0(basename(getwd()),".yml"))
  } else
  {
    config
  }
  if(!is.null(config_file) && file.exists(config_file))
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
      base::assign(varnames[i], data, envir=dest)
    }
    return(invisible())
  }
  
  # Create an environment to house API_KEYS locally
  if(!exists("apiKeyStore", inherits=FALSE)) apiKeyStore <- new.env()
  
  # Was a keyring specified?
  if(!is.null(keyring))
  {
    state <- keyring::keyring_list()
    state <- state[state$keyring==keyring,]
    
    # If so, does it exist?
    if(nrow(state) == 1)
    {
      # Is it locked
      if(state$locked)
      {
        password <- passwordFUN(msg =
                                  paste0("Please enter password to unlock API keyring ",keyring, " "))
        keyring::keyring_unlock(keyring, password)
      }
    } else # Keyring does not exist
    {
      password <- passwordFUN(msg =
                                paste0("Creating keyring. Enter password for the API keyring ",
                                       keyring, " "))
      # Create keyring if it doesn't exist
      keyring::keyring_create(keyring, password)
    }
  }
  
  # For each dataset requested
  for(i in seq_along(connections))
  {
    # If the API_KEY doesn't exist go look for it
    
    # Does it exist in a secret keyring, use that
    if(!key_saved(apiKeyStore, connections[i]))
    {
      if(!is.null(keyring) &&
         keyring %in% (keyring::keyring_list()[,1]) &&
         connections[i] %in% keyring::key_list("redcapAPI", keyring)[,2])
      {
        apiKeyStore[[connections[i]]] <- keyring::key_get("redcapAPI", connections[i], keyring)
      }
    }
    # Check again if it's set properly
    if(!key_saved(apiKeyStore, connections[i]))
    {
      # Pull from knit with params if that exists
      if(exists("params") && !is.null(params[[connections[i]]]) && params[[connections[i]]] != "")
      {
        # Pull from Rmarkdown parameters
        apiKeyStore[[connections[i]]] <- params[[connections[i]]]
      } else # Ask the user for it
      {
        apiKeyStore[[connections[i]]] <- passwordFUN(msg=paste("Please enter RedCap API_KEY for", connections[i]))
      }
      
      if(!is.null(keyring))
      {
        keyring::key_set_with_value("redcapAPI", username=i, password=apiKeyStore[[connections[i]]], keyring=keyring)
      }
    }
    
    withCallingHandlers(
      { 
        data <- FUN(apiKeyStore[[connections[i]]], ...)
        base::assign(varnames[i], data, envir=dest)
      },
      error = function(e)
      {
        if(grepl("Could not resolve host", e))
        {
          rm(varnames[i], envir = apiKeyStore)
          if(!is.null(keyring)) keyring::key_delete("redcapAPI", connections[i], keyring)
        }
        stop(e)
      }
    )
  }
  
  return(invisible())
}


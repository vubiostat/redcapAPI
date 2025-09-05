# Copyright (C) 2021-2025 Vanderbilt University,
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
#' Connect to REDCap and verify connection
#'
#' A function that given an API_KEY and a url will create a `redcapConnection`
#' object and verify that it is working with a version call.
#' If the API key is invalid it will return NULL.
#' If the URL is invalid or there are multiple redirects it will call `stop`.
#'
#' @param key The API key used to connect.
#' @param url The url of the REDCap server.
#' @param ... Additional arguments passed to redcapConnection
#' @return redcapConnection established or NULL if key is invalid.
#' @seealso
#' [redcapConnection()]
#' @export
#' @examples
#' \dontrun{
#' connectAndCheck("<AN API KEY HERE>", "<REDCAP URL HERE>")
#' }
#' @importFrom utils browseURL
connectAndCheck <- function(key, url, ...)
{
  tryCatch(
    {
      rcon    <- redcapConnection(token=key, url=url, ...)
      version <- list(content = "version", format = "csv")
      # Test connection by checking version
      response <- makeApiCall(
        rcon,
        body = version,
        success_status_codes=c(200L, 301L, 302L),
        redirect=FALSE
      )

      mapped <- readBin(response$content, character())
      if(grepl("html", mapped))
      {
        x <- tempfile("htmlresponse.html")
        writeLines(mapped, x)
        browseURL(paste0("file://",x))
        logStop("Server URL responded with web page. Check URL.")
      }

      # No redirect, this is success
      if(!response$status_code %in% c(301L, 302L)) return(rcon)

      # Handle redirect
      rcon <- redcapConnection(
        token = key,
        url   = response$header$location,
        ...)

      # Test connection by checking version post redirect
      response <- makeApiCall(
        rcon,
        body = version,
        success_status_codes=c(200L, 301L, 302L),
        redirect=FALSE
      )

      mapped <- readBin(response$content, character())
      if(grepl("html", mapped))
      {
        x <- tempfile("htmlresponse.html")
        writeLines(mapped, x)
        browseURL(paste0("file://",x))
        logStop("Server URL responded with web page. Check URL.")
      }

      if(response$status_code %in% c(301L, 302L))
        logStop(paste("Too many redirects from", url))

      rcon
    },
    error = function(e)
    {
      if(grepl("Could not resolve host",     e)  ||
         grepl("Could not connect to server", e))
        logStop("Invalid URL provided '",url,"'. Unable to resolve or route.\n", e$message)

      if(grepl("405", e$message) )
        logStop("URL '",url,"' refused connection. Not acting like a REDCap server.\n", e$message)

      if(grepl("403", e)) return(NULL) # Forbidden, i.e. bad API_KEY

      logStop(e)
    }
  )
}

#' Open REDCap connections using cryptolocker for storage of API_KEYs.
#'
#' Opens a set of connections to REDcap from API_KEYs stored in an encrypted keyring.
#' If the keyring does not exist, it will ask for password to this keyring to use on
#' later requests. Next it
#' will ask for the API_KEYs specified in `connections`. If an API_KEY does not
#' work, it will request again. On later executions it will use an open keyring
#' to retrieve all API_KEYs or for a password if the keyring is currently
#' locked.
#'
#' If one forgets the password to this keyring, or wishes to start over:
#' `shelter::keyring_delete("<NAME_OF_KEY_RING_HERE>")`
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
#' For production servers the use of ENV variables is also supported. The connection
#' string is converted to upper case for the search of ENV. If a YAML
#' and ENV variable both exist, the YAML will take precedence.
#'
#' IMPORTANT: Make sure that R is set to NEVER save workspace to .RData
#' as this *is* writing the API_KEY to a local file in clear text because
#' connection objects contain the unlocked key in memory. Tips
#' are provided in `vignette("redcapAPI-best-practices")`.
#'
#' To debug an entire session via what is called / returned from the server,
#' add the argument `config=list(options=list(verbose=TRUE))` to the call.
#'
#' @param connections character vector. A list of strings that define the
#'          connections with associated API_KEYs to load into environment. Each
#'          name should correspond to a REDCap project for traceability, but
#'          it can be named anything one desires.
#'          The name in the returned list is this name.
#' @param envir environment. The target environment for the connections. Defaults to NULL
#'          which returns the keys as a list. Use [globalenv()] to assign in the
#'          global environment. Will accept a number such a '1' for global as well.
#' @param keyring character(1). Name of keyring.
#' @param url character(1). The url of one's institutional REDCap server api.
#' @param \dots Additional arguments passed to [redcapConnection()].
#' @return If `envir` is NULL returns a list of opened connections. Otherwise
#'         connections are assigned into the specified `envir`.
#'
#' @seealso
#' [redcapConnection()]
#'
#' ## Vignettes
#' `vignette("redcapAPI-best-practices")`, \cr
#' `vignette("redcapAPI-getting-started-connecting")`
#'
#' @examples
#' \dontrun{
#' unlockREDCap(c(test_conn    = 'TestRedcapAPI',
#'                sandbox_conn = 'SandboxAPI'),
#'              keyring      = '<NAME_OF_KEY_RING_HERE>',
#'              envir        = globalenv(),
#'              url          = 'https://<INSTITUTIONS_REDCAP_DOMAIN>/api/')
#' }
#' @export
#' @importFrom shelter unlockKeys
unlockREDCap    <- function(connections,
                            url,
                            keyring,
                            envir       = NULL,
                            ...)
{
   ###########################################################################
  # Check parameters passed to function
  coll <- checkmate::makeAssertCollection()

  if(is.numeric(envir)) envir <- as.environment(envir)

  checkmate::assert_character(x = url,          null.ok = FALSE, add = coll, len=1)
  checkmate::assert_character(x = keyring,      null.ok = FALSE, add = coll, len=1)
  checkmate::assert_character(x = connections,  null.ok = FALSE, add = coll)
  checkmate::assert_class(    x = envir,        null.ok = TRUE,  add = coll, classes="environment")
  checkmate::reportAssertions(coll)

   ###########################################################################
  ## Do it
  shelter::unlockKeys(connections,
             keyring,
             function(key, ...) connectAndCheck(key, url, ...),
             envir=envir,
             yaml_tag='redcapAPI',
             ...)
}

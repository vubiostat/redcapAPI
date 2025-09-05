# Copyright (C) 2025 Vanderbilt University,
# Shawn Garbett, Cole Beck
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

# Definition from splunk
.log_levels <- c(TRACE=0, DEBUG=1, INFO=2, WARN=3, ERROR=4)

# Returns the current log level numerically
.currentLogLevel <- function()
{
  level <- toupper(getOption('redcapAPI_log_level', 'INFO'))

  if(!level %in% names(.log_levels))
  {
    warning(paste("Undefined redcapAPI_log_level", level,
                  "\nDefaulting to INFO."))
    level <- 'INFO'
  }

  .log_levels[level]
}


  ###########################################################################
 ## Main Function
##
#' Log event
#'
#' This is one of the more complex integration of services into the `redcapAPI``
#' package. It's purpose is to provide the ability for a system administrator
#' (or user) to integrate logging into a report or application. The ability
#' to inject a logging framework without a developer's code being altered.
#'
#' To do this the callback function is pulled from the option `redcapAPI_logger`
#' which defaults to doing nothing.
#'
#' When the package starts up, it checks to see if SPLUNK_TOKEN and SPLUNK_URL
#' ENV variables are set and if so, it automatically redirects the
#' `redcapAPI_logger` to point at Splunk. It will also use SPLUNK_PROJECT
#' if defined, otherwise the project will be the directory name that the
#' code is executing from.
#'
#' There are also two helper functions `logWarning` and `logStop` which
#' will call logging if enabled first, then warn or stop as requested.
#'
#' @param severity `string` One of the following: 'TRACE', 'DEBUG', 'INFO',
#' 'WARN', or 'ERROR'
#' @param ... Information to include in the log event. Each argument must
#' have a name.
#'
#' @examples
#' \dontrun{
#'   options(redcapAPI_logger=function(severity, ...) {cat(severity, ' ', dput(list(...)), '\n')})
#'   logEvent("INFO", "This is a logged event")
#' }
#'
#' @export
#' @rdname logEvent
#' @importFrom checkmate makeAssertCollection assert_string assert_named
logEvent <- function(severity, ...)
{
  f <- getOption('redcapAPI_logger')
  if(is.null(f)) return(invisible(NULL))

  severity <- toupper(severity)

  coll <- checkmate::makeAssertCollection()

  checkmate::assert_string(severity,
    pattern="TRACE|DEBUG|INFO|WARN|ERROR",
    add=coll)

  checkmate::assert_named(list(...), add=coll)

  checkmate::reportAssertions(coll)

  f(severity, ...)
}

#' @importFrom jsonlite toJSON
#' @importFrom curl new_handle handle_setheaders handle_setopt handle_reset curl_fetch_memory
.splunkPost <- function(token, url, body)
{
  h    <- curl::new_handle()
  handle_setheaders(h, .list=c(
    "Authorization" = paste("Splunk", token),
    "Content-Type"  = "application/json"
  ))
  handle_setopt(h, postfields = toJSON(body, auto_unbox=TRUE))
  on.exit(curl::handle_reset(h), add = TRUE)
  resp <- tryCatch(
  {
    curl_fetch_memory(url, handle=h)
  },
  error=function(e)
  {
    if(grepl("Timeout was reached", e$message))
    {
      warning("Due to timeout disabling logging.") # FIXME: SHould this be a stop?
      options(redcapAPI_logger='')
      structure(
        list(
          status_code = 408L,
          content = charToRaw(e$message),
          headers = list('content-type' = "text/plain; charset=utf-8")
        ),
        class="response")
    } else
    {
      stop(e)
    }
  })
  if(resp$status_code != 200L)
    warning(paste("Splunk logging call failed", resp$status_code))
  invisible(resp)
}

# Creates a logger function for SPLUNK
# Our Test URL "https://splunkinput.app.vumc.org/services/collector"
.createSplunkFUN <- function(
  token   = Sys.getenv('SPLUNK_TOKEN'),
  url     = Sys.getenv('SPLUNK_URL'),
  project = Sys.getenv('SPLUNK_PROJECT'))
{
  if(token   == '') stop("Splunk token not set when creating logging function.")
  if(url     == '') stop("Splunk url not set when creating logging function.")
  if(project == '') project <- basename(getwd())

  function(level, ...)
  {
    if(.log_levels[level] <  .currentLogLevel()) return(invisible(NULL))

    # https://docs.splunk.com/Documentation/Splunk/latest/Data/FormateventsforHTTPEventCollector
    packet <- list(
      time       = Sys.time(),
      host       = unname(Sys.info()['nodename']),
      source     = project,
      sourcetype = 'redcapAPI',
      event      = list(severity=level,
                        ...)
    )
    .splunkPost(token, url, packet)
  }
}

.callStackEnvir <- function()
{
  vapply(seq_len(sys.nframe()),
         function(i) environmentName(environment(sys.function(i))), character(1))
}

.callFromPackage <- function(pkg)
{
  ix <- which(.callStackEnvir() == pkg)[1]
  if(is.na(ix)) NA else sys.call(ix)
}

#' @rdname logEvent
#' @export
logWarning <- function(...)
{
  logEvent("WARN", message=paste(...))
  warning(...)
}

#' @rdname logEvent
#' @export
logStop <- function(...)
{
  logStop("ERROR", message=paste(...))
  stop(...)
}

#' @rdname logEvent
#' @export
logMessage <- function(...)
{
  logMessage("INFO", message=paste(...))
  message(...)
}


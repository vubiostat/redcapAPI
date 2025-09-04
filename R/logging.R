
# Definition from splunk
log_levels <- c(TRACE=0, DEBUG=1, INFO=2, WARN=3, ERROR=4)

# Returns the current log level numerically
current_log_level <- function()
{
  level <- toupper(getOption('redcapAPI_log_level', 'INFO'))

  if(!level %in% names(log_levels))
  {
    warning(paste("Undefined redcapAPI_log_level", level,
                  "\nDefaulting to INFO."))
    level <- 'INFO'
  }

  log_levels[level]
}

# A default logger that does nothing
default_logger <- function(...) {invisible(NULL)}

# Execute the current logger function
current_logger <- function(...)
  getOption('redcapAPI_logger', default_logger)(...)

#' @importFrom rjson toJSON
#' @impmorFrom curl new_handle handle_setheaders handle_setopt handle_reset curl_fetch_memory
.splunkPost <- function(token, url, body)
{
  h    <- curl::new_handle()
  handle_setheaders(h, .list=c(
    "Authorization" = paste("Splunk", token),
    "Content-Type"  = "application/json"
  ))
  handle_setopt(h, postfields = toJSON(body))
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
splunk_logger_FUN <- function(
  token   = Sys.getenv('SPLUNK_TOKEN'),
  url     = Sys.getenv('SPLUNK_URL'),
  project = Sys.getenv('SPLUNK_PROJECT'))
{
  if(token   == '') stop("Splunk token not set when creating logging function.")
  if(url     == '') stop("Splunk url not set when creating logging function.")
  if(project == '') project <- basename(getwd())

  function(level, ...)
  {
    if(log_levels[toupper(level)] <  current_log_level()) return(invisible(NULL))

    # https://docs.splunk.com/Documentation/Splunk/latest/Data/FormateventsforHTTPEventCollector
    packet <- list(
      time       = Sys.time(),
      host       = unname(Sys.info()['nodename']),
      source     = project,
      sourcetype = 'redcapAPI',
      event      = list(severity=toupper(level),
                        ...)
    )
    .splunkPost(token, url, packet)
  }
}

.call_stack_environ <- function()
{
  vapply(seq_len(sys.nframe()),
         function(i) environmentName(environment(sys.function(i))), character(1))
}

.call_from_package <- function(pkg)
{
  ix <- which(.call_stack_environ() == pkg)[1]
  if(is.na(ix)) NA else sys.call(ix)
}


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
  resp <- curl_fetch_memory(url, handle=h)
  if(resp$status_code != 200L)
    warning(paste("Splunk logging call failed", response$status_code))
  invisible(resp)
}

# Creates a logger function for SPLUNK
# url <- "https://splunkinput.app.vumc.org/services/collector"
splunk_logger_FUN <- function(
    token=Sys.getenv('SPLUNK_TOKEN'),
    url=Sys.getenv("SPLUNK_URL"),
    project=basename(getwd()))
{
  if(token == '') stop("Splunk token not set when setting logging function.")
  if(url == '')   stop("Splunk url not set when setting logging function.")

  function(level, ...)
  {
    if(log_levels[toupper(level)] <  current_log_level()) return(invisible(NULL))

    browser()
    packet         <- list(...)
    packet$time    <- Sys.time()
    packet$level   <- level
    packet$project <- project

    .splunkPost(token, url, packet)
  }
}


# Definition from splunk
log_levels <- c(TRACE=0, DEBUG=1, INFO=2, WARN=3, ERROR=4)

# Returns the current log level numerically
current_log_level <- function()
{
  level <- toupper(getOption('redcapAPI_log_level', 'INFO'))

  if(!level %in% names(log_levels))
  {
    warning(paste("Undefined redcapAPI_log_level", level, "\nDefaulting to INFO."))
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
splunk_request = function(token, url, packet)
{
  config <- list(url = url,
                 Authorization = paste("Splunk", token))
  response <- .curlPost(body = toJSON(packet), config = config)
  if(!response$status_code %in% success_status_codes)
    warning(paste("Splunk logging call failed with ", response$status_code))

  invisible(response)
}

# Creates a logger function for SPLUNK
# url <- "https://splunkinput.app.vumc.org/services/collector"
splunk_logger_FUN <- function(token, url, project)
{
  function(level, ...)
  {
    if(log_levels[toupper(level)] <  current_log_level()) return(invisible(NULL))

    packet              <- list(...)
    packet$time         <- Sys.time()
    packet$type         <- level  # Shouldn't this be "level"
    packet$project_type <- project

    splunk_request(token, url, packet)
  }
}

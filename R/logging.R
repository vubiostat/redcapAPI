
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
      warning("Due to timeout disabling logging.")
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
    warning(paste("Splunk logging call failed", response$status_code))
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

    browser()
    packet         <- list(...)
    packet$time    <- Sys.time()
    packet$level   <- level
    packet$project <- project

    .splunkPost(token, url, packet)
  }
}

.find_first_exported_call_from_package <- function(pkg)
{
  # Get the list of exported objects (symbols) from the package
  exported <- getNamespaceExports(pkg)
  ns_env   <- asNamespace(pkg)

  # Loop over call stack
  for (i in seq_along(sys.calls()))
  {
    call <- sys.call(i)
    fun  <- sys.function(i)

    # Try to find the name of the function if it's named
    if (is.symbol(call[[1]]) || is.call(call[[1]]))
    {
      fun_name <- deparse(call[[1]])

      # Try to find the object in the package namespace
      if (exists(fun_name, envir = ns_env, inherits = FALSE))
      {
        exported_fun <- get(fun_name, envir = ns_env)

        # Check if it's exported and ident
        if (fun_name %in% exported && identical(fun, exported_fun)) return(call)
      }
    }
  }

  return(NULL)
}

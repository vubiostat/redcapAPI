  ############################################################################
 #
#  redcapError
#
#  The current intent of this is to consistently deal
#  with API errors, and possibly translate them into
#  a meaningful message for users.
# 
#  There is a legacy intent could potentially be deprecated
#  in the future for supporting "Classic", i.e. version < 6.0.0,
#  REDCap projects. The interface for this ignores
#  certain errors by default which would occur when interacting
#  with these older projects. At present it is doubtful
#  the current framework would work with a project of this age.
#
#  Part of this legacy support also involved an option
#  'redcap_error_handler' which if set to anything but
#  NULL it will error on all cases. 
redcapError <- function(x)
{
  error_handling <- getOption("redcap_error_handling")
  error_message  <- as.character(x)
  
  # Legacy project ignore errors unless option given to treat as errors
  handle <- c("ERROR: The value of the parameter \"content\" is not valid",
              "ERROR: You cannot export arms for classic projects",
              "ERROR: You cannot export events for classic projects",
              "ERROR: You cannot export form/event mappings for classic projects")
  if (error_message %in% handle && is.null(error_handling)) return(NULL)
  
  # Translate network error codes to user relevant message
  if (grepl("Connection reset by peer", error_message) || 
      grepl("Timeout was reached", error_message))
    stop(paste0(.RESET_BY_PEER, ": ", error_message))
  
  # Otherwise just stop with code
  stop(paste0(x$status_code, ": ", error_message))
}

.RESET_BY_PEER <- "A network error has occurred. This can happen when too much data is requested causing a timeout (consider batching), or when REDCap is having trouble servicing requests. It may also be a misconfigured proxy or network routing congestion."

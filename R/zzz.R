.onLoad <- function(libname,pkgname)
{
  # Set the time out to 5 minutes (300 seconds) 
  # If a setting had already existed for timeout_ms, restore it.  
  # We don't want to disrupt the user's settings
  old <- httr::set_config(httr::timeout(300))
  if (!is.null(old$options$timeout_ms) && old$options$timeout_ms != 3e+05){
    httr::set_config(old)
  }
  
  options(redcap_api_url = character(0),
          redcap_error_handling = "null"
  )
}

.onUnload <- function(libPath)
{
  options(redcap_api_url = NULL,
          redcap_error_handling = NULL)
}

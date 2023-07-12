packageStartupMessage(
  "Welcome to redcapAPI.  Please Note:\n",
  " - Unless previously set, the http request has been set to time out after 5 minutes.\n",
  " - 'exportBundle' has been made redundant. See ?redcapConnection for details about caching project data.")

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
          redcap_error_handling = "null",
          redcap_bundle = 
            structure(
              list(
                version = NULL,
                meta_data = NULL,
                users = NULL,
                instruments = NULL,
                events = NULL,
                arms = NULL,
                mappings = NULL
              ),
              class = c("redcapBundle", "redcapProject", "list")
            )
  )
}

.onUnload <- function(libPath)
{
  options(redcap_api_url = NULL,
          redcap_error_handling = NULL,
          redcap_bundle = NULL)
}

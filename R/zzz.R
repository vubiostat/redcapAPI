packageStartupMessage(
  "A future release of redcapAPI 3.0.0 will introduce several breaking changes \n",
  "* The `exportRecords` function interface will be replaced. Please switch your processes to `exportRecordsTyped`\n",
  "* The `exportReports` function interface will be replaced. Please switch your processes to `exportReportsTyped`\n",
  "* The `importRecords` function interface will be replaced to utilize `castForImport` to prepare data for import.\n ",
  "* The `redcapFactor` class is being discontinued with all its supporting methods (including `redcapFactorFlip`).\n",
  "* The `exportProjectInfo` and `exportBundle` functions are being discontinued. Their functionality is replaced by caching values on the connection object.\n", 
  "See NEWS for more details."
)

.onLoad <- function(libname,pkgname)
{
  # Set the time out to five minutes (300 seconds) 
  # If a setting had already existed for timeout_ms, restore it.  
  # We do not want to disrupt the user's settings
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

packageStartupMessage(
  "Welcome to redcapAPI.  Please Note:\n",
  " - 'exportBundle' has been made redundant. See ?redcapConnection for details about caching project data.")

.onLoad <- function(libname,pkgname)
{
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

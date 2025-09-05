packageStartupMessage(
  "A future release of redcapAPI 3.0.0 will introduce several breaking changes \n",
  "* The `exportRecords` function interface is deprecated. Please switch your processes to `exportRecordsTyped`\n",
  "* The `exportReports` function interface is deprecated. Please switch your processes to `exportReportsTyped`\n",
  "* The `importRecords` function interface will be changed to utilize `castForImport` to prepare data for import.\n"
)

.onLoad <- function(libname, pkgname)
{
  # Set log level if set in ENV
  if(is.null(getOption('redcapAPI_log_level')))
    options(redcapAPI_log_level=Sys.getenv('REDCAPAPI_LOG_LEVEL', 'INFO'))

  # If SPLUNK_TOKEN is set in the ENV, setup splunk, but
  # only if the redcapAPI_logger function hasn't been already set
  if(Sys.getenv('SPLUNK_TOKEN') != '')
  {
    if(Sys.getenv('SPLUNK_URL') == '')
    {
      warning("SPLUNK_TOKEN is set but SPLUNK_URL is not. Unable to turn on Splunk logging.")
    } else if(is.null(getOption('redcapAPI_logger')))
    {
      options(redcapAPI_logger = .createSplunkFUN())
    }
  }
}

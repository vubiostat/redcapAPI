packageStartupMessage(
  "A future release of redcapAPI 3.0.0 will introduce several breaking changes \n",
  "* The `exportRecords` function interface is deprecated. Please switch your processes to `exportRecordsTyped`\n",
  "* The `importRecords` function interface will be changed to utilize `castForImport` to prepare data for import.\n"
)

.onLoad <- function(libname, pkgname)
{
  if(Sys.getenv('SPLUNK_TOKEN') == '') return(NULL)

  if(Sys.getenv('SPLUNK_URL') == '')
  {
    warning("SPLUNK_TOKEN is set but SPLUNK_URL is not. Unable to turn on Splunk logging.")
    return(NULL)
  }

  if(is.null(getOption('redcapAPI_logger')))
    options(redcapAPI_logger = .createSplunkFUN())
}

packageStartupMessage(
  "A future release of redcapAPI 3.0.0 will introduce several breaking changes \n",
  "* The `exportRecords` function interface is deprecated. Please switch your processes to `exportRecordsTyped`\n",
  "* The `exportReports` function interface is deprecated. Please switch your processes to `exportReportsTyped`\n",
  "* The `importRecords` function interface will be changed to utilize `castForImport` to prepare data for import.\n",
  "* The `exportProjectInfo` and `exportBundle` functions are deprecated. Their functionality is replaced by cached values on the connection object.\n"
)
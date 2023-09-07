#' @name recordsMethods
#' @title Export Records and Reports
#' 
#' @description These methods enable the user to export records and reports
#'   from a project.
#'   
#' @inheritParams common-rcon-arg
#' @inheritParams common-dot-args
#' @inheritParams common-api-args
#' @param report_id `integerish(1)`. Gives the report id of the desired report. 
#'   This is located on the Report Builder page of the user interface.
#' @param records `character` or `NULL`. Vector of record IDs to export.
#' @param fields `character` or `NULL`. Vector of fields to export. 
#' @param forms `character` or `NULL`. Vector of forms to export.
#' @param events `character` or `NULL`. Vector of events to export.
#' @param raw_or_label `character(1)`. One of `c("raw", "label")`. 
#'   Export the raw coded values or labels for the options of multiple choice fields
#' @param raw_or_label_headers `character(1)`. One of `c("raw", "label")`. 
#'   Export the variable field names (`"raw"`) or the labels (`"label"`).
#' @param export_checkbox_label `logical(1)`. Specifies the format of 
#'   checkbox field values specifically when exporting the data as labels 
#'   (i.e., when `rawOrLabel = "label"`). When exporting labels, by 
#'   default (`FALSE`), all checkboxes will either have a value 
#'   'Checked' if they are checked or 'Unchecked' if not checked. 
#'   But if `TRUE`, it will instead export the checkbox value as the 
#'   checkbox option's label (e.g., 'Choice 1') if checked or it will be 
#'   blank/empty (no value) if not checked.
#' @param export_survey_fields `logical(1)`. When `TRUE`, the survey identifier
#'   field (`redcap_survey_identifier`) and survey timestamp fields 
#'   (`[instrument]_timestamp`) will be exported.
#' @param export_dags `logical(1)`. When `TRUE`, the Data Access Group 
#'   identifier `redcap_data_access_group` will be exported.
#' @param csv_delimiter `character`. One of 
#'   `c(",", "\t", ";", "|", "^")`. Designates the delimiter for the CSV
#'   file received from the API.
#' @param batch_size `integerish(1)` (or `NULL`). When `NULL`,
#'   all records are pulled. Otherwise, the records are pulled in batches of this size.
#'   
#' @details
#' It is unnecessary to include `"redcap_event_name"`` in the fields argument.  
#' This field is automatically exported for any longitudinal database.  
#' If the user does include it in the fields argument, it is removed quietly 
#' in the parameter checks.
#' 
#' There are four ways the data from checkbox variables may be 
#' represented depending on the values of `factors` and 
#' `checkboxLabels`. The most common are the first and third 
#' rows of the table below.  When `checkboxLabels = TRUE`, either 
#' the coded value or the labeled value is returned if the box is 
#' checked, or an empty string if it is not.
#' 
#' | `factors`    | `checkboxLabels`      | `Output`            |
#' |--------------|-----------------------|---------------------|
#' | `FALSE`      | `FALSE`               | 0 / 1               |
#' | `FALSE`      | `TRUE`                | "" / code           |
#' | `TRUE`       | `FALSE`               | Unchecked / Checked | 
#' | `TRUE`       | `TRUE`                | "" / label          |
#' 
#' The 'offline' version of `exportReports` operates on the raw (unlabeled) data 
#' file downloaded from REDCap along with the data dictionary.  
#' This is made available for instances where the API cannot be accessed for 
#' some reason (such as waiting for API approval from the REDCap administrator).
#' 
#' A 'batched' export (or import) is one where the export is performed over a series of 
#' API calls rather than one large call.  For large projects on small servers, 
#' this may prevent a single user from tying up the server and forcing others 
#' to wait on a larger job.  The batched export is performed by first 
#' calling the API to export the record identifier field (the first field
#' in the meta data).  The unique ID's are then assigned a batch number with 
#' no more than `batch.size` ID's in any single batch.  The batches are 
#' exported from the API and stacked together.
#' 
#' In longitudinal projects, `batch.size` may not necessarily be the 
#' number of records exported in each batch.  If `batch.size` is ten and 
#' there are four records per patient, each batch will consist of 40 records.  
#' Thus, if the user is concerned about tying up the server with a large, 
#' longitudinal project, it would be prudent to use a smaller batch size.
#' 
#' 
#' 
#' @return 
#' `exportRecords` returns a data frame with the project data. The 
#'   data will be formatted consistent with the meta data and the arguments
#'   provided by the user. 
#'   
#' `exportReports` returns a data frame with the data from the requested
#'   report. The data will be formatted consisted with the meta data and
#'   the arguments provided by the user.
#'   
#'   
#' @seealso 
#' [exportRecordsTyped()], \cr
#' [exportReportsTyped()], \cr
#' [importRecords()], \cr
#' [deleteRecords()], \cr
#' [exportNextRecordName()], \cr
#' [renameRecord()]
#' 
#' 
#' @examples
#' \dontrun{
#' unlockREDCap(connections = c(rcon = "project_alias"), 
#'              url = "your_redcap_url", 
#'              keyring = "API_KEYs", 
#'              envir = globalenv())
#'              
#' # Export records
#' exportRecords(rcon)
#' 
#' # Export records in batches of one hundred IDs
#' exportRecords(rcon, 
#'               batch.size = 100)
#'               
#' # Export records without factors
#' exportRecords(rcon, 
#'               factors = FALSE)
#'               
#' 
#' # Export a report
#' exportReports(rcon, 
#'               report_id = 12345)
#' 
#' }
#' 
#' 
#' @usage NULL
#' @order 0

recordsMethods <- function(rcon,
                           report_id,
                           records,
                           fields, 
                           forms, 
                           events, 
                           raw_or_label, 
                           raw_or_label_headers, 
                           export_checkbox_label, 
                           export_survey_fields, 
                           export_dags, 
                           batch_size,
                           csv_delimiter,
                           ..., 
                           error_handling, 
                           config, 
                           api_param){
  NULL
}

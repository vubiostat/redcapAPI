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
#' @param factors `logical(1)`.  When `TRUE`, multiple choice fields
#'   will be returned as factors. Otherwise, they are returned as 
#'   character values. See 'Exporting Records'
#'   for more on how this interacts with the `checkboxLabels` argument.
#' @param fields `character`. Fields to be returned.  When `NULL`, 
#'   all fields are returned.
#' @param forms `character`. Forms to be returned.  When `NULL`, 
#'   all forms are returned.
#' @param records `character` or `integerish`. Record ID's to be 
#'   returned.  When `NULL`, all records are returned.
#' @param events `character`. Events to be returned from a 
#'   longitudinal database.  When `NULL`, all events are returned.
#' @param labels `logical(1)`. When `TRUE`, field labels are 
#'   attached to each column as an attribute.
#' @param dates `logical(1)`. When `TRUE`, date variables are converted to
#'   `POSIXct` objects.
#' @param drop `character`. An optional vector of REDCap field names to 
#'   remove from the dataset. Ignored when `NULL`. Any fields in this 
#'   argument that do not exist in the project will be ignored.
#' @param survey `logical(1)`. specifies whether or not to export the survey identifier field 
#'   (`redcap_survey_identifier`) or survey timestamp fields 
#'   (`[form_name]_timestamp`) when surveys are utilized in the project. 
#' @param dag `logical(1)`. When `TRUE`, the system field 
#'   `redcap_data_access_group` is included in the export. 
#'   This option is only 
#'   viable if the user whose token is being used to make the API request is 
#'   not in a data access group. If the user is in a group, then this 
#'   flag will revert to `FALSE`.
#' @param checkboxLabels `logical(1)`. When `FALSE` labels are 
#'   applied as "Unchecked"/"Checked".  
#'   When `TRUE`, they are applied as `""/[field_label]` where `[field_label]` 
#'   is the label assigned to the level in the data dictionary. 
#' @param form_complete_auto `logical(1)`. When `TRUE` 
#'   (default), the `[form]_complete` fields for any form 
#'   from which at least one variable is requested will automatically
#'   be retrieved.  When `FALSE`, these fields must be 
#'   explicitly requested.   
#' @param colClasses Named `character` vector. Column classes passed to 
#'   [utils::read.csv()] calls. 
#'   Useful to force the interpretation of a column in a specific type and 
#'   avoid an unexpected recast.
#' @param batch.size `integerish(1)`.  Specifies the number of subjects to be included 
#'   in each batch of a batched export or import.  Non-positive numbers 
#'   export/import the entire operation in a single batch. 
#'   Batching may be beneficial to prevent tying up smaller servers.  
#'   See Details.
#' @param dataFile `character(1)`. Gives the location
#'   of the dataset downloaded from REDCap.  This should be the raw
#'   (unlabeled) data set.
#' @param metaDataFile `character(1)`. Gives the location of the data dictionary 
#'   downloaded from REDCap.
#' @param meta_data Deprecated version of `metaDataFile`.
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
#' # Export raw data
#' exportRecordsTyped(rcon, 
#'                    validation = skip_validation,
#'                    cast = raw_cast)
#' 
#' }
#' 
#' 
#' @usage NULL
#' @order 0

recordsMethods <- function(rcon,
                           report_id,
                           factors, 
                           fields, 
                           forms, 
                           records, 
                           events, 
                           labels, 
                           dates, 
                           drop, 
                           survey, 
                           dag, 
                           checkboxLabels, 
                           form_complete_auto,
                           colClasses,
                           batch.size,
                           dataFile, 
                           metaDataFile,
                           ...)
{
  NULL
}

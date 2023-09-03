#' @name recordsMethods
#' @title Export, Import, and Delete Records
#' 
#' @description These methods enable the user to export, import, or delete 
#'   records from the project. Reports may also be exported with an 
#'   interface similar to \code{exportRecords}.
#'   
#' @inheritParams common-rcon-arg
#' @inheritParams common-dot-args
#' @inheritParams common-api-args
#' @param report_id \code{integerish(1)}. Gives the report id of the desired report. 
#'   This is located on the Report Builder page of the user interface.
#' @param factors \code{logical(1)}.  When \code{TRUE}, multiple choice fields
#'   will be returned as factors. Otherwise, they are returned as 
#'   character values. See 'Exporting Records'
#'   for more on how this interacts with the \code{checkboxLabels} argument.
#' @param fields \code{character}. Fields to be returned.  When \code{NULL}, 
#'   all fields are returned.
#' @param forms \code{character}. Forms to be returned.  When \code{NULL}, 
#'   all forms are returned.
#' @param records \code{character} or \code{integerish}. Record ID's to be 
#'   returned.  When \code{NULL}, all records are returned.
#' @param events \code{character}. Events to be returned from a 
#'   longitudinal database.  When \code{NULL}, all events are returned.
#' @param labels \code{logical(1)}. When \code{TRUE}, field labels are 
#'   attached to each column as an attribute.
#' @param dates \code{logical(1)}. When \code{TRUE}, date variables are converted
#'   \code{POSIXct} objects.
#' @param drop \code{character}. An optional vector of REDCap field names to 
#'   remove from the dataset. Ignored when \code{NULL}. Any fields in this 
#'   argument that do not exist in the project will be ignored.
#' @param survey \code{logical(1)}. specifies whether or not to export the survey identifier field 
#'   (\code{redcap_survey_identifier}) or survey timestamp fields 
#'   (\code{[form_name]_timestamp}) when surveys are utilized in the project. 
#' @param dag \code{logical(1)}. When \code{TRUE}, the system field 
#'   \code{redcap_data_access_group} is included in the export. 
#'   This option is only 
#'   viable if the user whose token is being used to make the API request is 
#'   not in a data access group. If the user is in a group, then this 
#'   flag will revert to \code{FALSE}.
#' @param checkboxLabels \code{logical(1)}. When \code{FALSE} labels are 
#'   applied as "Unchecked"/"Checked".  
#'   When \code{TRUE}, they are applied as ""/"[field_label]" where [field_label] 
#'   is the label assigned to the level in the data dictionary. 
#' @param form_complete_auto \code{logical(1)}. When \code{TRUE} 
#'   (default), the \code{[form]_complete} fields for any form 
#'   from which at least one variable is requested will automatically
#'   be retrieved.  When \code{FALSE}, these fields must be 
#'   explicitly requested.   
#' @param colClasses Named \code{character} vector. Column classes passed to 
#'   \code{\link[utils]{read.csv}} calls. 
#'   Useful to force the interpretation of a column in a specific type and 
#'   avoid an unexpected recast.
#' @param data A \code{data.frame} to be imported to the project.
#' @param arm \code{integerish}. the arm number of the arm in which the 
#'   record(s) should be deleted. This can only be used if the project is 
#'   longitudinal with more than one arm. If the arm parameter is not 
#'   provided, the specified records will be deleted from all arms in which 
#'   they exist. Whereas, if \code{arm} is provided, they will only be deleted from 
#'   the specified arm.  
#' @param overwriteBehavior \code{character(1)}. One of \code{c("normal", "overwrite")}. 
#'   \code{"normal"} prevents blank fields from overwriting populated fields.  
#'   \code{"overwrite"} causes blanks to overwrite data in the database.
#' @param returnContent \code{character(1)}.  
#'   One of \code{c("count", "ids", "nothing", "auto_ids")}.
#'   'count' returns the number of records imported; 
#'   'ids' returns the record ids that are imported;
#'   'nothing' returns no message; 
#'   'auto_ids' returns a list of pairs of all record IDs that were imported. 
#'   If used when \code{force_auto_number = FALSE}, the value will be changed to \code{'ids'}.
#' @param returnData \code{logical(1)}. When \code{TRUE}, prevents the REDCap 
#'   import and instead returns the data frame that would have been given
#'   for import. This is sometimes helpful if the API import fails without
#'   providing an informative message. The data frame can be written to a csv
#'   and uploaded using the interactive tools to troubleshoot the
#'   problem. 
#' @param logfile \code{character(1)}. An optional filepath (preferably .txt) 
#'   in which to print the log of errors and warnings about the data.
#'   When \code{""}, the log is printed to the console. 
#' @param batch.size \code{integerish(1)}.  Specifies the number of subjects to be included 
#'   in each batch of a batched export or import.  Non-positive numbers 
#'   export/import the entire operation in a single batch. 
#'   Batching may be beneficial to prevent tying up smaller servers.  
#'   See Details.
#' @param dataFile \code{character(1)}. Gives the location
#'   of the dataset downloaded from REDCap.  This should be the raw
#'   (unlabeled) data set.
#' @param metaDataFile \code{character(1)}. Gives the location of the data dictionary 
#'   downloaded from REDCap.
#'   
#' @details
#' 
#' A 'batched' export (or import) is one where the export is performed over a series of 
#' API calls rather than one large call.  For large projects on small servers, 
#' this may prevent a single user from tying up the server and forcing others 
#' to wait on a larger job.  The batched export is performed by first 
#' calling the API to export the record identifier field (the first field
#' in the meta data).  The unique ID's are then assigned a batch number with 
#' no more than \code{batch.size} ID's in any single batch.  The batches are 
#' exported from the API and stacked together.
#' 
#' In longitudinal projects, \code{batch.size} may not necessarily be the 
#' number of records exported in each batch.  If \code{batch.size} is 10 and 
#' there are four records per patient, each batch will consist of 40 records.  
#' Thus, if you are concerned about tying up the server with a large, 
#' longitudinal project, it would be prudent to use a smaller batch size.
#' 
#' ## Exporting Records:
#' 
#' ' The 'offline' version of \code{exportReports} operates on the raw (unlabeled) data 
#' file downloaded from REDCap along with the data dictionary.  
#' This is made available for instances where the API can not be accessed for 
#' some reason (such as waiting for API approval from the REDCap administrator).
#' 
#' It is unnecessary to include "redcap_event_name" in the fields argument.  
#' This field is automatically exported for any longitudinal database.  
#' If the user does include it in the fields argument, it is removed quietly 
#' in the parameter checks.
#' 
#' There are four ways the data from checkbox variables may be 
#' represented depending on the values of \code{factors} and 
#' \code{checkboxLabels}. The most common are the first and third 
#' rows of the table below.  When \code{checkboxLabels = TRUE}, either 
#' the coded value or the labelled value is returned if the box is 
#' checked, or an empty string if it is not.
#' 
#' \tabular{lll}{
#' \code{factors} \tab \code{checkboxLabels} \tab Output \cr
#' \code{FALSE}   \tab \code{FALSE}          \tab 0 / 1 \cr
#' \code{FALSE}   \tab \code{TRUE}           \tab "" / value \cr
#' \code{TRUE}    \tab \code{FALSE}          \tab Unchecked / Checked \cr
#' \code{TRUE}    \tab \code{TRUE}           \tab "" / label 
#' }
#' 
#' 
#' 
#' ## Importing Records: 
#' 
#' \code{importRecords} prevents the most common import errors by testing the
#' data before attempting the import.  Namely
#' \enumerate{
#'   \item Check that all variables in \code{data} exist in the REDCap data dictionary.
#'   \item Check that the study id variable exists
#'   \item Force the study id variable to the first position in the data frame (with a warning)
#'   \item Remove calculated fields (with a warning)
#'   \item Verify that REDCap date fields are represented in the data frame as
#'     either character, POSIXct, or Date class objects.
#'   \item Determine if values are within their specified validation limits.
#' }
#'
#' See the documentation for \code{\link{validateImport}} for detailed
#' explanations of the validation.
#' 
#' @return 
#' \code{exportRecords} returns a data frame with the project data. The 
#'   data will be formatted consistent with the meta data and the arguments
#'   provided by the user. 
#'   
#' \code{exportReports} returns a data frame with the data from the requested
#'   report. The data will be formatted consisted with the meta data and
#'   the arguments provided by the user.
#'   
#' \code{importRecords}, when \code{returnData = FALSE}, returns the content from the
#'   API response designated by the \code{returnContent} argument. 
#'   
#' \code{importRecords}, when \code{returnData = TRUE}, returns the 
#'   data frame that was internally prepared for import. This data frame has
#'   values transformed from R objects to character values the API will 
#'   accept. 
#' 
#' \code{deleteRecords} returns a character value giving the number of 
#'   records deleted.
#'   
#'   
#' @seealso 
#' \code{\link{exportRecordsTyped}}, \cr
#' \code{\link{exportReportsTyped}}, \cr
#' \code{\link{exportNextRecordName}}, \cr
#' \code{\link{renameRecord}}
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
#' 
#' # Import records
#' NewData <- data.frame(record_id = c(1, 2, 3), 
#'                       age = c(27, 43, 32), 
#'                       date_of_visit = rep(Sys.Date(), 3))
#' importRecords(rcon, 
#'               data = NewData)
#'               
#'               
#' # Import records and save validation info to a file
#' NewData <- data.frame(record_id = c(1, 2, 3), 
#'                       age = c(27, 43, 32), 
#'                       date_of_visit = rep(Sys.Date(), 3))
#' importRecords(rcon, 
#'               data = NewData, 
#'               logfile = "import-validation-notes.txt")      
#' 
#' 
#' # Delete records
#' deleteRecords(rcon, 
#'               records = c("1", "2"))
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
                           form_auto_complete,
                           colClasses,
                           data, 
                           arm, 
                           overwriteBehavior, 
                           returnContent, 
                           returnData, 
                           logfile, 
                           batch.size,
                           dataFile, 
                           metaDataFile,
                           ..., 
                           error_handling, 
                           config, 
                           api_param){
  NULL
}

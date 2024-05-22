#' @name recordsTypedMethods
#' @title Export Records or Reports From a Project
#' 
#' @description These methods enable the user to export records from a 
#'   database or from a report. These methods have more control for casting
#'   fields to R objects than `exportRecords`. 
#'   
#' @inheritParams common-rcon-arg
#' @inheritParams common-dot-args
#' @inheritParams common-api-args
#' @inheritParams common-cast-args
#' @param report_id `integerish(1)`. The ID number of the report to be exported. 
#' @param fields `character` or `NULL`. Vector of fields to be returned.  If `NULL`, 
#'   all fields are returned (unless `forms` is specified).
#' @param drop_fields `character` or `NULL`. A vector of field names to remove from 
#'   the export. 
#' @param forms `character` or `NULL`. Vector of forms to be returned.  If `NULL`, 
#'   all forms are returned (unless `fields` is specified.
#' @param records `character` or `integerish`. A vector of study ID's 
#'   to be returned.  If `NULL`, all subjects are returned.
#' @param events A `character` vector of events to be returned from a 
#'   longitudinal database.  If `NULL`, all events are returned. When 
#'   using a `redcapOfflineConnection` object, this argument is unvalidated, 
#'   and only rows that match one of the values given are returned; 
#'   misspellings may result in unexpected results.
#' @param survey `logical(1)`. When `TRUE`, the survey identifier field 
#'   (e.g., `redcap_survey_identifier`) and survey timestamp fields 
#'   (e.g., `[form_name]_timestamp`) will be exported 
#'   (relevant only when surveys are utilized in the project). If these
#'   fields are specified in the fields argument and this flag is set to 
#'   FALSE the requested fields will not be exported.
#' @param dag `logical(1)`. When `TRUE` the 
#'   `redcap_data_access_group` field will be included in the export \
#'   when data access groups are utilized in the project. 
#'   This flag is only viable if the user whose token is 
#'   being used to make the API request is not in a data access group. 
#'   If the user is in a group, then this flag will revert to its default value.
#'   Data Access Groups privilege is required when creating/renaming/deleting
#'   DAGs and when importing/exporting user-DAG assignments. Therefore, the 
#'   default for this flag is FALSE. To export DAG information set this flag to TRUE.
#' @param date_begin `POSIXct(1)` or `NULL`. Ignored if `NULL` (default). 
#'   Otherwise, records created or modified after this date will be returned.
#' @param date_end `POSIXct(1)` or `NULL`. Ignored if `NULL` (default). 
#'   Otherwise, records created or modified before this date will be returned.
#' @param csv_delimiter `character`. One of `c(",", "\t", ";", "|", "^")`. Designates the
#'   delimiter for the CSV file received from the API.
#' @param batch_size `integerish(1)` or `NULL`. When `NULL`,
#'   all records are pulled. Otherwise, the records all pulled in batches of this size.
#' @param filter_empty_rows `logical(1)`. Filter out empty rows post retrieval. 
#'   Defaults to `TRUE`.
#' @param warn_zero_coded `logical(1)`. Turn on or off warnings about
#'   potentially problematic zero coded fields. Defaults to `TRUE`. 
#' 
#' @details
#' 
#' The 'offline' method operates on the raw (unlabeled) data 
#' file downloaded from REDCap along with the data dictionary.  
#' This is made available for instances where the API cannot be accessed for 
#' some reason (such as waiting for API approval from the REDCap administrator).
#' 
#' When validating data for `offlineRedcapConnection` objects, 
#' links to invalid data forms will not work if the user does not provide
#' the `url`, `version`, `project_info`, and `events` arguments (if the 
#' project is longitudinal). For the `project_info`, the values `project_id`
#' and `is_longitudinal` are required. The user may be able to provide
#' as little as `project_info = data.frame(project_id = [id], is_longitudinal = [0/1])`.
#' The user should be aware that the REDCap User Interface download for 
#' events does not include the event ID. To include the event ID, the user
#' must construct a data frame to pass to `offlineConnection`.
#' 
#' ## Record Identifier (System) Fields 
#' 
#' In all calls, the project's ID fields will be included--there is no option
#' provided to prevent this. Additionally, if the project has a secondary
#' unique field specified, it will also be included. Inclusion of these fields
#' is necessary to support some post-processing functions. 
#' 
#' By default, the system fields `redcap_event_name`, 
#' `redcap_repeat_instrument`, and `redcap_repeat_instance` are 
#' exported (when they are appropriate to the project). These are automatically
#' included by the API. However, if the user omits any of these in `fields`
#' or designates one in `drop_fields`, the final result will honor those 
#' conditions. Excluding any of these identifiers may cause problems with 
#' some post-processing functions that operate on repeating instrument data.
#' 
#' The combination of the project ID field, secondary unique field, and the
#' system fields are what uniquely identify an experimental unit. In nearly 
#' all cases, it is desirable to have them all included.
#' 
#' System fields are cast to labelled values by default. They may be cast 
#' to their coded values using the override `cast = list(system = castRaw)`.
#' The fields affected by the `system` override are `redcap_event_name`, 
#' `redcap_repeat_instrument`, and `redcap_data_access_group`.
#' 
#' ## BioPortal Fields
#' 
#' Text fields that are validation enabled using the BioPortal Ontology service
#' may be cast to labeled values so long as the labels have been cached on the
#' REDCap server. Caching is performed when the field is viewed in a form on 
#' the web interface. However, labels are not cached when data are imported 
#' via the API. In cases where labels are not cached, the coded value is 
#' treated as both the code and the label.
#' 
#' ## Record Batching
#' 
#' A 'batched' export is one where the export is performed over a series of 
#' API calls rather than one large call.  For large projects on small servers, 
#' this may prevent a single user from tying up the server and forcing others 
#' to wait on a larger job.  The batched export is performed by first 
#' calling the API to export the subject identifier field (the first field
#' in the meta data).  The unique ID's are then assigned a batch number with 
#' no more than `batch_size` ID's in any single batch.  The batches are 
#' exported from the API and stacked together.
#' 
#' In longitudinal projects, `batch_size` may not necessarily be the 
#' number of records exported in each batch.  If `batch_size` is ten and 
#' there are four records per patient, each batch will consist of 40 records.  
#' Thus, if the user is concerned about tying up the server with a large, 
#' longitudinal project, it would be prudent to use a smaller batch size.
#' 
#' ## Inversion of Control
#' 
#' The final product of calling this is a `data.frame` with columns
#' that have been type cast to most commonly used analysis class (e.g. factor).
#' This version allows the user to override any step of this process by
#' specifying a different function for each of the stages of the type casting.
#' The algorithm is as follows:
#' 
#' 1. Detect NAs in returned data (`na` argument).
#' 2. Run `validate` functions for the field_types.
#' 3. On the fields that are not NA and pass validate do the specified cast.
#' 
#' It is expected that the `na` and `validate` overrides should
#' rarely be used. Their exposure via the function parameters is to future
#' proof against possible bugs in the defaults, and allows for things that
#' higher versions of REDCap add as possible field types. I.e., the overrides
#' are for use to continue using the library when errors or changes to REDCap
#' occur. 
#' 
#' The cast override is one where users can specify things that were controlled
#' by an ever increasing set of flags before. E.g., `dates=as.Date` was
#' an addition to allow dates in the previous version to be overridden if the 
#' user wanted to use the Date class. In this version, it would appear called
#' as `cast=list(_date=as.Date))`. See [fieldValidationAndCasting()]
#' for a full listing of package provided cast functions. 
#' 
#' @inherit isZeroCodedCheckField sections
#' 
#' @return
#' `exportRecordsTyped` returns a data frame with the formatted data.
#' 
#' `exportReportsTyped` returns a data frame with the formatted data.
#' 
#' @seealso 
#' ## Other records exporting functions
#' 
#' [exportRecords()], \cr
#' [exportReports()], \cr
#' [exportBulkRecords()]
#' 
#' ## Field validations and casting
#' 
#' [fieldValidationAndCasting()], \cr
#' [reviewInvalidRecords()]
#' 
#' ## Post-processing functionality
#' 
#' [recastRecords()], \cr
#' [guessCast()], \cr
#' [guessDate()], \cr
#' [castForImport()], \cr
#' [mChoiceCast()], \cr
#' [splitForms()], \cr
#' [widerRepeated()]
#' 
#' ## Vignettes
#' 
#' `vignette("redcapAPI-offline-connection")`\cr
#' `vignette("redcapAPI-casting-data")`\cr
#' `vignette("redcapAPI-missing-data-detection")`\cr
#' `vignette("redcapAPI-data-validation)`\cr
#' `vignette("redcapAPI-faq)`
#' 
#' @examples
#' \dontrun{
#' unlockREDCap(connections = c(rcon = "project_alias"), 
#'              url = "your_redcap_url", 
#'              keyring = "API_KEYs", 
#'              envir = globalenv())
#'              
#' # Export records with default settings
#' exportRecordsTyped(rcon)
#' 
#' # Export records with no factors
#' exportRecordsTyped(rcon, 
#'                    cast = default_cast_character)
#'                    
#' # Export records for specific records
#' exportRecordsTyped(rcon, 
#'                    records = 1:3)
#'                    
#' # Export records for specific instruments
#' exportRecordsTyped(rcon, 
#'                    forms = c("registration", "visit_1", "medications"))
#'                    
#' # Export records using filterLogic, an API parameter not provided
#' # in the exportRecordsTyped function signature
#' exportRecordsTyped(
#'   rcon, 
#'   records = 1:3, 
#'   api_param = list(filterLogic = "[age_at_enrollment] > 25")
#' )
#'                    
#'                    
#'                    
#' # Export a report 
#' exportReports(rcon, 
#'               report_id = 12345)
#'               
#'               
#' # Export records using files downloaded from the user interface
#' rcon_off <- 
#'   offlineConnection(
#'     meta_data = 
#'       system.file(file.path("extdata/offlineConnectionFiles", 
#'                             "TestRedcapAPI_DataDictionary.csv"), 
#'                   package = "redcapAPI"), 
#'     records = 
#'       system.file(file.path("extdata/offlineConnectionFiles",
#'                             "TestRedcapAPI_Records.csv"), 
#'                   package = "redcapAPI"))
#'
#' exportRecordsTyped(rcon_off)
#' }
#' 
#' @usage NULL
#' @order 0

recordsTypedMethods <- function(rcon, 
                                report_id, 
                                fields, 
                                drop_fields, 
                                forms, 
                                records, 
                                events, 
                                survey, 
                                dag, 
                                date_begin, 
                                date_end, 
                                na, 
                                validation, 
                                cast, 
                                assignment, 
                                filter_empty_rows, 
                                csv_delimiter, 
                                batch_size,
                                warn_zero_coded,
                                ...)
{
  NULL
}

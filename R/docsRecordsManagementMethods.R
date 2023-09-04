#' @name recordsManagementMethods
#' @title Export Next Record Name or Rename a Record
#' 
#' @description These methods enable the user to get the next record name
#'   (when auto numbering is enabled) or rename and existing record.
#'   
#' @inheritParams common-rcon-arg
#' @inheritParams common-dot-args
#' @inheritParams common-api-args
#' @param record_name `character(1)` or `integerish(1)`. 
#'   The name of an existing record in the project. 
#' @param new_record_name `character(1)` or `integerish(1)`. 
#'   The new name to give to the record. 
#' @param arm `character(1)` or `NULL`, an optional arm number. 
#'   If `NULL`, then all records with same name across all arms on 
#'   which it exists (if longitudinal with multiple arms) will be 
#'   renamed to new record name, otherwise it will rename the record 
#'   only in the specified arm.
#'   
#' @return
#' `exportNextRecordName` returns an integerish value. The value is
#'   determined by looking up the highest record ID number in the 
#'   project and incrementing it by 1. 
#'
#' `renameRecord` return a logical value that indicates if the 
#'   operation was successful.
#'   
#'   
#'   
#' @seealso 
#' [exportRecords()], \cr
#' [exportReports()], \cr
#' [importRecords()], \cr
#' [deleteRecords()], \cr
#' [exportRecordsTyped()], \cr
#' [exportReportsTyped()]
#' 
#' @examples
#' \dontrun{
#' unlockREDCap(connections = c(rcon = "project_alias"), 
#'              url = "your_redcap_url", 
#'              keyring = "API_KEYs", 
#'              envir = globalenv())
#'              
#' # Get the next record name
#' exportNextRecordName(rcon)
#' 
#' # Rename an existing record
#' renameRecord(rcon, 
#'              record_name = "1", 
#'              new_record_name = "42")
#' }
#' 
#' @usage NULL
#' @order 0

recordsManagementMethods <- function(rcon, 
                                     record_name, 
                                     new_record_name, 
                                     arm, 
                                     ..., 
                                     error_handling, 
                                     config, 
                                     api_param){
  NULL
}

#' @name recordsManagementMethods
#' @title Export Next Record Name or Rename a Record
#' 
#' @description These methods enable the user to get the next record name
#'   (when auto numbering is enabled) or rename and existing record.
#'   
#' @inheritParams common-rcon-arg
#' @inheritParams common-dot-args
#' @inheritParams common-api-args
#' @param record_name `character` or `integerish`. 
#'   The name of an existing record in the project. 
#' @param new_record_name `character` or `integerish`. 
#'   The new name to give to the record. Must have the same length as
#'   `record_name`.
#' @param arm `character` or `NULL`, an optional arm number. 
#'   If `NULL`, then all records with same name across all arms on 
#'   which it exists (if longitudinal with multiple arms) will be 
#'   renamed to new record name, otherwise it will rename the record 
#'   only in the specified arm. When not `NULL`, it must have the same 
#'   length as `record_name`.
#'   
#' @return
#' `exportNextRecordName` returns an integerish value. The value is
#'   determined by looking up the highest record ID number in the 
#'   project and incrementing it by 1. 
#'
#' `renameRecord` invisibly returns a logical vector that indicates if the 
#'   operation was successful. Otherwise, an error is thrown.
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
                                     ...)
{
  NULL
}

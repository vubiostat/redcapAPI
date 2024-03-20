#' @name importFileToRecord
#' @title Import a File With Option to Create A Record to Receive the File
#' 
#' @description There are times when the user may desire to create a record
#'   and import a file as part of a single action. For example, a study 
#'   consent form may have been collected and needs to be stored with the 
#'   data of the new study participant. `importFileToRecord` extends
#'   `importFiles` to allow the concurrent creation of the record in which 
#'   the file will be stored.
#'   
#' @inheritParams fileMethods
#' @param record `character(1)` or `integerish(1)` or `NULL`. The record ID in 
#'   which the desired file is stored. When `NULL`, an attempt will be made to
#'   create a new record for the file.  See 'Details'
#'   
#' @details
#' The behavior of `importFileToRecord` depends on 
#' 
#' 1. whether record auto numbering has been enabled in the project, 
#' 2. if the record is specified by the user
#' 3. if the record specified by the user exists.
#' 
#' The following table details the actions taken based on these conditions. 
#' (`force_auto_number` is an argument to [importRecords()]).
#' 
#' | Autonumbering enabled | `record` | Record Exists | Action | 
#' |-----------------------|----------|---------------|--------|
#' | Yes                   | `NULL`   | No            | Create a new record (using `force_auto_number = TRUE`) and import the file to the new record |
#' | Yes                   | Specified | Yes          | Import the file to the existing record |
#' | Yes                   | Specified | No           | Create a new record (using `force_auto_number = TRUE`)and import the file to the new record |
#' | No                    | `NULL`    | No           | Error: record must be provided when auto numbering is not enabled | 
#' | No                    | Specified | Yes          | Import the file to the existing record |
#' | No                    | Specified | No           | Create the record (using `force_auto_number = FALSE`) and import the file to the new record. |
#'   
#' @seealso 
#' [importFiles()],\cr
#' [importRecords()]
#' 
#' @examples
#' \dontrun{
#' unlockREDCap(connections = c(rcon = "project_alias"), 
#'              url = "your_redcap_url", 
#'              keyring = "API_KEYs", 
#'              envir = globalenv())
#' 
#' importFileToRecord(rcon,
#'                    file = "file_to_upload.txt" 
#'                    record = NULL, 
#'                    field = "file_upload_test")
#' }
#'   
#' @export

importFileToRecord <- function(rcon, 
                               file, 
                               record = NULL,
                               field, 
                               event, 
                               overwrite = TRUE, 
                               repeat_instance = NULL, 
                               ...){
  if (is.numeric(record)) record <- as.character(record)
  
  ##################################################################
  # Argument Validation
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_class(x = rcon, 
                          classes = "redcapApiConnection", 
                          add = coll)
  
  checkmate::assert_character(x = file, 
                              len = 1, 
                              any.missing = FALSE,
                              add = coll)
  
  checkmate::assert_character(x = record, 
                              len = 1, 
                              any.missing = FALSE, 
                              null.ok = TRUE,
                              add = coll)
  
  checkmate::assert_character(x = field, 
                              len = 1, 
                              any.missing = FALSE, 
                              add = coll)
  
  checkmate::assert_character(x = event, 
                              len = 1, 
                              any.missing = FALSE, 
                              null.ok = TRUE,
                              add = coll)
  
  checkmate::assert_logical(x = overwrite, 
                            len = 1, 
                            any.missing = FALSE, 
                            add = coll)
  
  checkmate::assert_integerish(x = repeat_instance,
                               len = 1,
                               any.missing = FALSE,
                               null.ok = TRUE,
                               add = coll)
  
  checkmate::reportAssertions(coll)
  
  ###################################################################
  # is autonumbering enabled, and does the record exist          ####
  is_autonumber_enabled <- as.logical(rcon$projectInformation()$record_autonumbering_enabled)
  
  if (!is_autonumber_enabled && is.null(record)){
    coll$push("`record` must be provided when autonumbering is not enabled.")
  }
  
  checkmate::reportAssertions(coll)
  
  record_exists <- 
    if (!is.null(record)){
      suppressMessages({
        RecordData <- exportRecordsTyped(rcon, 
                                         records = record, 
                                         fields  = rcon$metadata()$field_name[1])
      })
      nrow(RecordData) > 0
    } else {
      FALSE
    }

  ###################################################################
  # Create the new record, if necessary                          ####
  # In the case enumeration, the only condition under which the 
  # record doesn't exist and we don't create the record is when 
  # an error is returned. Since that is cleared in the validation, 
  # it is safe to create the record if the record does not exist.
  
  if (!record_exists){
    NewData <- data.frame(record_id = if (is.null(record)) "1" else record)
    names(NewData) <- rcon$metadata()$field_name[1]
    
    record <- importRecords(rcon, 
                            data = NewData, 
                            force_auto_number = is_autonumber_enabled, 
                            returnContent = "ids")[[1]]
  }
  
  ###################################################################
  # Import the file                                              ####
  importFiles(rcon = rcon, 
              file = file, 
              record = record, 
              field = field, 
              event = event, 
              overwrite = overwrite, 
              repeat_instance = repeat_instance, 
              ...)
}

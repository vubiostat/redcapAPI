#' @name fileMethods
#' @title Export, Import, or Delete Files to a Field in a REDCap Project
#' 
#' @description These methods enable to the user to export a file stored
#'   in a project field, import a file, or delete an existing file. 
#'   
#' @inheritParams common-rcon-arg
#' @inheritParams common-dot-args
#' @inheritParams common-api-args
#' @param record `character(1)` or `integerish(1)`. The record ID in 
#'   which the desired file is stored. 
#' @param field `character(1)`. The field name in which the file is stored. 
#' @param event `character(1)` or `NULL`. The event name for the file.  
#'   This applies only to longitudinal projects.  If the event is not
#'   supplied for a longitudinal project, the API will return an error message
#' @param repeat_instance `integerish(1)` or `NULL`. The repeat instance number of 
#'   the repeating event or the repeating instrument. When available in your 
#'   instance of REDCap, and passed as `NULL`, the API will assume a value of 1.
#' @param file `character(1)`. The file path to the file to be imported.
#' @param overwrite `logical(1)`.  When `FALSE`, the function checks 
#'   if a file already exists for that record.  If a file exists, the function 
#'   terminates to prevent overwriting.  When `TRUE`, no additional 
#'   check is performed.
#' @param dir `character(1)`. A directory/folder to which the file 
#'   will be saved. By default, the working directory is used.
#' @param file_prefix `logical(1)`.  Determines if a prefix is appended 
#'   to the file name.  The prefix takes the form `[record_id]-[event_name]-[file_name]`.  
#'   The file name is always the same name of the file as it exists in REDCap.
#'   
#' @details These functions only export, import, or delete a single file.
#' 
#' When exporting, the file name cannot be changed; whatever name exists in 
#' REDCap is the name that will be used. The record ID and event name 
#' may be appended as a prefix.
#' 
#' @seealso
#' [exportFilesMultiple()], \cr
#' [importFileToRecord()] (can create a record to receive the file if it does yet exist)
#' 
#' @return 
#' `exportFiles` invisibly returns the file path to which the exported 
#'   file was saved.
#'   
#' `importFiles` invisibly returns `TRUE` when successful, or throws an 
#'   error if the import failed.
#'
#' `deleteFiles` invisible returns `TRUE` when successful, or throws an
#'   error if the deletion failed.
#'   
#' @examples
#' \dontrun{
#' unlockREDCap(connections = c(rcon = "project_alias"), 
#'              url = "your_redcap_url", 
#'              keyring = "API_KEYs", 
#'              envir = globalenv())
#'
#' save_to_dir <- tempdir()              
#' 
#' # Export a file
#' exportFiles(rcon, 
#'             record = 1, 
#'             field = "file_upload_test", 
#'             dir = save_to_dir)
#'             
#' # Export a file for a specific event
#' exportFiles(rcon, 
#'             record = 1, 
#'             field = "file_upload_test",
#'             event = "event_1_arm_1", 
#'             dir = save_to_dir)
#' 
#' # Import a file 
#' importFiles(rcon,
#'             file = "file_to_upload.txt" 
#'             record = 1, 
#'             field = "file_upload_test")
#'            
#' # Delete a file
#' deleteFiles(rcon, 
#'             record = 1, 
#'             field = "file_upload_test")
#' }
#'
#' @usage NULL
#' @order 0

fileMethods <- function(rcon, 
                        record, 
                        field, 
                        event, 
                        repeat_instance, 
                        file, 
                        overwrite, 
                        dir, 
                        file_prefix, 
                        ...)
{
  NULL
}

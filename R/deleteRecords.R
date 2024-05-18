#' @name deleteRecords
#' @title Delete Records from a Project
#' 
#' @description These methods enable the user to delete records from a project.
#' 
#' @inheritParams common-rcon-arg
#' @inheritParams common-dot-args
#' @inheritParams common-api-args
#' @param records `character` or `integerish`. Record ID's to be 
#'   returned.
#' @param arm `integerish`. the arm number of the arm in which the 
#'   record(s) should be deleted. This can only be used if the project is 
#'   longitudinal with more than one arm. If the arm parameter is not 
#'   provided, the specified records will be deleted from all arms in which 
#'   they exist. Whereas, if `arm` is provided, they will only be deleted from 
#'   the specified arm.
#' @param instrument `character(1)` Optional instrument to delete records from.
#' @param event `character(1)` Optional event to delete records from.
#' @param repeat_instance `numeric(1)` optional repeat instance to delete records from.
#' @param delete_logging `logical`. Should the logging for this record be
#'   delete as well. Default to FALSE. 
#' 
#' @return
#' `deleteRecords` invisibly returns a character value giving the number of records deleted.
#' 
#' @seealso 
#' [exportRecords()], \cr
#' [importRecords()], \cr
#' [exportRecordsTyped()]
#' 
#' @examples
#' \dontrun{
#' unlockREDCap(connections = c(rcon = "project_alias"), 
#'              url = "your_redcap_url", 
#'              keyring = "API_KEYs", 
#'              envir = globalenv())
#'              
#' # Delete records
#' deleteRecords(rcon, 
#'               records = c("1", "2"))
#' 
#' }
#' 
#' @export

deleteRecords <- function(rcon, 
                          records,
                          arm             = NULL,
                          instrument      = NULL,
                          event           = NULL,
                          repeat_instance = NULL,
                          delete_logging  = FALSE,
                          ...)
{
  UseMethod("deleteRecords")
}

#' @rdname deleteRecords
#' @export
deleteRecords.redcapApiConnection <- function(
  rcon, 
  records, 
  arm             = NULL,
  instrument      = NULL,
  event           = NULL,
  repeat_instance = NULL,
  delete_logging  = FALSE,
  ...)
{
  if (is.numeric(records)) records <- as.character(records)
  if (is.character(arm)) arm <- as.numeric(arm)
  
   ##################################################################
  # Argument Validation
  
  coll <- checkmate::makeAssertCollection()
  
  checkmate::assert_character(x = records,
                              any.missing = FALSE,
                              min.len = 1,
                              add = coll)
  
  checkmate::assert_character(x = instrument,
                              any.missing = FALSE,
                              len = 1,
                              null.ok = TRUE,
                              add = coll)
  
  checkmate::assert_character(x = event,
                              any.missing = FALSE,
                              len = 1,
                              null.ok = TRUE,
                              add = coll)
  
  checkmate::assert_integerish(x = repeat_instance,
                               len = 1,
                               any.missing = FALSE,
                               null.ok = TRUE,
                               add = coll)
  
  checkmate::assert_logical(x=delete_logging,
                            any.missing = FALSE,
                            len = 1,
                            add = coll)
    
  checkmate::assert_integerish(arm,
                               len = 1, 
                               any.missing = FALSE,
                               null.ok = TRUE,
                               add = coll)

  checkmate::reportAssertions(coll)
  
  Arms <- rcon$arms()
  
  checkmate::assert_subset(x = arm,
                           choices = Arms$arm_num, 
                           add = coll)
  
  checkmate::reportAssertions(coll)
  
   ##################################################################
  # Build the Body List
  
  body <- list(content = "record",
               action = "delete", 
               arm = arm, 
               instrument = instrument,
               event = event,
               repeat_instance = repeat_instance,
               delete_logging = delete_logging)
  
  body <- c(body,
            vectorToApiBodyList(vector = records,
                                parameter_name = "records"))

   ##################################################################
  # Call the API
  invisible(as.character(makeApiCall(rcon, body, ...)))
}
